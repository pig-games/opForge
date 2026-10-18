# Preparation ownership and capture storage

Status, 2026-10-18: sequential lexical sealing and bounded cache buffer reuse are
implemented. The qualified-source register-binding repair passes focused native
checks, and the current release completes its full disk-backed self-host exactly.
On frozen input, buffer reuse reduces allocation traffic 88.41% while preserving
6.59 MiB peak heap. Runtime remains substantially above the pre-sealing reference.
An extra instrumented current-source self-host times out during global completion;
its peak is unqualified. Review that anomaly and the remaining tradeoff before
further ownership implementation. Parity remains paused.
See [measurements and proof limits](native-performance-diagnosis.md) and
[the remaining object boundary](native-fragment-preparation.md).

## Maintained storage policies

| Mechanism | Current decision |
|---|---|
| Resident capture | Default; dependency-first publication can retire completed raw captures before acquiring later modules. |
| Complete-graph replay | Retained for disk inputs, explicit closed manifests and resident graphs closing before first publication. Same semantic frontend. |
| `--capture-dir DIR` | One disk policy for raw capture, prepared records and the candidate lexical backing; no additional experiment switch. |
| Chunked raw capture | Sole capture representation; 32 KiB shared chunks and dedicated large records. |
| Prepared records | Disk policy caps payload cache at 128 KiB through finalization/reachability; reconstructs ordered contiguous assembly input after frontend retirement. |
| Lexical entries/names | Resident policy keeps contiguous entries and paged names. Disk candidate seals completed active ranges behind stable IDs and a 128 KiB cache. Both use owned copies. |
| Module metadata | Owners, origins and visibility arrays remain resident; private dependency closure is not assumed. |
| Macro-only deferral/audits, first-use plans/candidates, binary token prototype, fragment envelope | Removed; no demonstrated whole-build benefit justified retention. |
| In-build include-text reuse, persistent prepared leaf cache | Removed; inadequate savings and incomplete reuse equivalence. |

Normal packed macros, existing binding context and shared VM/package boundaries
remain. Git is the archive; no macro/cache selector stack remains.

## Capture acquisition and retirement

Shared writers use 32 KiB chunks. Records larger than 4 KiB receive dedicated
allocations rounded to eight bytes. Dedicated records and physical file boundaries
seal writers. Directory rows may move; logical offsets never do. Each record stays
contiguous. Sequential resolution caches a row; other resolution uses binary search.

Disk mode spools immutable chunks to a process-private DOS file. Its 1 MiB payload
budget includes writers and counted reader leases; directory, temporary records,
executable, stack and OS are outside it. Eviction skips pins and checked reloads may
move payload. I/O failure, oversized records or pin exhaustion fail explicitly,
without an unbounded RAM fallback. Use an existing real disk directory, for example
`--capture-dir Development:opforge-scratch`; `T:` may be RAM.

Retirement validates disjoint ranges and pinned rows before mutation. Fully retired
chunks release payload and become checked tombstones; partially shared chunks keep
live bytes. Views cannot cross retired boundaries or reload fully retired payload.
Unselected/shared gaps survive final capture cleanup. Scratch-file length need not
shrink when RAM ownership retires. Normal cleanup removes only owned paths; crashes
can leave process-private files for manual recovery.

## Publication, configuration and includes

The scheduler preserves dependency and source order. Configuration and semantic
frontends own independent state. Session switching restores shared VM control at a
stable frontend address; no borrowed line crosses the switch. Immutable parameter
snapshots seed by spelling. Repeated imports require equal complete key/value sets;
consumed section maps retain their state.

A successful replay owns retained records, values and templates before graph
acknowledgement and capture retirement. Includes suspend their parent cursor and
preserve origin/expansion context. In resident interleaved replay, included captures
retire after replay and before parent continuation. Complete-graph paths keep them
until capture cleanup. Inactive branches and unexpanded templates do not acquire
files. Definition-relative paths remain available for later active includes.

After configuration closes, final snapshots seed before configuration scratch,
graph and physical discovery retire. The remaining schedule transfers to semantic
ownership. Disk policy retains complete-graph scheduling because overlapping two
growing sessions previously increased peak. All paths keep normal failure cleanup.

## Sequential lexical storage sealing

Successful semantic module publication seals only the newly appended lexical range.
Public/interface facets remain retained; private entries and canonical names move
to checked backing offsets. An eight-byte row per global identity resolves entry
and canonical spelling locations. Active arenas are physically freed after successful
publication. Serialized data contains IDs and offsets, never memory pointers.

Closing a module does not close all its dependencies. Later imports, templates,
member identities and binding can still read or change private metadata. Readers
therefore receive caller-owned entry/name copies; mutations explicitly write back.
No page pointer escapes its lease. Stable global IDs preserve the existing late
binding and layout order. Canonical-name sharing survives aliasing and retirement.
The backing cache has four 32 KiB pages. At capacity, a successful dirty flush
precedes transfer of an unpinned buffer's Pointer/Capacity; both logical Used
extents remain with their rows. Reuse avoids allocation/clearing without growing
the cache. Failed flush preserves ownership; failed reload frees its destination
and reduces Reserved. Appends and bounded views expose initialized bytes only.

Storage errors are sticky and checked before successful frontend publication or
cleanup, so boolean lookup fallbacks cannot turn I/O failure into ordinary absence.
Names retire at the existing post-binding numeric frontier. Entries remain readable
until their later numeric consumers finish. Partial sealing retains normal owned
cleanup. The resident control uses the same APIs without module spooling.

This is **storage sealing**, not semantic finalization, module-local ID remapping
or persistent `.ofm` serialization. Module owner/origin/flag arrays, typed values,
schemas, templates, import state and plans still have build-wide lifetimes.
Buffer reuse removes 730,562,560 B of allocation/free traffic on the frozen input;
heap remains 6,909,720 B. Release self-host completion does not supply a peak for
the stalled instrumented current-source case. The [measurement report](native-performance-diagnosis.md)
separates those configurations and retains the contiguous reference.

## Prepared records and the remaining frontier

Prepared-record module views cap reads at module boundaries. Dirty pages write
before eviction; leases protect borrowed views. Layout, output, binding and remapping
traverse whole-record batches in their original order. Late section-map recipes
preserve geometry until the full map set is known. Reachability records whole-block
decisions independently, preserving internal label fallthrough.

Runtime bindings, typed values, configuration and member IDs transfer before frontend
retirement. Only then are ordered records reconstructed and omitted blocks marked.
Prepared backing releases before assembly; `beginSizing` retains its no-growth
contract. Resident identity layouts can reuse validated complete streams; reordered
or partial resident layouts copy.

Preparation still processes applicable module bodies and uses full configuration
semantics, not a cheap structural-only scan. A real `.ofm` must own local identities,
exports, templates and unresolved relocation/layout/observation recipes before
source-oriented owners can retire. A final-only cache cannot reduce an earlier peak.

No 2 MiB fit is claimed. The frozen candidate's tracked heap alone is 6.59 MiB;
prepared and assembly live heap remain 2.83 and 4.71 MiB, excluding executable,
stack and OS. Resource results and current qualification are reported separately.
