# Preparation ownership and capture storage

Status, 2026-10-15: one eager preparation implementation and one shared chunked
capture store are maintained. Macro-only deferral investigations have ended.
Parity is paused. The next proposed ownership change is the
[structural scan and relocatable-object design](native-fragment-preparation.md),
which awaits review before implementation.

## Experiment decisions

| Experiment | Maintained decision |
|---|---|
| Chunked capture | Promoted to the default; fresh fixed-input and current-source full native self-host proofs pass. |
| First-use macro-body binding/plans | Removed with switches and experiment-specific tests; no established runtime gain. |
| First-use call candidates | Removed with switches and experiment-specific tests. |
| First-use whole-line plans | Removed with switches and experiment-specific tests. |
| Binary token/spelling interface prototype | Removed; no justified prerequisite for a currently authorized implementation. |
| Fragment envelope | Removed with its selector/probes/tests; the future object design remains a proposal. |
| In-build raw include text reuse | Dropped after identical-input qualification: runtime-neutral, extra peak heap, unchanged preparation work. |

The standard packed macro execution, declaration context and shared VM/package
boundaries remain. These decisions remove alternate preparation paths, not language
features. No opt-in switch stack remains. Old implementations are recoverable from
Git; baseline executables and frozen measurements are diagnostic artifacts outside
the tree. The comparison scripts no longer expose removed experiment modes.

## Shared chunked capture

The append-only store uses 32 KiB chunks; records larger than 4 KiB get dedicated
allocations rounded to eight bytes. A dedicated allocation ends the current shared
chunk, keeping directory intervals ordered and nonoverlapping. A logical source
region can span chunks, and a chunk can contain several regions. Record bytes never
move when the small descriptor directory grows.

Logical offsets and source-region ownership remain stable. Each record is contiguous;
sequential resolution caches a directory row and arbitrary resolution uses binary
search. There is no per-token indirection or original-text replay. The temporary
record is still copied once into retained storage; construction itself is not lazy.
Long copies plus a byte tail implement append. CPU semantics remain package-owned.

On identical frozen eager input, capture payload stays 23,836,984 bytes, capacity
falls from 33,337,344 to 23,953,408 bytes, and growth copies fall from 33,064,076 to
zero. Tracked peak heap falls 21.4%, to 34,324,616 bytes. See the
[qualified resource comparisons](native-performance-diagnosis.md#chunked-capture).
The earlier 1.4% release difference is below timing noise, not a speed result.

## Capture storage and lifetimes

Chunking removes growth overhead, not the acquisition floor: all captured modules
are still accumulated before replay. Records, plans, macro templates, configuration
exports and binding state can reference the retained source representation. Nothing
may be freed merely because its emitting block is unreachable or a macro was not
invoked. Complete referenced blocks preserve internal-label fallthrough.

On the earlier lifetime-audit input, 30,216,808 tracked bytes were already resident
before a replay span completed. Capture accounted for about 71% of that input's
heap peak. These are historical observations, not the current heap measurement.
Early retirement after replay cannot remove a peak already reached during acquisition.
The current frozen input retains about 23.84 MB of capture even after growth waste
is removed. No qualified largest-module or active-closure bound exists yet.

Whole-application `OPFORGE_LAZY_AUDIT` and `OPFORGE_CAPTURE_LIFETIME_AUDIT` builds
are rejected with chunked storage: their contiguous-region assumptions are invalid.
Their unsupported whole-app image builders were removed. Compatible component
probes, layouts and strict historical decoders remain; old decoded reports do not
qualify a fresh chunked whole-app run. Existing MEMD/capture/hotspot observers supply
current comparisons. The capture telemetry field `retained_region_count` now counts
physical store allocations, not logical source regions; do not infer module size
or early-release eligibility from it.

## Next boundary

Replace capture-all/replay-all only after review of the structural design. An owned,
relocatable near-output object could release source-oriented storage at publication;
a resident object and a disk-backed object must share the same pointer-free format.
This requires an explicit configuration, include-continuation and publication
contract, not another macro observer.

The separately authorized in-build include experiment is complete. Sharing raw
bytes produced 40.7% hits but avoided no preparation work, added peak heap and
changed release time by only +0.02%; it and its tests were removed. The original
path remains the default. See the [separate runtime/memory results](native-performance-diagnosis.md#in-build-include-text-reuse),
including the collection-counter scope caveat.

The next proposed feasibility slice is one persistent leaf-module entry that can
avoid preparation. It remains unimplemented. Preserve each include's ordered
semantic effects and remap numeric identities; measure dependency invalidation,
actual avoided work and cold/warm hit rates before a complete `.ofm` cache. A new
framework or further macro-only audit is not authorized.

The current heap remains roughly 32.8 MiB before executable, stack and OS costs;
there is no 2 MiB fit claim. Structural scanning and object publication may lower
the bound but have not been implemented or measured.
