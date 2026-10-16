# Preparation ownership and capture storage

Status, 2026-10-16: one eager preparation implementation and one shared chunked
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
| In-build raw include text reuse | Dropped: no runtime gain, extra heap, unchanged preparation work. |
| Persistent prepared declaration leaves | Dropped: tested local-ID/schema reuse avoids 1.73% of compilations, but has no peak saving or demonstrated speed gain and lacks preparation-observation consistency qualification. |

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

The raw include and persistent prepared declaration-leaf experiments are complete
and dropped. See the [separate runtime/memory results](native-performance-diagnosis.md#cache-experiment-decisions).
The prepared prototype reused five entries / 698 rows with exact output on the
frozen full workload, but retained every source capture and left peak heap at
34,517,176 bytes. Serialization after global binding cannot lower an acquisition
peak already reached. Its passing cases also do not resolve the difference
between early preparation snapshots and final folded values.

The next decision is the structural publication contract, not another cache flag.
An object must retain unresolved binding/layout recipes and ordered preparation
observations before its source-oriented storage can be freed. Review the linked
design and its disk-spill alternative before changing that lifetime boundary.
No general `.ofm` implementation or early-release memory bound has been qualified.

The production reference is restored byte-for-byte in native/Rust source to
`bcac8dc8`; experimental code and tests are preserved in Git, not the active tree.
The measured full-profile heap remains 32.92 MiB before executable, stack and OS
costs, about 16.5 times the entire 2 MiB target. No low-memory fit is claimed.
