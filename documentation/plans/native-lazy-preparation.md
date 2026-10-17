# Preparation ownership and capture storage

Status, 2026-10-17: one eager preparation implementation uses shared chunked
capture. Resident storage remains the default; `--capture-dir DIR` selects bounded
disk backing. Validated identity layouts now avoid a second prepared-record
buffer. The isolated change lowers full-build tracked peak by 2.07%. The initial 2.40%
release slowdown did not reproduce in controlled follow-up; no whole-build
runtime change is established. See the
[separate results](native-performance-diagnosis.md#reuse-an-already-ordered-record-stream).
Parity remains paused. Structural scanning, publication and `.ofm` remain
[designs awaiting review](native-fragment-preparation.md).

## Experiment decisions

| Experiment | Maintained decision |
|---|---|
| Identity record layout reuse | Automatic after complete validation; real reorders/partial selections keep copying. Full self-build passes; materialization saves about 76 ms, with no established whole-build runtime change. |
| Bounded disk backing | Retained as one explicit directory/storage policy; full current-source native self-host and identical-input comparisons pass. |
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
features. No macro/cache experiment switch stack remains; disk backing is a storage
policy, not another language-preparation path. Old implementations are recoverable
from Git; baseline executables and frozen measurements are diagnostic artifacts outside
the tree. The comparison scripts no longer expose removed experiment modes.

## Shared chunked capture

The append-only store uses 32 KiB chunks; records larger than 4 KiB get dedicated
allocations rounded to eight bytes. A dedicated allocation ends the current shared
chunk, keeping directory intervals ordered and nonoverlapping. A logical source
region can span chunks, and a chunk can contain several regions. Payload bytes
remain stable while pinned, including when the descriptor directory grows.
Resident chunks can be evicted and faulted back at different physical addresses;
logical offsets stay unchanged.

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

Chunking removes growth waste. Disk backing removes accumulated payload residency:
immutable chunks are spooled by logical offset into one private DOS scratch file.
A 1 MiB payload budget includes the active writer and every counted reader pin;
directory metadata, temporary records and OS buffers remain outside it. Configuration,
replay and nested source lookahead acquire/release explicit leases. Templates and
prepared values must own their data before a lease ends. Descriptors can move on
append and must never escape into retained state.

The current policy seals a chunk before exposing it to a reader. It evicts unpinned
chunks round-robin and reloads checked exact extents. A whole record must fit the
budget; exhaustion or IO failure produces an explicit diagnostic with normal owned
cleanup. The resident path performs no file IO. Both still capture and prepare all
applicable source; neither skips unreachable language work nor changes whole-block
fallthrough, declaration context, macros or module ordering.

On the identical 214-file workload, 116,855 records / 23,852,392 logical bytes are
unchanged. Resident capture peaks at 23,986,176 B; disk-backed payload peaks at
1,048,576 B. At the storage checkpoint, whole tracked heap falls from 34,517,176
to 11,839,624 B. Allocation churn increases through reloads. Below-budget controls may gain nothing or retain
another partial chunk; this is why resident storage stays the default. See the
[separate runtime/memory results](native-performance-diagnosis.md#bounded-disk-backed-capture).

Use an existing writable disk directory, for example
`--capture-dir Development:opforge-scratch`. Do not assume `T:` uses disk. This is
a process-private raw capture spool, not a persistent cache or published object.
No automatic fallback to unbounded RAM is allowed. Normal cleanup removes only
owned paths; process crashes may leave scratch directories for manual recovery.

Existing capture snapshots count resident allocations/used/capacity, while logical
composition counters cover all records. Evicted rows retain logical length without
owning payload. The old whole-source lazy/lifetime observers remain unsupported
because they require a contiguous arena per region; no new observer framework was
added. Current MEMD/capture events supply the comparisons.

## Prepared-record ownership

Final ordering now retains the packed buffer when validated module spans already
cover the whole stream contiguously in order. It still builds module starts and
physical origins through the existing metadata loop. Reorders and partial
selections keep the copying path. No input-text use or semantic ownership changes.

On identical disk-backed input this avoids 2,032,832 copy bytes and a 2 MiB
allocation, but another overlap sets the remaining 11,594,504 B peak. The actual
saving is 245,120 B / 2.07%; prepared/assembly live heap is unchanged. The initial
release slowdown did not reproduce. Matched phase controls save about 76 ms in
ordering, with effectively unchanged postorder work; no whole-build speed change
is established. Both smaller controls avoid copies without lowering their peaks.
Full current-source disk-backed self-host passes.

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

The next architectural decision is the structural publication contract, not another
cache flag.
An object must retain unresolved binding/layout recipes and ordered preparation
observations before its source-oriented storage can be freed. Review the linked
design and its measured raw-spill reference before changing that lifetime boundary.
No general `.ofm` implementation or early-release memory bound has been qualified.

The pre-storage reference (`1171797a`, native/Rust code matching `bcac8dc8`) remains
recoverable and its executable is preserved for comparison. Disk backing changes
storage ownership, not eager preparation semantics. After identity reuse the paired
peak is 11.06 MiB of tracked heap before executable, stack and OS costs, about 5.5 times
the entire 2 MiB target. No low-memory fit is claimed.
