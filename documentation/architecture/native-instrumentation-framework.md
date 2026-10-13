# Native Instrumentation Framework

The native instrumentation framework isolates debug behavior in centralized
68000 routines. Ad-hoc probes are forbidden.

## Safety contract

Enabled macros emit a fixed-size call-site stub. Disabled macros emit zero
bytes. Predicate logic and event writes occur only in module routines.

The assertion and structured-event routines:

- preserve D0-D7 and A0-A6
- preserve CCR exactly and do not touch supervisor state
- return with zero stack delta
- write only the dedicated bounded event buffer
- leave the event buffer unchanged when it is full
- avoid request, service, last-error, and production output buffers

The bounded assembly-progress bridge follows the same passive preservation
contract. Its record-pointer getter returns A0, and its explicitly queried
diagnostic-abort routine returns status in D0/CCR. Those are documented ABI
outputs rather than hidden instrumentation clobbers.

Use the [instrumentation guide](../../agents/rules/native-68000-safe-instrumentation.md)
for call-site placement and verification. It requires safety evidence, not a
separate safety-note form. The ABI and build-mode descriptions here do not grant
permission to add new macros or relax preservation requirements.

## Reusable telemetry calls

`native/motorola68000/amigaos/debug/telemetry_macros.i` owns the conditional
runtime-observer import and the `OPFORGE_DEBUG_CONTRACTS` plus
`OPFORGE_PROGRESS_RUNTIME_COUNTERS` gates. Call sites include it and invoke:

```asm
    .include "telemetry_macros.i"
    ; At the appropriate VM boundary:
    .TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_EXPRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR
    .TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_EXPRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR
    .TELEMETRY_VM_LEAVE
```

Service enter/leave and candidate macros wrap the same existing bounded observer.
IDs are existing compile-time constants; new measurements must use meaningful
shared operation identities and the approved bounded record contract. The macros
preserve CCR before argument setup, preserve setup registers and rely on the
observer routines' documented full preservation. They introduce no runtime toggle,
I/O or new counter storage. Missing either build gate emits no macro bytes or
observer import; release linking continues to omit the observer module/storage.
The host macro test compares disabled output byte-for-byte with removed call sites.
Full native comparisons qualify the actual enabled expression call sites separately.

Use these macros in new/adapted runtime code. Other legacy counter families retain
their existing implementation until adapted; this is a reusable entry point into
the current framework, not a mandate for a repository-wide instrumentation rewrite.

## Bounded memory accounting

`debug/memory_telemetry.i` supplies `MEMORY_ALLOC`, `MEMORY_FREE`, `MEMORY_PHASE`,
`MEMORY_LAYOUT`, `MEMORY_WORK`, `MEMORY_CLOCK`, `MEMORY_STAGE`, `MEMORY_FAILURE` and terminal `MEMORY_SAVE`. Both `OPFORGE_DEBUG_CONTRACTS` and
`OPFORGE_MEMORY_TELEMETRY` are required; missing either gate emits no calls, imports
or storage. These macros and the dedicated `debug.amigaos.memory_profile` owner
preserve registers/CCR, never use request/output/error buffers, and keep a bounded
2280-byte record. The terminal export writes that record separately as `Work:memory.bin`;
a missing/partial record fails the host accounting check. Ordinary release builds
perform no accounting I/O.
`OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT` selects relative `memory.bin` instead,
for a physical run whose Shell current directory owns its inputs and results.
It changes only the instrumented export path; the default emulator path is retained.

The current MEMD record (magic `0x4D454D44`) starts with sixteen big-endian u32 fields: magic, live capacity, peak live
capacity, cumulative allocated, cumulative freed, live after preparation, cumulative
freed before assembly, entry free memory, entry largest free block, Exec version,
live after assembly, live after cleanup, DOS version, retained runtime-prefix bytes,
packed-record bytes and source bytes. Three further fields count successfully
compiled expressions, evaluation calls and compiled program bytes; nine fields
store three DOS DateStamps (days/minutes/50-Hz ticks), taken before preparation,
after preparation and after assembly. These clocks include instrumentation cost
and have 20 ms resolution; use separate release runs for performance claims.
The stage portion appends the E-clock frequency and error flags (u32 each), seven exclusive
elapsed totals (u64, high word first), then seven stage-entry counts (u32). Stages
are source I/O and other preparation, package setup, tokenization, binding/raw
records, expression preparation, runtime finalization, and module discovery.
Discovery includes path seeding/scanning, candidate declaration indexing, and
graph resolution. Selection within a chosen source file stays in the first stage.
`MEMORY_STAGE` changes the active stage;
clock 0 initializes it and clock 1 flushes and stops it. The stage sum must agree
with the coarse preparation clock within 40 ms. All values are big-endian.
Error bits 64 and 128 distinguish a bounded block-reserve rejection from an
Exec allocation failure. Four trailing u32 fields record the failure count,
last requested allocation size and that block's previous capacity and used bytes. They are
recorded only in instrumented builds.

The profiler uses [timer.device ReadEClock](https://amigadev.elowar.com/read/ADCD_2.1/Includes_and_Autodocs_2._guide/node04FB.html)
for short intervals. Terminal save closes the device and deletes its request and
message port, including on source rejection. These OS allocations are outside
assembler-owned allocation counters. Error bits are 1 setup failure, 2 invalid
stage, 4 changed frequency, 8 arithmetic overflow, and 16 incomplete preparation
at terminal save. Positive runs require zero flags; ordinary syntax-rejection
checks permit 16. Allocation failures additionally set 64 or 128. Stage totals
include probe overhead, with no calibration subtraction; compare
coarse phases against the preceding accounting baseline before interpreting rank.

MEMD retains 21 opcode counters at byte 204, 441 ordered adjacent-opcode pairs at
288, and seven work counters at 2052: line bytes, committed tokens, committed
lexeme bytes, source-byte reads, and taken EOL/byte/class branches. Two u64
E-clock totals at 2080 measure scanner/emission, numeric normalization and composed-name helpers and nested token commits.
`TOKEN_BEGIN` resets adjacency per invocation; `TOKEN_OPCODE` and `TOKEN_WORK`
count work; `TOKEN_SCOPE_BEGIN/END` bracket the two nested scopes, while
`TOKEN_SCOPE_CLOSE` closes an active scope on a shared success/failure return.
The detailed token probes require `OPFORGE_TOKEN_DETAIL_TELEMETRY` in addition to
the two general gates; the harness enables it by default when memory telemetry
is requested. `OPFORGE_PHASE_ONLY=1` leaves those hot probes out while retaining
phase clocks and allocation accounting. The probes share the passive ABI. Error bit 32 reports scope
imbalance. Invalid opcodes break adjacency without indexing outside the record.
Nested scope times must not be added to stage totals. Source reads include the
newline prescan and rereads; committed payload excludes failed staging attempts.
The hot probes add substantial overhead: use counts to identify repeated work and
separate release runs to judge performance. Previous telemetry schemas have no
compatibility decoder.

The compact CLI can additionally enable `OPFORGE_PREPARATION_PROGRESS` together
with both memory-telemetry gates. `MEMORY_PROGRESS` then writes a fixed-length
line to captured guest stdout at source-file boundaries and preparation/assembly
transitions. Each line contains a phase code, source ordinal, source line,
packed-record bytes, and tracked live allocation bytes, all in hexadecimal.
The phase codes are 1/2 for source begin/end, 10–15 for order through completed
preparation, and 20–22 for assembly begin/end and output. Progress output is
diagnostic localization only; it adds bounded console I/O and is excluded from
release timing and parity proof. The extra gate emits no code or data by itself.

`MEMORY_COUNTER_CLEAR` and `MEMORY_COUNTER_INC` provide local, gated work
counters without touching registers or CCR. The compact block selector uses
them for queue insertions, scanned packed records and numeric mark attempts;
their storage and updates are absent from ordinary builds. With preparation
progress enabled, phase 23 reports these three values in the source, line and
records fields after successful block selection. They do not change the MEMD
binary record or its version.

At an assembly failure, phases 24/25 report the failing operation and numeric
name. Phases 26–28 add a bounded sweep snapshot: phase 26 carries assembly pass,
raw sweep selector and section ID; phase 27 carries zero-based packed-record
byte offset and total record bytes (the third field is cleared output size);
phase 28 carries section mode, Hunk section count and the pass again.
Hunk mode 5 now uses raw sweep 0 for its one source-order traversal per pass;
its section ID is the most recently opened section, including unselected sections.
Mapped modes retain filtered sweeps. Pass 0 and record
offset `ffffffff` identify failure before record execution starts.

`ASSEMBLY_POSITION` stores this snapshot at sweep and section-control
boundaries, preserving D0 and CCR and zero-extending word-valued section fields.
A0/A1 are also preserved.
Its `AssemblyPosition` type,
20-byte owner storage and calls require all three progress gates. Bases are
loaded with `lea`, then fields are read/written through relative offsets;
`MEMORY_PROGRESS_BLOCK` and `MEMORY_PROGRESS_RECORDS` apply the same rule to
reporting. These sites retain base-relative addressing after the Rust repair
for absolute `Base+Struct.Field` address operands. No per-record
logging or MEMD schema change is introduced. The self-host test decodes the
terminal snapshot from that run's captured stdout. Its percentage describes
position within the current record-buffer scan, not overall assembly work:
section filtering, repeated passes and loop replay prevent that inference.
An explicit failed guest completion remains a failed assembly; progress capture
does not turn it into artifact parity or release timing.

Phase 300 reports aggregate assembly work once when the compact assembly owner
returns, on success or failure: `f` is scheduled source sweeps, `l` is selected
record visits (including layout and repetition controls and loop replay), and `r` is
zero. `TraversalPasses` and `TraversalRecords` use the existing gated local-counter
macros; their storage and updates require both memory gates, and reporting also
requires preparation progress. Hunk uses two sweeps for a complete two-pass
assembly independently of output-section count; mapped modes can schedule more
than two. Record visits count executed traversal work, not unique source lines.
Counters measure actual work, not completed statements or assembly success; the
normal terminal result owns completion. The fixed progress-line format and MEMD
schema are unchanged. Decode one captured run with:

```sh
python3 scripts/performance/decode_native_progress.py --assembly-progress-text capture.txt
```

The JSON `assembly_work` field exposes
`scheduled_source_sweeps` and `selected_record_visits`. A binary OFPR record can
also be supplied to retain its independent completion validation.

`SelectionPosition` is a separate 12-byte owner snapshot of the last attempted
numeric package row: u32 `Priority`, `Recipe`, and `Projection`. Its type, storage
and `SELECTION_POSITION_CLEAR/CANDIDATE/PROJECTION` calls require the same three
progress gates. Clear initializes every field to `ffffffff` (no candidate yet).
Candidate receives a word priority memory operand and a byte recipe memory
operand, zero-extends each, and resets projection to `ffffffff` (no projection
attempt yet for this row). Projection receives a byte kind memory operand and
zero-extends it. Inputs must use stable memory addressing, not stack-relative
or scratch D0/D1-indexed operands. Inputs are read before loading the capture
base, so A0-based operands are supported. Every macro saves CCR before setup,
preserves all registers and stack depth, and uses `lea` plus relative field
stores. Missing any gate emits no calls or type; owners gate storage identically.

Terminal failure phase 29 uses `MEMORY_PROGRESS_BLOCK` to report these fields
in priority/recipe/projection order alongside the other terminal failure phases. Updates replace
one bounded snapshot; they perform no console I/O or variable-length logging and
do not enlarge the MEMD record, event buffer or production request storage.
Numeric identities describe an attempted row and projection, not their semantic
meaning or proof that the row completed. The host macro transparency test covers
all incomplete gate combinations and compares enabled bytes to the explicit
balanced preservation sequence; it does not execute the guest or establish parity.

The completion diagnostic in
[`binding_telemetry.i`](../../native/motorola68000/amigaos/debug/binding_telemetry.i)
and [`debug.amigaos.binding_diagnostic`](../../native/motorola68000/amigaos/debug/opforge_binding_diagnostic.asm)
uses the same three gates. Only scopes, imports and the compact app include its
macros. The dedicated owner retains a 308-byte primary snapshot, three optional
308-byte correlation snapshots, and a twelve-byte pending check (1244 bytes).
Scopes and imports each supply a twenty-two-byte view descriptor. MEMD
remains 2280 bytes. Missing any gate emits no diagnostic calls, imports, storage
or changes to the original failure branches.

`BINDING_ATTEMPT(stage, entry, related)` records the check about to run; entry
and related are zero-based source-entry indices, with `ffffffff` meaning absent.
`BINDING_ATTEMPT_WORD` zero-extends an import's word-valued target index.
`BINDING_CANONICAL_TARGET(stage, target, baseWord)` follows a successful binder
call: it converts the returned ID to a zero-based canonical target index and
retains the previous origin proxy index as related. A target-declaration failure
therefore captures the composed canonical name and its declaration flags.
`BINDING_DIAGNOSTIC_CLEAR` starts one completion. At the existing routine return,
`BINDING_DIAGNOSTIC_COMMIT(status, scope, view)` latches only a nonzero status;
the first actual failure wins, including a nested import failure before outer
scope completion. Stage 17 identifies final numeric member-plan acquisition for
an imported forward occurrence; stage 13 remains ordinary import resolution.
The descriptor supplies word offsets for count, base, current,
entries pointer, arena pointer/used extent, entry stride and name offset/length,
plus flags and owner offsets. `DECLARED` is bit zero of the current shared entry
ABI; the owner is a one-based source-entry index, with zero meaning absent.
The observer copies at most 28 entry bytes and 255 canonical-name bytes. It
checks the index against count and the name extent against the owned arena,
rejects arithmetic overflow, and stores a zero-padded name independently of the
source allocations. Descriptor entry fields must fit inside the bounded stride.
For a valid stage-14 target, the observer scans at most the retained entry count
for declarations: a complete folded canonical match takes priority over the first
folded final-component match. Import proxies (flag bit three) are excluded:
validated references do not constitute owner declarations. Each candidate name must fit the owned arena and
the 255-byte limit. A match replaces the entry/name snapshot with stage 15 or 16
and retains the failed target index as related; absent matches retain stage 14.
This search uses no binding hash and changes no source entries. It reuses the
primary snapshot and pending storage. At stages 15/16 only, the observer also
copies the declaration's owner and immediate previous/next entries into the
three correlation views. Every related entry must fit the retained count before
being read; absent owners and out-of-range neighbors remain empty. Each view
uses the same entry/name bounds and zero padding as the primary. Its stage is
inherited, its index identifies the related entry, and its related index points
to the primary declaration. The original failure snapshot remains unchanged.
Marks and commits allocate nothing and perform no I/O.

`BINDING_DIAGNOSTIC_REPORT(dosbase)` emits through the existing progress ABI at
app failure reporting, with no output when no completion failure was latched.
Its hexadecimal `f`, `l`, `r` fields have these diagnostic meanings:

| Phase | `f` | `l` | `r` |
|---:|---|---|---|
| 32 | failure stage or declaration-search result | entry index | related entry index |
| 33 | base in high word, count in low word | current lexical scope | canonical-name byte count |
| 34 | raw entry bytes 0–3 | bytes 4–7 | bytes 8–11 |
| 35 | raw entry bytes 12–15 | bytes 16–19 | bytes 20–23 |
| 36 | raw entry bytes 24–27, padded | zero | zero |
| 64–84 | name chunk's first four bytes | next four bytes | next four bytes |
| 85 | final four name-buffer bytes | zero | zero |

Name chunks are big-endian byte groups; concatenate phases 64–85 in order and
truncate to the phase-33 byte count. Raw entry fields follow the current
`binary_binding_records.Entry` layout. The owner-supplied entry width determines
the array stride and field bounds even when it exceeds the observer's 32-byte
capture prefix. Enlarged entries therefore retain the same bounded diagnostic
schema and canonical-name capture. The ordinary progress `m` field still
reports tracked live allocation bytes.

Optional correlation views use the identical row schema at distinct phase bases:

| View | Five metadata phases | Twenty-two name phases |
|---|---|---|
| declaration owner | 96–100 | 128–149 |
| previous entry | 160–164 | 192–213 |
| next entry | 224–228 | 256–277 |

A valid correlation view emits every row; an empty view emits none. The primary
phases and their meaning remain unchanged. Correlations are neighboring storage
and ownership observations, not additional assembly failures.

For a packed-record consumer failure, the same failure-only observer may emit
the optional 80-byte capture at phases 336–343. The first row stores the
cursor byte offset, containing-record byte offset (`ffffffff` when absent), and
record extent. The next 64 bytes hold the first 32 record bytes and a 32-byte
window beginning at the cursor (or one byte before it when the cursor is
nonzero). Phases 336–341 each carry 12 bytes; phase 342 carries the final four
bytes in its first word and zeroes in its next two words. Phase 343 carries the
consumer stage (8 for prepared-record remapping, 18 for block indexing, 19 for
block references) and zeroes in its next two words. The decoder trims the
record prefix to the reported extent and preserves the cursor window as a
zero-padded 32-byte capture; neither view has a separately reported known
length. Stage 8 requires a matching primary binding snapshot; block consumer
stages can be decoded without one because they have no binding lexical base or
count. This observer is bounded, retains no pointers, and emits no allocation
or I/O of its own. Emission requires `OPFORGE_DEBUG_CONTRACTS`,
`OPFORGE_MEMORY_TELEMETRY`, and `OPFORGE_PREPARATION_PROGRESS`; release builds
omit it.

Numeric failure stages are:

| Stage | Check |
|---:|---|
| 1 | lexical scope still open |
| 2 | aggregate import completion |
| 3 | section completion |
| 4 | output-section resolution |
| 5 | undeclared explicit name |
| 6 | exhausted lexical parent search |
| 7 | module visibility |
| 8 | prepared-record identity remap |
| 9, 10 | missing module in all-import or selected-import validation |
| 11, 12 | invalid selected names in all-import or selected-import validation |
| 13 | unresolved import proxy |
| 14 | canonical import target is undeclared |
| 15 | declared canonical target found by linear scan |
| 16 | first declared spelling with the same leaf component |
| 17 | forward numeric member acquisition |

All macros and passive helper APIs preserve D0–D7, A0–A6, CCR and stack depth;
argument setup is inside the preserving wrappers. No production request, VM or
error buffer is used. Host tests cover all seven incomplete gate combinations
and compare enabled wrapper bytes with explicit balanced save/restore sequences.
This is a bounded failure-localization facility; those checks do not execute the
guest, establish artifact parity or supply release performance measurements.

Decode the captured guest stdout with
`python3 scripts/performance/decode_binding_diagnostic.py /absolute/capture.txt`.
The [decoder](../../scripts/performance/decode_binding_diagnostic.py) rejects
incomplete or inconsistent snapshots and explicitly reports an absent snapshot.
Its result remains localization evidence only.

The compiler/evaluator counters describe actual calls, not a semantic redundancy
proof. The previous telemetry record is superseded, with no compatibility decoder.

`OPFORGE_BINDING_DETAIL=1` adds bounded timing within packed-source preparation
when memory telemetry is enabled. Seven elapsed totals at byte 2112 and seven
call counts at byte 2168 cover source-line packed writing including binding, initial
line plans, conditional/scope/import processing, prepared-record copying and
appending, string line plans, template dispatch, and template-next handling.
Two words at byte 2196 hold sampled binding-callback ticks; the following words
count all binding calls and samples. One in 64 callbacks is timed, so scaled
binding time is an estimate, while the seven boundary times are direct
observations that include probe cost. This option can be combined with
phase-only mode to omit token probes. `OPFORGE_TEMPLATE_WORK=1` adds twelve
aggregate counters at byte 2212 for role calls/searches/candidates,
template-line candidate search and outcomes, and the actual macro-plan and
string-plan work. Candidate counts cover leaf-bucket visits, including hash
collisions; resolved imported targets use a separate exact numeric index and are
not counted as bucket visits. At the pre-index checkpoint these same fields
counted visits through the complete local definition array. These modes emit no probes in
ordinary builds. The record is 2280 bytes; earlier field offsets remain
unchanged.
`OPFORGE_INPUT_DETAIL=1` enables the additional `OPFORGE_INPUT_TELEMETRY`
assembly gate when memory telemetry is active. `MEMORY_INPUT_BEGIN/END` measure
physical line collection alone, before lowering or parsing, and
`MEMORY_INPUT_READ` counts DOS refills within that scope, including EOF reads.
At byte 2260, a u64 E-clock total is followed by u32 collection-call, consumed-byte
and refill counts. Consumed bytes include LF and any rejected overflow byte;
discovery scans and reads outside collection are excluded. Error bit 512 indicates
a nested collection scope; an unfinished scope at terminal save sets bit 256.
The clock includes its own probe cost. Use matched ordinary builds for elapsed
performance comparisons. Missing any of the three gates emits no input probes.

Allocation amounts are actual reserved block
capacities; allocation precedes old-block release so copy/growth overlap is counted.
These counters do not claim to trace OS-wide allocations. Configuration and pre-entry
Version/CPU/Stack/Avail observations accompany constrained guest measurements.

## Bounded capture ownership accounting

`debug/capture_telemetry.i` and `debug.amigaos.capture_profile` add a separate
272-byte observer for the compact source-capture path. Calls, imports and owner
storage require `OPFORGE_DEBUG_CONTRACTS`, `OPFORGE_MEMORY_TELEMETRY`, and
`OPFORGE_CAPTURE_TELEMETRY`. Terminal reporting additionally requires
`OPFORGE_PREPARATION_PROGRESS`. Missing a measurement gate emits no call-site
bytes or observer import; release linking carries no observer code or storage.
MEMD remains 2280 bytes. The observer has no retained pointers, allocation,
clock, or per-byte/per-token event output. Its fixed BSS is outside the tracked
heap accounting. All passive APIs and argument wrappers preserve D0–D7,
A0–A6, CCR and stack depth. The constructor-local
`CAPTURE_CONSTRUCTION_CLEAR_BEGIN counter` and
`CAPTURE_CONSTRUCTION_CLEAR_GAP counter, bytes` macros explicitly output the
counter register; they preserve other registers, CCR and stack depth and emit
nothing when any observer gate is missing.
The include defines `CAPTURE_ACQUISITION`, `CAPTURE_REPLAY` and `CAPTURE_RELEASE`
outside the observer gates. Disabled macro arguments still undergo numeric binding;
their identities must remain available without a gated observer import.
The append wrapper's owner formal is `arena`. Native acquisition currently
misclassifies `lea .block,a0` in an unexpanded macro body as a labeled `.block`;
using a formal named `block` would leave an extra acquisition scope open. The
wrapper avoids that known classifier gap without changing generated instructions.

Counters reset at one compact `execute` entry. Construction counts every record
whose explicit clear and payload copies run, including records subsequently
filtered from retention. Construction copies include the seven outcome longs
and token, lexeme and candidate-plan payloads; header stores are writes rather
than copies. Cleared bytes count actual explicit zero writes, excluding Exec's
allocation clearing. The current 56-byte header has no reserved storage; the
constructor clears only its two alignment gaps, totaling 0–6 bytes per
construction. Overwritten fields and payloads are not cleared first. Retained
composition counts only actual region appends:
header bytes, 20-byte token rows, lexemes, plans, alignment padding and their
captured source-byte extent. Append-copy bytes count the whole copied record.
Configuration and semantic replay materialization count actual capture-to-scratch
copies, including preview calls, separated at the replay acquisition boundary.
Those are work counts rather than unique records or successful semantics.

Generic `binary_memory.grow` counters cover every successful allocation and its
actual `Used` copy across this execute, including unrelated owners. Retained
arena reserves bracket a separate attribution context. On successful growth,
the observer considers current retained capacity plus the new allocation before
subtracting old capacity, so its retained peak includes allocate-before-free
overlap. Retained releases subtract their actual capacity; repeated zero-capacity
cleanup cannot double-count. Neither the context nor snapshots retain descriptor
addresses when the region table moves.

Acquisition is observed immediately after graph-span ownership transfers to
`CaptureOrder`, at the start of semantic replay. Active includes can acquire
additional captured regions during replay; the acquisition snapshot remains
that initial frontier, while the pre-release snapshot includes later ownership.
Replay records the latest pre-release state on success or the failed replay
return; release records the
latest state after all capture owners have been freed. A failure before a
boundary leaves that snapshot zero and its mask bit clear. Each boundary scans
only the bounded published region table once. Temporary ownership is the reused
`CaptureLine` arena; the constructor's transient plan owner has already been
freed at these boundaries. Table and temporary used extents come from their
blocks. Order used is its published count times `SPAN_BYTES`, validated against
capacity, because its writer keeps count separately from `memory.Block.Used`.

Terminal report emits these 22 rows through `memory_profile.progress`, once
before memory save, including on failure. Progress `m` retains its usual tracked
live-heap meaning and is independent of the fields below.

| Phase | `f` | `l` | `r` |
|---:|---|---|---|
| 400 | schema 1 | error bits | boundary mask |
| 401 | retained record count | retained headers | retained token bytes |
| 402 | retained lexemes | retained plans | retained padding |
| 403 | retained source bytes | constructed records | explicit cleared bytes |
| 404 | construction copies | retained append copies | configuration materialization copies |
| 405 | replay materialization copies | global grow calls | global grow Used-copy bytes |
| 406 | retained grow calls | retained grow Used-copy bytes | retained peak capacity including overlap |

Snapshot bases are 416 acquisition, 424 replay and 432 release. At each base:

| Relative phase | `f` | `l` | `r` |
|---:|---|---|---|
| 0 | retained region count | retained used | retained capacity |
| 1 | retained slack | temporary used | temporary capacity |
| 2 | table used | table capacity | order used |
| 3 | order capacity | temporary slack | table slack |
| 4 | order slack | zero | zero |

Cumulative counters saturate at `ffffffff` and set error bit 1. Bit 2 marks an
invalid owner extent or snapshot layout; bit 4 marks retained-capacity accounting
underflow. The boundary mask uses bits 1, 2 and 4 for acquisition, replay and
release. Any error makes the accounting unsuitable for numerical conclusions.
The snapshots describe capture ownership, not total preparation ownership or
release timing. Existing completion and fresh-run parity evidence remains
independently required.

The capture decoder defaults to the current 56-byte header. For a frozen export,
pass `--capture-source /absolute/bootstrap/src/experimental/binary_capture.asm`
to derive the header width from that executable's validated source. Comparison
runners bind this width before replacing workload inputs; the assembled source
can describe a different layout from the running executable. They never infer
the layout from observed counter totals. This is host accounting for preserved
comparison images, not native format compatibility or version dispatch.

## Sampled native routine hotspots

`debug/hotspot_telemetry.i` owns the reusable `HOTSPOT_CLEAR`,
`HOTSPOT_ENTER slot`, `HOTSPOT_LEAVE slot`, measured `HOTSPOT_CALL slot,target` /
`HOTSPOT_BSR slot,target`, exact `HOTSPOT_COUNT counterID,amount` and
`HOTSPOT_REPORT dosbase` macros.
The observer operations require `OPFORGE_DEBUG_CONTRACTS`, `OPFORGE_MEMORY_TELEMETRY`
and `OPFORGE_HOTSPOT_TELEMETRY`; reporting also requires
`OPFORGE_PREPARATION_PROGRESS`. Missing a gate emits no call-site instructions,
observer import, code or storage. Slot constants remain available outside gates.
Passive counter/timing macros and observer APIs preserve D0–D7, A0–A6,
CCR and stack depth. Measured-call wrappers forward the callee's result
registers and flags.
Slot arguments are operands, for example `#HOTSPOT_NAME_BIND`.

The observer has 1,756 bytes of fixed static storage: 32 counter/timing rows,
16 exact work counters,
a bounded 32-frame nesting stack and metadata. It allocates nothing and emits
only one terminal report. `memory_profile.readClock` is a separately gated
bridge to its already-open timer: A0 receives the high/low E-clock timestamp,
D0 returns frequency or zero if unavailable, other registers are preserved,
and CCR reflects D0. Clock reads change no memory accounting or phase state.

`OPFORGE_HOTSPOT_SAMPLE_PERIOD` selects 64, 128 (default) or 256 calls;
`OPFORGE_HOTSPOT_PHASE_SEED` selects 0–255 (default 0). A slot's zero-based
call index is selected when `(index + 17*slot + seed) & (period-1) == 0`.
Every enter/leave updates bounded nesting; fully unsampled paths read no clock.
An independently selected invocation records an own sample. Its direct
instrumented children are timed even when not independently selected, allowing
their complete wall intervals to be subtracted from the parent's sample.
Deeper untimed descendants are already inside those child intervals. Forced-only
child windows contribute no own-sample elapsed totals. Recursion uses separate
frames, even when slot identities repeat.

Inclusive sampled time contains instrumented children. Exclusive sampled time
subtracts those direct children; uninstrumented callees remain attributed to the
parent. Timestamps exclude some wrapper work but do not remove all observer
overhead. The decoder reports measured sample seconds and separately marked
estimates scaled by calls/own samples. Fixed-period samples can alias workload
patterns; use different periods/seeds to check ranking sensitivity. Inclusive
times overlap, and independently sampled exclusive estimates do not form an
exact decomposition of full assembly time. Do not add them to the measured
preparation stages. A routine with no own sample has no time estimate.

Selected parents incur forced-child clock reads that most unsampled parents
avoid. Child entry work before its timestamp and exit bookkeeping after its
end timestamp remain in the parent's exclusive interval. Scaling that parent
sample to every call also scales those conditional observer costs. Consequently,
parent estimates can be systematically inflated even when two periods/seeds
agree. Short leaf handlers can likewise be dominated by clock/wrapper margins;
chain traversal additionally includes its per-probe/byte counter work. These
estimates locate work but do not establish release-time seconds or obtainable
savings. Actual identical-input overall durations measure observer overhead;
exact work counts remain independently useful.

| Slot | Actual instrumented boundary |
|---:|---|
| 0 | `binary_frontend.captureLine` |
| 1 | `binary_frontend.captureCandidates` |
| 2 | `binary_capture.create` |
| 3 | Retained append copy in `binary_source_schedule.i` |
| 4 | `binary_memory.grow`, including allocation/copy/release |
| 5 | `binary_frontend.replayLine` |
| 6 | `binary_frontend.previewCapture` |
| 7 | `binary_frontend.writeTokens` |
| 8 | `binary_frontend.bind` |
| 9 | `binary_scopes.bindName` |
| 10 | `binary_imports.resolve` |
| 11 | `binary_expression.compileMode` |
| 12 | `binary_frontend.processRecord` |
| 13 | `binary_frontend.bindCapturedPlan` |
| 14 | `binary_frontend.tokenizeLine` |
| 15 | `binary_scopes.lookup` |
| 16 | `processRecord` source-control callback |
| 17 | Declaration normalization |
| 18 | Conditional processing |
| 19 | Metadata checks (including deferred validation) |
| 20 | Configuration statement binding |
| 21 | Operand-role binding |
| 22 | Scope processing |
| 23 | Configuration value capture |
| 24 | File-effect processing |
| 25 | Data preparation |
| 26 | Expression preparation (ordinary or unfolded) |
| 27 | Prepared-record copy |
| 28 | Graph updates |
| 29 | Scope count publication |
| 30 | Lookup name hashing |
| 31 | Lookup bucket-chain traversal, including name comparisons |

Terminal progress uses decimal phases 500 (schema 1, error bits, period),
501 (E-clock frequency, maximum depth, active depth) and 502 (capacity, seed,
stopped state). For slot `i`, phases `512+3*i` through `514+3*i` contain
`id/calls/completed-own-samples`, `completed-timing-windows/inclusive-high/low`
and `exclusive-high/low/reserved-zero`. Work counter `i` uses phase `608+i`, with `id/value/reserved-zero`.
There are exactly 115 rows. MEMD remains
2,280 bytes; capture accounting is unchanged.

Exact counters do not read the clock or call an observer routine. Updates
preserve CCR and D0, saturate on overflow and stop observation on the same
arithmetic-error bit. Counter identities must be compile-time indices in `0..15`, **without**
`#`; the amount is a long source operand. For example:

```asm
.HOTSPOT_COUNT HOTSPOT_LOOKUP_HASH_BYTES, d6
.HOTSPOT_COUNT_IF_EQ_BYTE capture.Record.Count(a4), #0, HOTSPOT_REPLAY_EMPTY_RECORDS, #1
```

Predicate macros own their conditional gates and comparisons. `IF_EQ` uses a
long immediate comparison, `IF_EQ_BYTE` a byte immediate comparison, and
`IF_NE_WORD` reserves D1 internally: its expected/amount operands must not use
D1. Operands must not depend on SP because preservation uses the stack. The
measured-call wrappers emit precisely the original call when disabled and return
the callee's result registers and CCR when enabled; callers keep their ordinary
status branch immediately after the wrapper.

| Counter | Observable work |
|---|---|
| `CAPTURE_EMPTY_RECORDS` | Retained physical captures with zero TKVM tokens |
| `CAPTURE_EMPTY_BYTES` | Their complete aligned encoded record bytes, including headers/plans/padding |
| `REPLAY_EMPTY_RECORDS` | Resolved zero-token captures visited by replay |
| `PROCESS_RECORDS` | All `processRecord` entries, including expanded records |
| `PROCESS_EMPTY_INPUTS` | Header-only inputs reaching ordinary declaration processing (excludes early source-control consumption) |
| `PROCESS_EMPTY_OUTPUTS` | Header-only outputs reaching normal successful publication |
| `SOURCE_CONSUMED` | Successful source-control callbacks consuming a record |
| `CONDITION_CONSUMED` | Conditional processing consuming a record without a published label |
| `LOOKUP_HASH_BYTES` | Input-name bytes hashed |
| `LOOKUP_CHAIN_PROBES` | Entries visited in bucket chains |
| `LOOKUP_NAMESPACE_REJECTS` | Entries rejected before length/name comparison |
| `LOOKUP_LENGTH_REJECTS` | Namespace-compatible entries rejected by length |
| `LOOKUP_NAME_COMPARES` | Namespace/length-compatible name comparisons started |
| `LOOKUP_COMPARE_BYTES` | Byte pairs compared, including a mismatch pair |
| `LOOKUP_HITS`, `LOOKUP_MISSES` | Lookups finishing with a match or exhausted chain |

An empty representation is a pruning **candidate**, not proof of no side effects.
Conditional, scope, module and macro controls may emit nothing yet change state;
these counters do not authorize discarding them. No record handling changes in
this measurement slice. Per-byte comparison counters and additional wrappers
add observer work; compare enabled durations with the identical light baseline.

Errors stop only the observer, leaving production control flow intact:
invalid slot 1, stack overflow 2, mismatched leave 4, unavailable clock 8,
changed frequency 16, arithmetic overflow 32, invalid period 64, unfinished
depth at report 128. Such a report is diagnostic evidence, not valid profiling.
The [decoder](../../scripts/performance/hotspot_telemetry.py) rejects missing,
duplicate, erroneous or inconsistent rows, invalid completed sample counts,
remaining depth, stopped state and exclusive totals exceeding inclusive totals.
The A6000 runner additionally requires the requested period/seed and MEMD
frequency to agree, and preserves assembly success separately from a profiling
failure. Artifact parity and fresh completion remain independently required.

## Event ABI

Each 28-byte record contains:

| Offset | Field | Width |
|---:|---|---:|
| 0 | event kind | 2 |
| 2 | contract ID | 2 |
| 4 | routine ID | 2 |
| 6 | statement index | 2 |
| 8 | line number | 4 |
| 12 | arg0 | 4 |
| 16 | arg1 | 4 |
| 20 | arg2 | 4 |
| 24 | arg3 | 4 |

The initial buffer holds eight records. Capacity is a safety boundary, not a
diagnostic tuning knob.

`EVENT_CLI_DEBUG_HEADER` is the first adopted production event. At the native
CLI debug-header boundary it records debug-enabled state, output format, input
path storage, and binary-output path storage. It replaces only the free-form
header line in debug-contract builds; release builds retain the existing text.

Proof level: D. The FS-UAE harness executes the real CLI branch and validates
the event ID and four arguments. This test proves native event emission and
preservation at this site. This test does not prove unrelated CLI parity or any
later tokenizer, parser, selector, encoder, or output boundary.

## Build modes

`OPFORGE_DEBUG_CONTRACTS` enables the fixed-size stubs. Without it, macros
expand to zero bytes. A layout-sensitive NOP mode is intentionally deferred
until a concrete need exists.

## Bounded assembly progress bridge

`opasm.amigaos.progress` is a diagnostic bridge for diagnosing long
native assembly runs. It is linked into the production composition only when
`OPFORGE_DEBUG_CONTRACTS` is defined; its module and call sites emit zero bytes
in an ordinary release build.

The module owns one 128-byte memory record and two private tick words. Passive
updates preserve D0-D7/A0-A6, CCR, and stack depth and never write production
request, VM, image, diagnostic, or output storage. The CLI samples AmigaDOS
`DateStamp` only at coarse phase, optional heartbeat, and terminal boundaries.
Statement visits perform bounded memory updates but no clock, console, or file
operation.

The work-counter option adds a separately gated 128-byte `OFWM` companion with
`OPFORGE_PROGRESS_WORK_COUNTERS`. It is correlated to `OFPR` by run ID and
counts statement visits by pass mode, layout rounds/reasons, flow direction and
span, retained statement classifications, and convergence/final image bytes.
Every group saturates and sets a defined overflow bit. The companion's code,
call sites, and storage emit nothing in release or progress-only builds.

The symbol/expression-counter option adds a separately gated 256-byte `OFSE` companion with
`OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS`. Aggregate mode counts
exact/scoped/imported/final-component lookup calls and outcomes, expression
request/parse/compile/bind/evaluate outcomes, and lookup/request phase identity.
`OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL` adds actual candidates, compared byte
positions, a bounded exact-probe histogram, and maximum chain depth. Detailed
mode owns one private fixed 512-byte scratch table; aggregate mode has no such
storage and leaves every detailed field zero.

The `OFSE` module observes existing lookup and expression boundaries. It never
changes lookup order, ambiguity, diagnostics, source position, expression
results, hash chains, or callbacks. Its passive routines preserve D0-D7,
A0-A6, CCR, and stack depth; its record-pointer getter returns A0 as its sole
documented ABI output. Scoped and imported counters retain their logical class
while nested exact/final comparisons remain attributed to their actual owner.
Release, progress-only and work-counter-only builds emit none of the
symbol/expression-counter code or storage.

The runtime-counter option adds a separately gated 192-byte `OFVE` companion with
`OPFORGE_PROGRESS_RUNTIME_COUNTERS`. It counts only provisional CPU-neutral
TKVM/PRVM/EXVM/ExprVM and program invocations/opcodes, coarse service entries,
selector candidates, encoder program rows, and marginal phase totals. Fixed
four-entry private stacks restore enclosing VM/program and service contexts for
nested executor, selection/value, and encoding/branch/fixup calls. Every passive
routine preserves D0-D7/A0-A6, CCR, and stack depth; every counter saturates with a visible bit;
there is no per-opcode identity, PC, address, timing, event I/O, VM rewrite, or
target-semantic decision. Release and earlier counter-only builds emit none of
the runtime-counter imports, calls, code, or storage.

Heartbeat and graceful diagnostic abort are separately default-off. Builds may
set `OPFORGE_PROGRESS_HEARTBEAT_QUANTUM` or
`OPFORGE_PROGRESS_ABORT_VISITS`; reaching the latter follows the normal failure
path and seals an explicitly incomplete record. A heartbeat writes the existing
bounded structured-event buffer and may be dropped when it is full. The memory
record remains authoritative.

The complete binary schema, decoder command, flag meanings, proof boundary, and
current combined frontend phase are documented in
[`opforge-native-progress-record-v1.md`](../performance/opforge-native-progress-record-v1.md).
An active or incomplete record is localization evidence only and cannot satisfy
native parity proof.

## Experimental compact CLI record output

The compact CLI has one optional explicit artifact request: `-x`/`--hex
[FILE]` or `-s`/`--srec [FILE]` selects Intel HEX or Motorola S-record output;
`-g`/`--go ADDRESS` supplies a 4–8 digit hexadecimal start address. The
record writers operate on flat addressable output and reject Hunk record
conversion. Their renderer also supports sparse numeric spans, while CLI sparse
`.org` parity remains unclaimed. The renderer uses an 84-byte frame and a
caller-owned 4096-byte streaming buffer. Ordinary binary/Hunk output allocates
neither this buffer nor captured output spans.

When debug contracts, memory telemetry, and preparation progress are enabled,
phase 31 reports span-capture event count, record count, and ASCII output bytes
in the existing bounded progress line. The reusable gated
`MEMORY_COUNTER_ADD` macro preserves D0 and CCR; counter storage and updates
are absent from ordinary builds. Listing and source metadata are later slices.
Byte counts describe rendered chunks returned to transport, not completed
device writes when an output operation fails.
These behaviors are provisional. They do not establish performance or parity;
see the [native progress record](../performance/opforge-native-progress-record-v1.md)
for the instrumentation contract.
