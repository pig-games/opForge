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
In Hunk mode 5, raw sweep 8 is the first section,
9 the second, and so on; raw sweep 0 is the later outside-section control scan.
The section ID is meaningful only during a section sweep. Pass 0 and record
offset `ffffffff` identify failure before record execution starts.

`ASSEMBLY_POSITION` stores this snapshot at sweep boundaries, preserving D0 and
CCR and zero-extending word-valued section fields. A0/A1 are also preserved.
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
scope completion. The descriptor supplies word offsets for count, base, current,
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
`binary_binding_records.Entry` layout. The ordinary progress `m` field still
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
