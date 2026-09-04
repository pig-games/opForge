# Native Assembly Progress Bridge Record v1

Status: provisional performance bridge for optimization-plan Items 0a-0e. This is
not an OFTB/OFTB-stable result format and is not semantic proof.

The native debug-contract build owns one fixed, big-endian, 128-byte `OFPR`
record at `opasm.amigaos.progress.OpasmProgressRecord`. Release builds do not
link the module or emit its call sites. The record is memory-resident; a
debugger or controlled harness may copy exactly 128 bytes and decode it with:

```sh
python3 scripts/performance/decode_native_progress.py progress.ofpr
```

`--require-complete` fails closed unless the record is terminal, has the
complete bit, and carries zero exit status. A record with no active or terminal
state is malformed. An active record or an explicit diagnostic abort is useful
only as localization evidence and cannot satisfy Level D.

## Layout

All integers are unsigned big-endian. `0xffffffff` is the unavailable sentinel
for the current and last-completed statement fields.

| Offset | Bytes | Field |
|---:|---:|---|
| 0 | 4 | magic `OFPR` |
| 4 | 2 | schema version (`1`) |
| 6 | 2 | flags |
| 8 | 4 | run ID (initial coarse tick) |
| 12 | 2 | phase |
| 14 | 2 | pass |
| 16 | 2 | layout round |
| 18 | 2 | reserved |
| 20 | 4 | current statement |
| 24 | 4 | last-completed statement |
| 28 | 4 | total statements |
| 32 | 4 | statement visits |
| 36 | 4 | current source ID |
| 40 | 4 | current module ID |
| 44 | 4 | current VM/service ID |
| 48 | 4 | current program ID |
| 52 | 4 | flow redirects |
| 56 | 4 | backward redirects |
| 60 | 4 | last-progress tick |
| 64 | 4 | run-start tick |
| 68 | 4 | total elapsed ticks |
| 72 | 4 | current-phase start tick |
| 76 | 32 | elapsed ticks for phases 1 through 8 |
| 108 | 4 | heartbeat visit quantum |
| 112 | 4 | next heartbeat visit |
| 116 | 4 | diagnostic abort-after-visits limit |
| 120 | 4 | overflow bits |
| 124 | 4 | terminal CLI status |

Flags are active `0x0001`, complete `0x0002`, incomplete `0x0004`, abort
requested `0x0008`, and heartbeat enabled `0x0010`. Complete and incomplete are
mutually exclusive. Overflow bit 0 reports saturated statement visits and bit
1 reports saturated phase ticks. All other flag and overflow bits are reserved
and must be zero. A complete record cannot also request abort.

Phases are idle, startup, package, frontend, statement build, pass one, layout,
final emission, and artifacts (`0` through `8`). The current native frontend
interleaves source ingestion and statement construction, so phase 3 measures
that combined boundary honestly; phase 4 is reserved until a measured split is
introduced. Flow counters remain reserved for later bridge items. With Item 0d
enabled, the VM/service field packs the current provisional VM ID in its high
word and service ID in its low word, while the program field carries its
provisional CPU-neutral program ID. Item 0b deliberately leaves the Item 0a
envelope stable and uses the correlated `OFWM` companion below.

## Work-multiplication companion

Defining both `OPFORGE_DEBUG_CONTRACTS` and
`OPFORGE_PROGRESS_WORK_COUNTERS` adds one independently removable, big-endian,
128-byte `OFWM` record. Its run ID must equal the `OFPR` run ID. The decoder
rejects run-ID/state/exit mismatches, unknown flags/modes/overflow bits,
nonzero reserved bytes, contradictory terminal state, and incomplete input
requested as proof:

```sh
python3 scripts/performance/decode_native_progress.py progress.ofpr \
  --work-record work.ofwm --require-complete
```

| Offset | Bytes | Field |
|---:|---:|---|
| 0 | 4 | magic `OFWM` |
| 4 | 2 | schema version (`1`) |
| 6 | 2 | active/complete/incomplete flags |
| 8 | 4 | correlated `OFPR` run ID |
| 12 | 2 | current mode: none/pass one/layout/final emission |
| 14 | 2 | reserved |
| 16 | 4 | pass-one statement visits |
| 20 | 4 | layout statement visits |
| 24 | 4 | final-emission statement visits |
| 28 | 4 | layout rounds |
| 32 | 4 | final emissions |
| 36 | 4 | label-value layout changes |
| 40 | 4 | placement/extent layout changes |
| 44 | 4 | flow-control rows |
| 48 | 4 | forward redirects |
| 52 | 4 | backward redirects |
| 56 | 4 | module rows visited |
| 60 | 4 | endmodule rows visited |
| 64 | 4 | use rows visited |
| 68 | 4 | generic rows visited |
| 72 | 4 | maximum statement index reached |
| 76 | 4 | maximum forward redirect span |
| 80 | 4 | maximum backward redirect span |
| 84 | 4 | convergence image-byte work |
| 88 | 4 | final image-byte work |
| 92 | 4 | overflow bits |
| 96 | 4 | terminal CLI status |
| 100 | 28 | reserved, must be zero |

Overflow bits report saturation for visits (`0x01`), layout counts (`0x02`),
flow counts (`0x04`), classifications (`0x08`), and image bytes (`0x10`). All
counter groups saturate at `0xffffffff`. Flow rows count callback-selected next
rows; forward/backward counts include only actual redirects, not the callback's
explicit sequential-arrival marker. Statement classifications are counted per
visit, intentionally exposing pass multiplication rather than unique rows.

The companion adds no timers or I/O. Pass-end image observation restores the
already-proven zero engine status after passing the byte count to the passive
recorder; a structural contract locks that status-preservation invariant.

## Symbol/expression work companion

Defining `OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS` with
`OPFORGE_DEBUG_CONTRACTS` adds a separately removable, big-endian, 256-byte
`OFSE` record. Its run ID, terminal state, exit status, phase, and pass must
correlate with `OFPR`. Aggregate mode records lookup calls/outcomes, expression
lifecycle calls, and request phase. Defining
`OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL` as well enables candidate, compared-byte,
probe-distribution, and chain-maximum fields. The decoder rejects detail mode
without aggregate mode at assembly time and rejects malformed, contradictory,
unknown, incomplete, overflowing, uncorrelated, or falsely populated aggregate
records at decode time:

```sh
python3 scripts/performance/decode_native_progress.py progress.ofpr \
  --symbol-expression-record work.ofse --require-complete
```

| Offset | Bytes | Field |
|---:|---:|---|
| 0 | 4 | magic `OFSE` |
| 4 | 2 | schema version (`1`) |
| 6 | 2 | active/complete/incomplete/detail flags |
| 8 | 4 | correlated `OFPR` run ID |
| 12 | 2 | current phase |
| 14 | 2 | current pass |
| 16 | 4 | reserved, must be zero |
| 20 | 16 | exact/scoped/imported/final-component lookup calls |
| 36 | 16 | exact/scoped/imported/final-component candidates |
| 52 | 16 | exact/scoped/imported/final-component compared bytes |
| 68 | 16 | exact/scoped/imported/final-component hits |
| 84 | 16 | exact/scoped/imported/final-component misses |
| 100 | 4 | final-component ambiguous results |
| 104 | 4 | expression-snapshot candidates |
| 108 | 4 | expression-snapshot compared bytes |
| 112 | 28 | expression request/parse/compile/bind/evaluate/success/failure calls |
| 140 | 20 | exact-hash probe histogram: 0/1/2/3/4+ |
| 160 | 4 | maximum exact-hash probes |
| 164 | 4 | maximum hash-chain depth observed during insertion |
| 168 | 16 | lookup calls in pass one/layout/final/other |
| 184 | 16 | expression requests in pass one/layout/final/other |
| 200 | 4 | overflow bits |
| 204 | 4 | terminal CLI status |
| 208 | 48 | reserved, must be zero |

Overflow bits report saturation for lookup calls (`0x01`), candidates (`0x02`),
compared bytes (`0x04`), lookup outcomes (`0x08`), expression lifecycle
(`0x10`), probe histograms/max probes (`0x20`), and maximum chain depth
(`0x40`). Every counter saturates at `0xffffffff`.

Exact hash lookups count actual visited chain candidates and compared byte
positions. Scoped lookups count logical cascade calls and qualified-name
candidates; the nested exact lookup owns its actual comparisons and bytes so
the aggregate does not double-count them. Imported lookups count the callback
request, candidate, and result; comparisons inside the opaque callback are not
observable and therefore contribute zero imported bytes. Final-component scans
count visited label rows and suffix byte positions, including distinct
ambiguous outcomes. Expression bind counts each symbol-snapshot resolution;
snapshot candidate and byte counts reflect its actual scan.

Detailed mode adds a private fixed 512-byte chain-depth scratch table alongside
the 256-byte record. It observes insertion depth without walking or modifying
the production chain. Aggregate mode keeps all detailed fields zero. Neither
mode performs timing, console output, file I/O, lookup caching, index creation,
interning, or expression preparation, and neither changes lookup order,
ambiguity, diagnostics, source position, or expression results.

## Runtime-execution companion

Defining `OPFORGE_PROGRESS_RUNTIME_COUNTERS` with
`OPFORGE_DEBUG_CONTRACTS` adds one separately removable, big-endian, 192-byte
`OFVE` record. Its run ID, terminal state, exit status, phase, and pass must
correlate with `OFPR`. The decoder rejects malformed, contradictory, unknown,
incomplete, overflowing, uncorrelated, or nonzero-reserved-byte records:

```sh
python3 scripts/performance/decode_native_progress.py progress.ofpr \
  --runtime-record execution.ofve --require-complete
```

| Offset | Bytes | Field |
|---:|---:|---|
| 0 | 4 | magic `OFVE` |
| 4 | 2 | schema version (`1`) |
| 6 | 2 | active/complete/incomplete flags |
| 8 | 4 | correlated `OFPR` run ID |
| 12 | 2 | current phase |
| 14 | 2 | current pass |
| 16 | 2 | current VM ID |
| 18 | 2 | current program ID |
| 20 | 2 | current service ID |
| 22 | 2 | reserved, must be zero |
| 24 | 16 | VM invocations: TKVM/PRVM/EXVM/ExprVM |
| 40 | 16 | VM executed opcodes in the same order |
| 56 | 16 | program invocations: tokenizer/parser/expression frontend/evaluator |
| 72 | 16 | program executed opcodes in the same order |
| 88 | 32 | service invocations: expression/selection/encoding/operand/state/branch/fixup/value |
| 120 | 4 | selector candidates attempted |
| 124 | 4 | encoder program rows examined |
| 128 | 4 | overflow bits |
| 132 | 4 | terminal CLI status |
| 136 | 16 | opcodes in pass one/layout/final/other |
| 152 | 16 | service entries in pass one/layout/final/other |
| 168 | 24 | reserved, must be zero |

Overflow bits report saturation for invocations (`0x01`), opcodes (`0x02`),
services (`0x04`), candidates (`0x08`), unknown IDs or service-stack overflow
(`0x10`), and phase buckets (`0x20`). Every counter saturates at
`0xffffffff`. VM/program and service contexts each use fixed four-entry private
stacks, so nested executor calls and nested selection/value or
encoding/branch/fixup calls restore the enclosing IDs shown in `OFPR`.

These IDs are provisional, CPU-neutral bridge identities and intentionally do
not encode an opcode, PC, address, CPU, family, dialect, instruction, or
benchmark. Item 6b replaces them with the shared identities defined by Item 3.
Phase fields are marginal totals, not a VM-by-phase matrix. The observer adds
no timing or event I/O and does not rewrite or accelerate a VM.

## Timing and controls

The CLI supplies a wrapping 50 Hz AmigaDOS `DateStamp` value. It is sampled at
run/phase/terminal boundaries and, only when enabled, at a heartbeat quantum.
The run begins after argument parsing selects an assembly invocation and before
input resolution, so the startup bucket covers input resolution/open and output
bootstrap work before package setup.
There is no per-statement clock call and no per-operation console or file I/O.
Statement visits and per-phase elapsed ticks are saturated in the fixed record;
the corresponding overflow bit makes either loss of range explicit.

`OPFORGE_DEBUG_CONTRACTS` enables the bridge. Heartbeat is default-off and may
be set at build time with `OPFORGE_PROGRESS_HEARTBEAT_QUANTUM`; 4096 is the
initial investigation quantum. `OPFORGE_PROGRESS_ABORT_VISITS` enables the
otherwise-disabled graceful visit limit. Reaching it returns through the normal
CLI failure path and seals the record with `complete=false`, the current and
last-completed statements, elapsed ticks, overflow state, and nonzero status.

The heartbeat uses one bounded structured debug event containing phase/pass,
current statement, total statements, and visits. It is a sampling hint; the
memory record remains authoritative. Neither the record nor heartbeat changes
artifacts, diagnostics, or exit status unless the explicit diagnostic abort
limit is configured.

## Platform and memory companion — provisional OFIO schema 2

`OPFORGE_DEBUG_CONTRACTS` plus `OPFORGE_PROGRESS_PLATFORM_COUNTERS` adds a
fixed 528-byte `OFIO` record, available through
`debug.amigaos.platform_profile.opforgePlatformProfileGetRecordV1`. It uses the
same memory-resident bridge boundary as the earlier companions; copying exactly
528 bytes lets the host decode it alongside the matching OFPR record:

```sh
python3 scripts/performance/decode_native_progress.py progress.ofpr \
  --platform-record platform.ofio --require-complete
```

`OPFORGE_PROGRESS_EXPORT_RECORDS` is a separate, explicit debug-only export
switch. At the existing controlled terminal boundary, after all records/timing
are sealed and before closing dos.library, the debug framework writes
`opforge-profile.ofpr` and each enabled `.ofwk`, `.ofse`, `.ofvm`, `.ofio`
companion to the invocation directory. Run this mode only in a fresh scratch
directory: these fixed filenames are overwritten. Export does not change the
CLI's exit or normal artifacts; missing/short exports invalidate profile
evidence, and no stored file is accepted in place of a fresh completed guest.
The focused runner captures the files in memory and deletes the entire guest
artifact tree before returning. Export I/O is excluded from sealed phase times
and operation counters, but remains part of externally timed process duration.

Schema 1's unfinished 192-byte draft is rejected, not silently interpreted as
schema 2. Item 0e is still in progress: these records are not yet a complete
all-sites memory audit or B01-B10 attribution result.

| Offset | Bytes | Field |
|---:|---:|---|
| 0 | 4 | Magic `OFIO` |
| 4 | 2 | Schema version 2 |
| 6 | 2 | Active=1, complete=2, incomplete=4; I/O enabled=8, bulk enabled=16 |
| 8 | 4 | Correlated OFPR run ID |
| 12, 14 | 2 each | Current phase and pass |
| 16, 18 | 2 each | Current I/O class and next bulk range; terminal records clear both |
| 20, 40, 60 | 20 each | Open attempts, close attempts, read calls by I/O class |
| 80, 100, 120 | 20 each | Completed read bytes, write calls, completed write bytes by I/O class |
| 140 | 4 | Reserved zero: audited native CLI has no seek operation; nonzero is rejected |
| 144 | 12 | Clear calls, requested bytes, completed bytes |
| 156 | 12 | Copy calls, requested bytes, completed bytes |
| 168, 172, 176, 180 | 4 each | Source read bytes, logical-line visits, module candidates, short reads including EOF |
| 184, 188 | 4 each | Overflow bits and CLI exit status |
| 192 | 120 | Five bulk range rows, 24 bytes each |
| 312 | 216 | Nine bulk phase rows, 24 bytes each |

I/O classes 1-5 are source, bootstrap, module, package, artifact. Every current
CLI file operation chooses its own class immediately before its DOS wrapper,
so a nested include or module scan cannot misclassify the enclosing close or
next read. Error returns count as calls but add no completed bytes. Source
bytes count actual source-class reads, including rereads; they are not unique
input size. Logical-line visits count lines delivered to the source frontend,
including a nonempty EOF line without a newline and lines that later fail.

Bulk range IDs 0-4 are other, session, package, state, presence. Phase rows use
the OFPR phase IDs 0-8, including idle. Each row contains clear calls/requested/
completed bytes followed by copy calls/requested/completed bytes. These are
two marginal breakdowns, not a range-by-phase cross product.

With the master platform flag enabled, `OPFORGE_PROGRESS_PLATFORM_NO_IO`
disables class/read/write/source observers and
`OPFORGE_PROGRESS_PLATFORM_NO_BULK` disables clear/copy/range observers.
Both switches may be combined. Disabled public observer bodies compile to
register/CCR-preserving returns; their existing call-site envelopes remain,
so these modes are not equivalent to an uninstrumented release build.
Record flags and decoded `enabled_groups` distinguish disabled groups from
measured zero work. The decoder rejects observations in a disabled group.
`seeks_by_class` contains audited zeros, not inferred filesystem behavior.

Fixed-size helpers record requested bytes before their loop and completed bytes
after it returns, preserving the original output registers and CCR. Completion
consumes the range selection so it cannot leak to later operations. Variable
C-string copies record their observed byte count, including NUL, on return.
The session arena, CLI state helpers, package staging copy, package-derived
state clear, source-text copies, label/hash resets, image gap/main/mapped work,
layout flag resets, fixed-string copies, PRG payload, directive scratch copies,
operand-copy primitive, and per-pass image-presence clear are instrumented.
Only known session destinations select Session; caller-owned request buffers
remain Other. Scalar record construction, encoded output generation, NUL stores,
and nonzero sentinel fills are not byte-copy/zero-clear operations. In particular,
each layout capacity reset clears 100,000 flag bytes and also writes 200,000
sentinel bytes; only the first is included in the clear count. This distinction
must remain explicit in the attribution report. Final coverage review is pending.

This bridge measures the named F1-F3 mechanisms, not every memory instruction.
Residual unmeasured families include text-encoding/struct state resets (1,184
array bytes plus 8 bytes respectively), token/path copy/append helpers, and
small field/string construction inside directive, flow, package and VM services.
Their work must not be reported as zero or included in an inferred total from
these counters; symbolized samples determine whether finer observation is needed.
The PRG payload completion uses actual source-pointer advancement, including the
existing low-word loop bound; a larger requested count cannot silently become
complete-byte evidence.

Platform-enabled CLI profiling begins before initial state clearing, not after
argument parsing. The platform-disabled progress-only boundary is unchanged.
The decoder requires each dimension to sum to the corresponding aggregate;
completed bytes cannot exceed requests, and a successful terminal record
cannot contain unfinished bulk work. Any overflow rejects complete proof.
No per-byte observation, loop elimination, buffering, or output change is added.

## Evidence boundary

Host decoder tests are Level C. Source/assembly and deterministic harness tests
are Levels B/C. The harness seeds both saturating fields at `0xfffffffe`, drives
the real update routines across the boundary, and verifies `0xffffffff` plus
the matching overflow bit. A fresh FS-UAE harness run confirms those checks on
native execution of the 68020-targeted build under the recorded emulator CPU, but
does not prove a full assembly. Only an independently complete, fresh guest run
with explicit zero exit and exact Rust artifact equality can be Level D parity
proof. An `OFPR` complete bit never substitutes for that contract.
