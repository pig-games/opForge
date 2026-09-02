# Native Assembly Progress Bridge Record v1

Status: provisional performance bridge for optimization-plan Items 0a-0c. This is
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
introduced. IDs and flow counters are present but remain zero until Items
0c-0e populate them. Item 0b deliberately leaves the Item 0a envelope stable
and uses the correlated `OFWM` companion below.

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

## Evidence boundary

Host decoder tests are Level C. Source/assembly and deterministic harness tests
are Levels B/C. The harness seeds both saturating fields at `0xfffffffe`, drives
the real update routines across the boundary, and verifies `0xffffffff` plus
the matching overflow bit. A fresh FS-UAE harness run confirms those checks on
real 68020 execution but
does not prove a full assembly. Only an independently complete, fresh guest run
with explicit zero exit and exact Rust artifact equality can be Level D parity
proof. An `OFPR` complete bit never substitutes for that contract.
