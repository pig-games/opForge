# Native Assembly Progress Bridge Record v1

Status: provisional performance bridge for optimization-plan Item 0a. This is
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
0b-0e populate them.

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
