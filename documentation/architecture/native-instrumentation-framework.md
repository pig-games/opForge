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

Instrumentation may not sit between a flag-setting instruction and its branch.
Every use must carry the safety note required by
`agents/rules/native-68000-safe-instrumentation.md`.

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

`opasm.amigaos.progress` is an Item 0a provisional bridge for diagnosing long
native assembly runs. It is linked into the production composition only when
`OPFORGE_DEBUG_CONTRACTS` is defined; its module and call sites emit zero bytes
in an ordinary release build. Item 0a's result ledger separately attributes the
release-binary difference from the fetched checkpoint to mandatory native guard
cleanup, not to this bridge.

The module owns one 128-byte memory record and two private tick words. Passive
updates preserve D0-D7/A0-A6, CCR, and stack depth and never write production
request, VM, image, diagnostic, or output storage. The CLI samples AmigaDOS
`DateStamp` only at coarse phase, optional heartbeat, and terminal boundaries.
Statement visits perform bounded memory updates but no clock, console, or file
operation.

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
