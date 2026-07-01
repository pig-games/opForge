# Native Instrumentation Framework

The native instrumentation framework isolates debug behavior in centralized
68000 routines. Ad-hoc probes are forbidden.

## Safety contract

Enabled macros emit a fixed-size call-site stub. Disabled macros emit zero
bytes. Predicate logic and event writes occur only in module routines.

All current routines:

- preserve D0-D7 and A0-A6
- preserve CCR exactly and do not touch supervisor state
- return with zero stack delta
- write only the dedicated bounded event buffer
- leave the event buffer unchanged when it is full
- avoid request, service, last-error, and production output buffers

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

## Build modes

`OPFORGE_DEBUG_CONTRACTS` enables the fixed-size stubs. Without it, macros
expand to zero bytes. A layout-sensitive NOP mode is intentionally deferred
until a concrete need exists.
