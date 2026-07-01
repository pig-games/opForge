# Native Debug-Contract Modules

These modules provide bounded structured events and generic contract predicates
for native 68000/AmigaOS code. They are debug infrastructure, not a replacement
for production diagnostics.

- `debug_contract_ids.asm` owns stable contract and event IDs.
- `debug_events.asm` owns the bounded event buffer and four-argument writer.
- `debug_assert.asm` owns predicate routines.
- `debug_macros.i` owns fixed-size enabled call sites and zero-byte disabled
  expansions.

Callers must import the assert/events modules using the aliases documented in
`debug_macros.i`, then textually include that file. Define
`OPFORGE_DEBUG_CONTRACTS` only for a debug-contract build.

The routines preserve D0-D7, A0-A6, CCR, and stack balance. They never use
request, service, or last-error buffers. Supervisor state is neither read nor
modified; user-visible CCR is saved and restored with 68020-safe instructions.

Enabled assert call sites and event call sites have documented fixed sizes
locked by the focused assembly test. Disabled macros emit zero bytes. Event
records are bounded and silently stop appending at capacity so instrumentation
cannot overwrite adjacent state.
