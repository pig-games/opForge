# Native Debug-Contract Modules

These modules provide bounded structured events and generic contract predicates
for native 68000/AmigaOS code. They are debug infrastructure, not a replacement
for production diagnostics.

- [`contracts.asm`](contracts.asm) owns stable contract and event IDs.
- [`events.asm`](events.asm) owns the bounded event buffer and four-argument writer.
- [`assert.asm`](assert.asm) owns predicate routines.
- [`macros.i`](macros.i) owns fixed-size enabled call sites and zero-byte disabled
  expansions.

Callers must import the assert/events modules using the aliases documented in
[`macros.i`](macros.i), then textually include that file. Define
`OPFORGE_DEBUG_CONTRACTS` only for a debug-contract build.

The routines preserve D0-D7, A0-A6, CCR, and stack balance. They never use
request, service, or last-error buffers. Supervisor state is neither read nor
modified; user-visible CCR is saved and restored with 68020-safe instructions.

Enabled assert call sites and event call sites have documented fixed sizes
locked by the focused assembly test. Disabled macros emit zero bytes. Event
records are bounded and silently stop appending at capacity so instrumentation
cannot overwrite adjacent state.

## Maintained measurements

Memory/phase accounting, binding diagnostics, capture ownership and I/O counters,
sampled hotspots, template work counters and VM telemetry remain available through
their reusable gated macros. The [instrumentation reference](../../../../documentation/architecture/native-instrumentation-framework.md)
defines the gates, layouts, preservation contracts and decoders. Capture-lifetime
accounting remains a component probe; its contiguous-region assumptions do not
support the current whole-application chunked store.

Shared language controls and storage probes live in
`crates/opforge-asm/src/tests/native_source_controls.rs` and
`native_store_probes.rs`. The former preserves live Rust oracles and fresh native
artifact comparisons; the latter retains independent store probe builders. Their
host tests and component checks do not replace full current-source self-host proof.
