# Native instrumentation

Use this guide before adding debug output, assertions, traces or events to native
assembly. Use the approved debug/assert framework; ad-hoc injected instrumentation
remains forbidden. The [framework reference](../../documentation/architecture/native-instrumentation-framework.md)
owns the macro/event ABI and build-mode details; the [contract catalog](../../documentation/architecture/native-debug-contracts.md)
owns assertion identities and meanings.

## Safety at the call site

Instrumentation must preserve its documented registers and SR/CCR unless its API
explicitly returns changed flags, return with zero stack delta, and avoid request,
service and last-error buffers. Use debug/contract build flags and bounded structured
events. Documented framework outputs must not be mistaken for passive observations.

Do not insert instrumentation between a flag setter and its conditional branch,
inline variable-length logic at the call site, print from mutable request/service
buffers, or change production control flow. Discuss the memory and behavior impact
before enlarging event/request buffers as a diagnostic tactic. These restrictions
remain in force; this guide does not authorize new instrumentation interfaces.

Verify preservation, branch neutrality, buffer safety and build-mode behavior for
the actual use. Treat instrumentation as production code until those properties
are proven. Explain non-obvious risks and whether a probe will be removed or
maintained; no separate safety-note artifact is required. Unsafe probes cannot
supply fix evidence. Use the [parity contract](native-rust-parity-porting.md) for
what the resulting observations can establish.
