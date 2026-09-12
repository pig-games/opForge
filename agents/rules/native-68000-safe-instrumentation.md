# Native 68000 Safe Instrumentation Rule Pack

Load this rule pack before adding debug output, an assertion, a trace, an event,
or any other diagnostic to native 68000 assembly. Also load
`agents/rules/native-68000.md`.

## Hard rules

Ad-hoc instrumentation is forbidden. Use only approved macros and routines from
the native debug/assert framework.

Instrumentation must:

- be controlled by debug/contract build flags
- preserve every documented register
- preserve SR/CCR unless its API explicitly documents a no-flags variant
- return with zero stack delta
- avoid request, service, and last-error buffers
- prefer structured event records over free-form text
- have a removal or stabilization plan

Instrumentation must not:

- appear between a flag-setting `cmp`, `tst`, arithmetic, or logical instruction
  and its conditional branch
- inline variable-length logic at a call site
- print from mutable request or service buffers
- enlarge event or request buffers as a diagnostic tactic without explicit
  discussion of the resulting memory and behavior impact
- change production control flow

Instrumentation is production code until preservation, branch neutrality, and
build-mode behavior are proven.

## Verification

Verify register and flags preservation, zero stack delta, shared-buffer safety
and build-mode behavior for the actual instrumentation point. Explain any
non-obvious risk and whether the probe will be removed or maintained. No separate
safety-note artifact is required. Do not use unsafe probes as fix evidence.
