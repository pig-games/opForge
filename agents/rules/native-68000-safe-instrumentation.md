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
  approval
- change production control flow

Instrumentation is production code until preservation, branch neutrality, and
build-mode behavior are proven.

## Required safety note

Record this with every instrumentation patch:

```text
Instrumentation point:
Macro/routine used:
Registers preserved:
SR/CCR preserved:
Stack delta at return:
Shared buffers touched:
Why this cannot change branch decisions:
Removal/stabilization plan:
```

Do not use evidence from an unsafe probe. Classify temporary probes as proof
Level E and remove them before claiming a fix.
