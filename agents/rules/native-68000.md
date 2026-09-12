# Native assembly guidance

Use this guide when changing `native/motorola68000` assembly. The
[repository workflow](../../documentation/workflow/README.md) governs scope,
checkpoints and validation cadence. This guide preserves existing calling and
style conventions; it does not prescribe a new native architecture.

## Routine structure and public boundaries

Keep each logical routine in a `.block` / `.bend` pair, with `.block` on the
routine-label line and a trailing routine-name comment on `.bend`. End the block
after its return paths. Ordinary internal branch targets do not need blocks.
Group exported routines under `.pub` before helpers under `.priv`; export only
symbols another module intentionally consumes.

```asm
routineName .block
    ; implementation
    rts
    .bend  ; routineName
```

Public entry points are module ABI. Preserve caller-visible registers unless the
contract explicitly declares outputs or clobbers, keep output meanings stable,
and use named status constants. Do not assume undocumented behavior across an
OS, library or module boundary.

## Caller-facing documentation

Public routines need a short header describing purpose, inputs, outputs, clobbers
and CCR behavior. Private helpers need one when multi-call, shared-state or status
behavior makes it useful. The format can be compact:

```asm
; Pop the top expression-stack value.
; Inputs: D7 = stack depth.
; Outputs: D0 = status; D3 = value; D7 decremented on success.
; Clobbers: D2/A2.
; CCR: reflects D0 on return.
popD3 .block
```

Include relevant memory/stack effects and non-obvious dependencies. Avoid default
`Saved/restored:` or `Preserves:` fields that repeat an obvious prologue/epilogue;
document unusual preservation requirements when callers need them. Comments should
explain invariants and intent, not narrate every temporary store or instruction.

## Register and status discipline

Private hot helpers can use a small documented clobber set when callers can tolerate
it. Avoid reflexive whole-register save/restore; stack traffic is real work.
Public/orchestration routines often benefit from balanced preservation because it
simplifies callers. Follow the actual ABI, not a universal save/restore rule.

Keep register lifetimes understandable. Avoid repeatedly saving a live loop count
from D0 just to call a status-return helper; a different loop-state register or
explicit local slot may be clearer. Do not perform broad register rewrites without
a concrete reason. Keep fall-through paths clear and unnecessary global access out
of per-token, per-byte and per-opcode helpers.

## Condition codes

Callers may branch directly on returned CCR only when the callee documents the
contract or inspection proves it on every return path. `movem` and `rts` preserve
CCR, so this epilogue retains the flags from the D0 status write:

```asm
    moveq #0,d0
    movem.l (sp)+,d1-d7/a0-a6
    rts
```

A later flag-changing instruction, such as `tst.l d3`, would invalidate that D0
contract. Keep an explicit test when flags are unspecified, a path fails to
establish them, intervening code changes them, or a named semantic comparison
is clearer. Never infer OS/library CCR behavior from a return value alone.

Use `beq`/`bne` for zero/nonzero status, `bmi`/`bpl` for signed status and
`bcs`/`bcc` where the unsigned carry/borrow contract calls for them. Compare enum
statuses explicitly. Avoid mixing boolean, signed and enum conventions in one
helper. Remove a redundant failure branch only when the caller has proved all
of the helper's relevant preconditions; preserve real defensive boundaries.

## Validation and instrumentation

Use `.opforgefmt.toml` and `make native-68000-format-check` for formatting.
`make native-68000-format` writes formatter changes across the native tree: inspect
its scope before using it for a small edit. The redundant-test checker is available
as `make native-68000-redundant-test-check`; its findings need the actual CCR contract.
Run behavior checks appropriate to the changed boundary before declaring readiness.

For debug changes, read the [instrumentation guide](native-68000-safe-instrumentation.md).
For native comparison, read the [parity contract](native-rust-parity-porting.md).
Preserve package semantic ownership and the existing CPU-boundary check: legitimate
implementation instruction syntax is allowed, while target-specific behavior must
not leak into generic infrastructure.
