# Native Motorola 68000 Rule Pack

Load this rule pack only when touching supported `native/motorola68000/**/*.asm` files.

This file is both a rule pack and an agent-facing guide to writing good opForge
native 680x0 assembly. It is intentionally practical: prefer small, correct,
measurable improvements over broad rewrites.

## Scope

This guide applies to native Motorola 68000-family assembly in opForge, especially:

- `native/motorola68000/**/*.asm`
- native AmigaOS CLI/runtime modules
- VM runtime slices such as PRVM, TKVM, EXPRVM, and bridge code
- test harness assembly under native Motorola 68000 paths

The root `AGENTS.md` remains binding. This rule pack adds target-specific
assembly guidance; it does not relax repository workflow, safety, CPU-boundary,
or validation requirements.

## Validation baseline

Before treating native Motorola 68000 assembly formatting as complete:

```sh
scripts/workflow/run_native_68000_format_gate.sh
```

or:

```sh
make native-68000-format-check
```

If formatting changes are required:

```sh
scripts/workflow/run_native_68000_format_gate.sh --write
```

or:

```sh
make native-68000-format
```

Then re-run the check.

Use the root `.opforgefmt.toml` unless the user explicitly requests otherwise.

## Routine structure

Every logical routine must be enclosed in a `.block` / `.bend` pair.

Use this shape:

```asm
routineName	.block
	; ...
	rts
	.bend  ; routineName
```

Rules:

- Put `.block` on the same line as the routine label.
- Put `.bend` after the routine's final `rts`, with a trailing comment naming the routine.
- Do not wrap ordinary branch targets or loop labels in their own `.block` unless they are standalone callable routines.
- Local control-flow labels belong inside the enclosing routine block.
- Group exported routines before internal helpers.
- Start the exported group with `.pub`; start helper/internal routines with `.priv`.
- Mark a symbol public only when another module intentionally consumes it.
- Treat public routines as module ABI.
- Public entry points must document or make clear their input/output register contract.
- Public routines should preserve caller-visible registers unless their contract explicitly says otherwise. Use balanced save/restore such as `movem.l` when needed.

## Naming and readability

Prefer the native naming conventions already emerging in the repo:

- routine labels: lower camel case, for example `parseModuleUse`
- data labels: upper camel case, for example `ExprvmStack`
- constants: upper snake case, for example `EXPRVM_STACK_CAPACITY`
- local control-flow labels: short but meaningful, for example `fail`, `done`, `loop`, `copyNext`

Avoid vague labels when the routine is long or has multiple failure modes. Prefer
specific labels such as `missingValue`, `stackOverflow`, or `invalidOpcode` when
they improve diagnostics or code review.

## Register contracts

Document register contracts for public routines and non-trivial private helpers.

Use a compact comment shape:

```asm
; Inputs:
; - A0: bytecode pointer
; - D0: bytecode length
;
; Outputs:
; - D0: status, 0 on success, nonzero on failure
; - D3: scalar result on success
;
; Clobbers:
; - D1-D3/A0-A1
```

Rules:

- Keep ABI registers stable unless the routine contract says they are outputs or clobbers.
- Prefer `movem.l` for balanced save/restore around routines with several preserved registers.
- Do not save registers reflexively in every tiny helper. Make the helper contract clear instead.
- Keep scratch register use local and obvious.
- Avoid hidden dependencies on registers not listed in the contract.

## Condition code register discipline

The 680x0 condition code register is a first-class result channel. Do not ignore
it, but also do not rely on it accidentally.

Many instructions set condition codes directly, including common result-writing
instructions such as:

```asm
moveq #0, d0
moveq #-1, d0
move.l source, d0
clr.l d0
subq.l #1, d0
cmpi.l #1, d7
tst.l d0
```

`rts` does not alter condition codes. Therefore, when a subroutine returns through
a final CCR-setting instruction, the caller can branch directly on the returned
flags.

### Avoid redundant `tst.l d0` after status-return helpers

Bad:

```asm
bsr.w popD3
tst.l d0
bmi.w fail
```

Better, when the helper guarantees that CCR reflects `d0` on return:

```asm
bsr.w popD3
bmi.w fail
```

This is safe for helpers shaped like:

```asm
popD3	.block
	tst.l d7
	beq.s fail
	subq.l #1, d7
	move.l d7, d2
	lsl.l #2, d2
	lea ExprvmStack, a2
	move.l 0(a2, d2.l), d3
	moveq #0, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; popD3
```

Both exit paths end with `moveq ..., d0`, which sets CCR. Since `rts` preserves
CCR, the caller can use `bmi`, `beq`, `bne`, or related branches immediately.

### Keep `tst` when CCR may not match the value being tested

Keep the explicit test when any instruction between the call and the branch may
alter CCR:

```asm
bsr.w helper
move.l d0, SavedStatus   ; MOVE sets CCR based on SavedStatus write, but be careful
; other work here may change CCR
tst.l d0
bne.w fail
```

Keep it when the called routine's return contract does not guarantee CCR.

Keep it when the branch is not testing the returned status but a separate semantic
state. For example, testing a byte in memory after a call is still meaningful:

```asm
jsr someRoutine
tst.b SomeFlag
bne.s flagSet
```

### Make CCR guarantees explicit

If callers branch directly after `bsr`/`jsr`, the helper contract should say so:

```asm
; Outputs:
; - D0: signed status, 0 on success, negative on failure
; - CCR: reflects D0 on return
```

If a helper does not guarantee CCR, say so:

```asm
; Outputs:
; - D0: status
; - CCR: undefined
```

Do not branch directly on CCR after calling helpers with undefined CCR.

## Status return conventions

Use consistent status conventions inside a module.

Acceptable patterns:

```asm
; Boolean status
; D0 = 0 success / not matched
; D0 = 1 failure / matched
```

```asm
; Signed status
; D0 = 0 success
; D0 < 0 failure
```

```asm
; Enumerated status
; D0 = STATUS_*
```

Rules:

- Do not mix boolean, signed, and enumerated status casually inside the same helper family.
- If a helper returns signed status, use signed branches such as `bmi` / `bpl`.
- If a helper returns boolean/nonzero status, use `beq` / `bne`.
- If a helper returns enumerated status, prefer explicit `cmpi` against named constants.
- Document whether `0` means success, false, no-match, or OK.
- Avoid magic numeric status values at call sites when named constants exist.

## Avoid overloading `D0` in hot loops

`D0` is commonly used for return status, but VM loops often also use it as a
working value such as bytecode length, remaining count, token count, or cursor.

That creates save/restore noise:

```asm
move.l d0, EvalRemaining
bsr.w pushD3
bmi.w fail
move.l EvalRemaining, d0
```

Better designs for hot VM loops:

- keep `D0` as final routine status only
- hold bytecode remaining count in another register or a local memory slot
- use pointer/end-pointer pairs, for example `A0 = cursor`, `A1 = end`
- reserve helper status for short-lived use and immediately branch on CCR
- avoid repeatedly spilling loop state around helper calls

Do not perform a broad register ABI refactor unless it is part of the requested
slice. For tactical cleanup, remove redundant tests first.

## Stack helper discipline

For VM stack helpers such as push/pop:

- Define one stack-depth register and keep it consistent.
- Check stack capacity before push.
- Check stack non-empty before pop.
- Return status consistently.
- If caller already proves the stack precondition locally, avoid duplicate checks only when the proof is obvious and adjacent.

Example:

```asm
cmpi.l #1, d7
bne.w endStackFail
bsr.w popD3
moveq #0, d0
bra.s return
```

The post-call failure check is unnecessary when `popD3` only fails on empty stack
and the caller has just proven the stack contains exactly one value.

Inline the pop only when profiling or clarity justifies duplicating stack layout
knowledge:

```asm
cmpi.l #1, d7
bne.w endStackFail
clr.l d7
lea ExprvmStack, a2
move.l (a2), d3
moveq #0, d0
bra.s return
```

Prefer the helper version unless the code is a measured hot path.

## Branch-size discipline

Use branch sizes intentionally.

- Prefer `.s` only when the target is clearly local and stable.
- Use `.w` when the target may move as the routine grows.
- Do not use long branches reflexively.
- Do not spend time shrinking branches during feature work unless the current slice is explicitly about size/performance.

Good:

```asm
beq.s done       ; nearby local label
bne.w fail       ; shared failure exit farther away
bra.w evalLoop   ; central loop target
```

## Prefer simple addressing, but use 680x0 strengths

Good 680x0 assembly should use the machine naturally.

Prefer indexed addressing when it avoids noisy manual address arithmetic:

```asm
move.l 0(a2, d2.l), d3
```

Prefer `lea` for address computation:

```asm
lea ExprvmStack, a2
```

Prefer `moveq` for small constants:

```asm
moveq #0, d0
moveq #-1, d0
```

Avoid sequences that emulate another architecture's style when the 680x0 has a
directer expression.

## Signedness discipline

Be explicit about signed vs unsigned interpretation.

- Use `bmi` / `bpl` for signed negative/non-negative status.
- Use `bcs` / `bcc`, `blo` / `bhs` style branches for unsigned magnitude where supported by assembler syntax.
- Use `beq` / `bne` for zero/nonzero.
- Use `cmpi` against named constants for enums.

Do not write code where the reader has to infer whether `d0 = -1`, `d0 = 1`, and
`d0 = STATUS_INVALID` are part of the same convention.

## Public ABI vs private helper freedom

Public routines:

- must document inputs, outputs, clobbers, and status convention
- should preserve caller-visible registers unless outputs/clobbers say otherwise
- should avoid surprising CCR dependencies unless documented
- should not expose internal stack or VM layout accidentally

Private helpers:

- may be tighter and more specialized
- may rely on module-local scratch conventions
- should still be readable enough for agents to avoid accidental regressions
- should document non-obvious CCR or register contracts

## CPU-specific architecture boundary

Generic opForge Rust VM, Native VM, workflow, and CLI implementation paths must
not grow CPU/family/dialect/register/addressing-mode/instruction-specific logic.

For native Motorola 68000 work, be especially careful not to copy 6502 concepts
into generic layers. Terms such as `accumulator`, `indirect_x`, or 6502-specific
addressing names usually belong in package/family/dialect definitions, fixtures,
examples, tests, or documentation, not generic native VM code.

Native 680x0 implementation details are allowed in `native/motorola68000/**`, but
target CPU semantics for assembled programs should still live in the appropriate
package VM/family/dialect layer.

## Comments

Good comments explain intent, contract, or non-obvious machine behavior.

Useful comments:

```asm
; D7 is ExprVM stack depth.
; CCR reflects D0 on return; callers may branch directly.
; A4 is the stable request-frame base for this run.
```

Weak comments:

```asm
; increment counter
addq.l #1, d0
```

Avoid comments that merely repeat the instruction. Prefer comments that explain
why the instruction is there.

## Agent cleanup rules

When improving existing native 680x0 assembly:

1. Preserve behavior first.
2. Prefer local, mechanical cleanups.
3. Do not combine CCR cleanup with large ABI/register refactors.
4. Do not rewrite working routines into a new style without a narrow reason.
5. Keep changes reviewable.
6. Run the native format gate.
7. Report whether the cleanup is semantic, performance-oriented, or readability-only.

Safe mechanical cleanup candidates:

```asm
bsr.w helperReturningD0Status
tst.l d0
bmi.w fail
```

to:

```asm
bsr.w helperReturningD0Status
bmi.w fail
```

only when the helper guarantees CCR reflects `D0`.

```asm
bsr.w helperReturningD0Status
tst.l d0
bne.w fail
```

to:

```asm
bsr.w helperReturningD0Status
bne.w fail
```

only when the helper guarantees CCR reflects `D0` and the convention is zero/nonzero.

Do not apply this rewrite across calls to external OS/library routines unless the
routine's CCR behavior is known and documented. For AmigaOS/DOS calls, assume CCR
is not part of the API unless explicitly documented.

## Review checklist for native 680x0 changes

Before finishing a native Motorola 68000 assembly slice, check:

- [ ] Does every routine have `.block` / `.bend` structure?
- [ ] Are public routine register contracts clear?
- [ ] Are status conventions consistent within the helper family?
- [ ] Are direct CCR branches after calls backed by a documented helper contract?
- [ ] Are redundant `tst.l d0` instructions removed only where safe?
- [ ] Are explicit `tst` instructions kept where CCR may have been clobbered?
- [ ] Is `D0` overloading avoided or at least contained in hot VM loops?
- [ ] Are CPU-specific semantics kept out of generic layers?
- [ ] Did the native formatter/check gate run?
- [ ] Is the final report honest about remaining cleanup opportunities?

## Codex prompt for CCR cleanup work

Use this prompt when asking an implementation agent to perform a focused CCR/status cleanup:

```md
Analyze `native/motorola68000/**/*.asm` for local call-site patterns where a helper returns status in `D0` and its final instruction before `rts` sets CCR to match `D0`.

Focus only on safe, local rewrites:

- `bsr/jsr helper`
- immediately followed by `tst.l d0`
- immediately followed by `bmi/bne/beq/bpl` using that status

Replace the explicit `tst.l d0` with a direct branch only when:

1. the helper's success and failure exits both end in a CCR-setting write to `D0`,
2. no instruction between the call and branch clobbers CCR,
3. the helper contract is local or documented,
4. the branch condition matches the helper's status convention.

Do not rewrite external OS/library calls.
Do not change helper ABIs.
Do not combine this with broader register allocation or VM loop refactors.
Update helper comments where callers rely on CCR.
Run the native 68000 format gate and report exact files changed.
```
