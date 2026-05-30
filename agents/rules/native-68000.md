# Native Motorola 68000 Rule Pack

Load this rule pack only when touching supported `native/motorola68000/**/*.asm` files.

This file is written for both human maintainers and AI coding agents. The goal is
not to turn 680x0 assembly into ceremony. The goal is to make routines easy to
read, safe to change, and difficult for agents to subtly damage.

## Core principles

- Prefer clear, idiomatic 680x0 assembly over mechanically translated high-level code.
- Keep routine contracts explicit: inputs, outputs, clobbers, and CCR behavior.
- Do not duplicate information that is already obvious from the code.
- Treat public routines as ABI surfaces.
- Treat private hot-path helpers as performance-sensitive code.
- Do not add generic CPU/family/dialect-specific semantics to native VM or shared
  implementation layers.
- Validate formatting before treating native assembly work as complete.

## Formatter

- Run `scripts/workflow/run_native_68000_format_gate.sh` or
  `make native-68000-format-check` before treating formatting as complete.
- If formatting changes are required, run
  `scripts/workflow/run_native_68000_format_gate.sh --write` or
  `make native-68000-format`, then re-run the check.
- Use the root `.opforgefmt.toml` unless the user explicitly requests otherwise.

## Routine structure

- Every logical routine must be enclosed in a `.block` / `.bend` pair.
- Put `.block` on the same line as the routine label.
- Put `.bend` after the routine's final `rts`, with a trailing comment naming the routine:

```asm
routineName	.block
	; ...
	rts
	.bend  ; routineName
```

- Do not wrap ordinary branch targets or loop labels in their own `.block` unless
  they are standalone callable routines.
- Local control-flow labels belong inside the enclosing routine block.
- Group exported routines before internal helpers.
- Start the exported group with `.pub`; start helper/internal routines with `.priv`.
- Mark a symbol public only when another module intentionally consumes it.
- Treat public routines as module ABI. Public entry points must document their
  input/output register contract.
- Public routines should preserve caller-visible registers unless their contract
  explicitly says otherwise. Use balanced save/restore such as `movem.l` when needed.

## Routine documentation headers

Every public routine must have a short documentation header. Private helpers
should also have one when they are called from multiple places, return status,
touch shared VM state, or rely on non-obvious register conventions.

The header is a caller-facing contract. It should say what the caller must supply,
what the caller may read afterwards, what the routine may destroy, and whether
condition codes are meaningful on return.

### Standard public routine header

```asm
; ---------------------------------------------------------------------------
; <one-line purpose>
;
; Inputs:
; - A0: ...
; - D0: ...
;
; Outputs:
; - D0: ...
; - D3: ...
;
; Clobbers:
; - D1-D3/A0-A1/CCR
;
; CCR:
; - Unspecified on return.
; ---------------------------------------------------------------------------
routineName	.block
```

### Compact private helper header

Small private helpers may use a compact single-paragraph header:

```asm
; Pop top ExprVM stack value.
; Inputs: D7 = stack depth.
; Outputs: D0 = signed status; D3 = popped value; D7 decremented on success.
; Clobbers: D2/A2.
; CCR: reflects D0 on return.
popD3	.block
```

### Header field rules

Use these fields consistently:

- `Inputs:` registers, memory locations, module globals, or stack layout the
  caller must set before calling.
- `Outputs:` registers, memory locations, module globals, or stack changes the
  caller may depend on after return.
- `Clobbers:` registers and condition codes the caller must not expect to survive.
- `CCR:` whether condition codes are meaningful on return.

Do not list every temporary store or local branch label. The header should support
safe calling and safe editing, not exhaustively narrate the implementation.

### CCR documentation values

Use clear phrases such as:

```asm
; CCR:
; - Reflects D0 on return. Callers may branch directly on signed/zero status.
```

```asm
; CCR:
; - Reflects the final compare against the input character.
```

```asm
; CCR:
; - Unspecified on return. Callers must test the documented output explicitly.
```

```asm
; CCR:
; - Not meaningful; routine restores status through memory only.
```

## Clobber versus save/restore guidance

Do not reflexively save and restore every register. Stack traffic is real work,
especially inside VM runtimes, scanners, tokenizers, and inner helpers.

Prefer **clobbering** when:

- the routine is private to one module,
- the caller is nearby and can cheaply tolerate the destroyed registers,
- the routine is hot-path or called per opcode/token/character,
- the clobber set is small and documented,
- saving/restoring would cost more than recomputing or avoiding the live value.

Prefer **save/restore** when:

- the routine is public or module-ABI-facing,
- many callers exist or future callers are likely,
- the routine is orchestration code rather than a tight helper,
- preserving registers substantially simplifies callers,
- the routine calls out to other modules or OS/library functions,
- the register lifetime is hard to reason about at call sites.

Bad pattern:

```asm
tinyHelper	.block
	movem.l d0-d7/a0-a6, -(sp)
	; small two-register helper
	movem.l (sp)+, d0-d7/a0-a6
	rts
	.bend  ; tinyHelper
```

Better pattern for a private helper:

```asm
; Parse one byte from the local bytecode stream.
; Inputs: A0 = current bytecode pointer; D6 = remaining byte count.
; Outputs: D0 = unsigned byte; A0 advanced; D6 decremented.
; Clobbers: D0/CCR.
; CCR: reflects D6 after decrement.
readByte	.block
	; ...
	rts
	.bend  ; readByte
```

Better pattern for a public/orchestration routine:

```asm
; ---------------------------------------------------------------------------
; Run one parser VM request frame.
;
; Inputs:
; - A0: request frame pointer.
; - D0: request frame size in bytes.
;
; Outputs:
; - D0: PRVM_STATUS_*.
; - D1: result record count on success.
; - D2: final token cursor or status-specific offset.
; - D3: committed result bytes on success.
;
; Clobbers:
; - D0-D3/CCR. Other caller-visible registers are protected by the routine body.
;
; CCR:
; - Unspecified on return. Callers must test D0 explicitly.
; ---------------------------------------------------------------------------
prvmRun68000	.block
	movem.l d4-d7/a4-a6, -(sp)
	; ...
	movem.l (sp)+, d4-d7/a4-a6
	rts
	.bend  ; prvmRun68000
```

Notice that the header does not say `Saved/restored:`. The prologue and epilogue
already show it.

## Status returns and CCR

Many native routines return status in `D0`. If a routine's final success/failure
path writes `D0` with an instruction that also sets condition codes, callers can
often branch directly on the returned CCR.

For example:

```asm
pushD3	.block
	cmpi.l #EXPRVM_STACK_CAPACITY, d7
	bhs.s fail
	; ...
	moveq #0, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; pushD3
```

The `moveq` instructions set CCR, and `rts` does not destroy CCR. Therefore this
caller pattern is redundant:

```asm
bsr.w pushD3
tst.l d0
bmi.w fail
```

Prefer:

```asm
bsr.w pushD3
bmi.w fail
```

This is safe only when the callee's contract says CCR reflects `D0` on return,
or when direct inspection proves every return path ends with a CCR-setting write
to `D0` and no CCR-clobbering instruction follows.

### When to keep an explicit test

Keep `tst.l d0`, `cmpi.*`, or another explicit test when:

- the callee does not document CCR behavior,
- the callee may return through paths that do not set CCR from the status value,
- an instruction between the call and branch clobbers CCR,
- the branch is testing a semantic value rather than a status convention,
- readability benefits from an explicit comparison against a named status code,
- the call crosses a module/OS boundary whose CCR behavior should not be assumed.

Example where the explicit test is appropriate for clarity:

```asm
jsr token_util.opforgeNativeCliTokenEquals
tst.l d0
bne.w parseHelp
```

A token equality routine may well return a boolean in `D0`, but unless its ABI
states that CCR reflects the returned boolean, the caller should test explicitly.

### Avoid impossible failure checks

If a caller has already proven a helper's precondition, do not preserve redundant
failure handling unless it documents a real defensive boundary.

Example:

```asm
opcodeEnd
	cmpi.l #1, d7
	bne.w endStackFail
	bsr.w popD3
	tst.l d0
	bmi.w popFail
	moveq #0, d0
	bra.s return
```

If `popD3` only fails when `D7 == 0`, the failure path is unreachable after
`cmpi.l #1, d7` succeeds. Prefer:

```asm
opcodeEnd
	cmpi.l #1, d7
	bne.w endStackFail
	bsr.w popD3
	moveq #0, d0
	bra.s return
```

Or inline the pop only when profiling or clarity justifies exposing the stack
layout at the call site.

## `D0` discipline

`D0` is commonly used for return status. Avoid overloading it with multiple live
meanings inside the same hot loop unless doing so is clearly worthwhile.

A common smell is:

```asm
move.l d0, SomeSavedRemainingCount
bsr.w helperReturningStatusInD0
bmi.w fail
move.l SomeSavedRemainingCount, d0
```

This may be correct, but it indicates that `D0` is serving as both loop state and
helper status. In VM runtimes, consider keeping bytecode remaining count, cursor
state, or loop state in another register or in an explicit local slot, and reserve
`D0` for routine-boundary status.

Good options include:

- `A0` as current bytecode pointer plus an end pointer in another address register,
- a memory slot as authoritative remaining count,
- `D0` as status only at helper/routine boundaries,
- clearly documented helper ABIs that state which registers carry loop state.

Do not perform broad register rewrites casually. Apply this when it simplifies a
hot path or removes repeated save/restore traffic.

## Branch and comparison discipline

- Prefer the branch that matches the documented contract.
- Use `beq`/`bne` for zero/nonzero status.
- Use `bmi`/`bpl` for signed negative/nonnegative status.
- Use `bcs`/`bcc` for carry/borrow or unsigned range checks where appropriate.
- Use named status constants for public ABI status comparisons.
- Avoid mixing boolean, signed-status, and enum-status conventions in one helper.

Examples:

```asm
; signed status: 0 success, -1 failure
bsr.w helper
bmi.w fail
```

```asm
; boolean: 0 false, nonzero true
bsr.w predicate
bne.w matched
```

```asm
; enum status
bsr.w runVm
cmpi.l #PRVM_STATUS_EXPR_REQUEST, d0
beq.w handleExprRequest
```

## Hot-path helper guidance

For per-opcode, per-token, per-byte, or per-character helpers:

- keep the ABI small,
- document clobbers,
- avoid broad `movem.l`,
- avoid unnecessary `tst`/`cmp` after calls when CCR is already contractual,
- keep fall-through paths obvious,
- avoid touching module globals unless the helper contract says so.

Hot helpers are allowed to be less defensive when their callers already checked
preconditions locally.

## Public ABI guidance

For public routines:

- document the register contract in the header,
- preserve non-output registers unless the contract explicitly allows clobbering,
- return named status constants where possible,
- do not rely on callers knowing private implementation details,
- use explicit tests at module boundaries unless CCR behavior is part of the ABI,
- keep output register meanings stable.

## CPU-specific architecture boundary

Generic opForge Rust VM, Native VM, workflow, and CLI implementation paths must
not grow CPU/family/dialect/register/addressing-mode/instruction-specific logic.

CPU-specific behavior belongs in package VM definitions, family/dialect packages,
fixtures, examples, tests, or documentation.

When touching native Motorola 68000 assembly, distinguish between:

- legitimate 68000 implementation syntax and register use inside
  `native/motorola68000/**`, and
- accidental leakage of 6502/8080/etc. concepts into generic opForge logic.

The architecture-boundary guard intentionally does not treat normal assembler
instruction syntax as a violation. It does inspect implementation-owned
identifiers and metadata such as labels, macro names, constant names, and module
or section names.

## Comments

Use comments to clarify intent, invariants, ABI contracts, and non-obvious
680x0 choices. Avoid comments that simply restate the instruction.

Good:

```asm
; D7 is stack depth, not byte offset.
lsl.l #2, d2
```

Bad:

```asm
; Shift D2 left by 2.
lsl.l #2, d2
```

When a caller depends on CCR from a callee, make that dependency visible either
through the callee header or a short local comment.

## Checklist for native 68000 changes

Before treating native 68000 assembly work as complete:

- Every touched public routine has a current routine header.
- Touched private multi-call helpers have at least a compact header.
- Headers document inputs, outputs, clobbers, and CCR behavior.
- Headers do not include routine `Saved/restored:` or default `Preserves:` fields.
- Public ABI routines preserve non-output registers unless explicitly documented.
- Private hot helpers do not perform unnecessary broad save/restore.
- Callers do not use `tst.l d0` immediately after a helper when CCR already
  reflects the documented returned status.
- Explicit tests remain where CCR is undocumented, clobbered, semantic, or clearer.
- No CPU/family/dialect-specific semantics were added to generic layers.
- Native formatter check passes.

## Codex prompt fragment for CCR/status cleanup

Use this fragment when asking an implementation agent to clean up native 68000
status-return call sites:

```md
Inspect touched `native/motorola68000/**/*.asm` routines for redundant status
tests after `bsr/jsr` calls.

For each candidate:

1. Confirm the callee's documented CCR contract, or inspect all return paths.
2. Only replace `bsr/jsr; tst.l d0; bmi/beq/bne/...` when CCR on return is proven
   to reflect the value being branched on.
3. Do not rewrite enum-status comparisons where explicit `cmpi` is clearer.
4. Do not cross OS/library/module boundaries unless the callee ABI documents CCR.
5. Update or add routine headers for touched routines:
   - Inputs
   - Outputs
   - Clobbers
   - CCR
6. Do not add `Saved/restored:` or default `Preserves:` fields.
7. Run the native 68000 formatter/check gate.
8. Report every changed call site and why it was safe.
```
