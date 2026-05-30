# opForge Native 680x0 Status/CCR Cleanup Analysis

Date: 2026-05-30  
Repository inspected: `pig-games/opForge` remote  
Primary area: `native/motorola68000/amigaos`  
Primary example: `native/motorola68000/amigaos/exprvm/exprvm_runtime.asm`

## 1. Summary

The native 680x0 implementation currently contains many instances of this idiom:

```asm
bsr.w someHelper
tst.l d0
bmi.w fail
```

or:

```asm
jsr someHelper
tst.l d0
bne.w fail
```

This is often functionally correct, but in many cases it is unnecessarily conservative on 680x0 because the helper itself already sets the condition codes when it writes its return value to `d0`.

For helper routines that end with status-setting instructions such as:

```asm
moveq #0, d0
rts
```

or:

```asm
moveq #-1, d0
rts
```

the caller can normally branch directly after the subroutine call:

```asm
bsr.w someHelper
bmi.w fail
```

The `tst.l d0` repeats a status test that has already happened.

This is especially interesting for the native VM code because:

- VM dispatch paths are hot.
- Small 680x0 instruction savings compound over many expressions, tokens, and parser steps.
- Several current routines already use a clean “return status in `d0`” convention.
- The code has places where stack-depth or frame preconditions already prove that a helper cannot fail, making both `tst.l d0` and the following error branch redundant.

The issue is not just “remove `tst.l d0` everywhere.” Some `tst` instructions are meaningful semantic checks, and some calls return non-negative status codes where testing zero/nonzero is clearer than relying on flags. The improvement should be done with explicit rules.

## 2. The concrete EXPRVM example

Current pattern:

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

Relevant helper:

```asm
popD3    .block
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

There are two separate optimization observations here.

### 2.1 The `tst.l d0` is redundant after the helper

Both success and failure paths in `popD3` end by writing to `d0`:

```asm
moveq #0, d0
rts
```

or:

```asm
moveq #-1, d0
rts
```

On 680x0, `moveq` updates the condition code register. `moveq #0,d0` clears N and sets Z. `moveq #-1,d0` sets N and clears Z. `rts` does not destroy those flags.

Therefore this:

```asm
bsr.w popD3
tst.l d0
bmi.w popFail
```

can become:

```asm
bsr.w popD3
bmi.w popFail
```

provided that `popD3` continues to end its return paths with CCR-setting writes to `d0`.

### 2.2 The failure branch is also redundant in this specific case

`opcodeEnd` first checks:

```asm
cmpi.l #1, d7
bne.w endStackFail
```

So when `popD3` is called, `d7 == 1`.

But `popD3` can only fail if `d7 == 0`:

```asm
tst.l d7
beq.s fail
```

Therefore, in this exact site, the call cannot fail under normal single-threaded VM execution. This can be simplified further:

```asm
opcodeEnd
    cmpi.l #1, d7
    bne.w endStackFail
    bsr.w popD3
    moveq #0, d0
    bra.s return
```

or fully inlined:

```asm
opcodeEnd
    cmpi.l #1, d7
    bne.w endStackFail
    subq.l #1, d7
    lea ExprvmStack, a2
    move.l (a2), d3
    moveq #0, d0
    bra.s return
```

The first version keeps stack access encapsulated. The second saves the subroutine call as well, but duplicates stack layout knowledge. For a VM hot path, inlining may be justified; for maintainability, the helper call is still fine.

## 3. The 680x0 condition code principle

The useful rule is:

> If a subroutine returns by executing a CCR-setting instruction that places its status/result in `d0`, and no later instruction before `rts` modifies CCR, the caller may branch directly on the flags after `bsr`/`jsr`.

Typical status-setting return instructions include:

```asm
moveq #0, d0
moveq #1, d0
moveq #-1, d0
clr.l d0
move.l source, d0
subq.l #1, d0
addq.l #1, d0
ext.l d0
```

`rts` itself does not perform an arithmetic/logical operation and does not reset the flags.

So these are normally equivalent:

```asm
bsr.w helper
tst.l d0
bmi.w fail
```

```asm
bsr.w helper
bmi.w fail
```

and:

```asm
bsr.w helper
tst.l d0
bne.w fail
```

```asm
bsr.w helper
bne.w fail
```

provided the helper’s return convention is stable.

## 4. Where this applies cleanly in `exprvm_runtime.asm`

The EXPRVM runtime has several helpers with clear status-in-`d0` returns.

### 4.1 `pushD3`

```asm
pushD3    .block
    cmpi.l #EXPRVM_STACK_CAPACITY, d7
    bhs.s fail
    move.l d7, d2
    lsl.l #2, d2
    lea ExprvmStack, a2
    move.l d3, 0(a2, d2.l)
    addq.l #1, d7
    moveq #0, d0
    rts

fail
    moveq #-1, d0
    rts
    .bend  ; pushD3
```

Safe caller rewrite:

```asm
bsr.w pushD3
tst.l d0
bmi.w fail
```

to:

```asm
bsr.w pushD3
bmi.w fail
```

### 4.2 `popD3`

As above, success ends in `moveq #0,d0`, failure ends in `moveq #-1,d0`.

Safe caller rewrite:

```asm
bsr.w popD3
tst.l d0
bmi.w fail
```

to:

```asm
bsr.w popD3
bmi.w fail
```

### 4.3 `readU8`, `readU16`, `readI64Low32`

These are slightly more nuanced.

For example:

```asm
readU8    .block
    tst.l d0
    beq.s fail
    moveq #0, d3
    move.b (a0)+, d3
    subq.l #1, d0
    rts

fail
    moveq #-1, d0
    rts
    .bend  ; readU8
```

Here `d0` is not merely status. On success it is the remaining byte count. On failure it is `-1`.

The caller currently does:

```asm
bsr.w readU8
tst.l d0
bmi.w fail
```

This can still become:

```asm
bsr.w readU8
bmi.w fail
```

because both the success path and failure path leave condition codes valid:

- success: `subq.l #1,d0` sets flags based on the new non-negative remaining count;
- failure: `moveq #-1,d0` sets N.

However, this is a less clean ABI than the stack helpers because `d0` is overloaded as both:

- remaining bytecode count,
- helper status sentinel,
- final VM return status.

That overloading causes additional save/restore traffic elsewhere.

## 5. The larger EXPRVM design smell: `d0` is overloaded

The current EXPRVM loop uses `d0` as the bytecode remaining count. But helper routines also use `d0` as a status return register, and the top-level evaluator also returns final status in `d0`.

This forces patterns like:

```asm
move.l d0, ExprvmEvalRemaining
bsr.w pushD3
tst.l d0
bmi.w fail
move.l ExprvmEvalRemaining, d0
bra.w evalLoop
```

This appears around literal push, current address push, symbol push, unary operator handling, and binary operator handling.

The `tst.l d0` is a small cost. The larger cost is:

```asm
move.l d0, ExprvmEvalRemaining
...
move.l ExprvmEvalRemaining, d0
```

This is a sign that the register contract is fighting the VM structure.

### Recommended EXPRVM direction

Make the bytecode cursor state independent of `d0`.

Better options:

#### Option A: Keep remaining byte count in memory

Use `ExprvmEvalRemaining` as the authoritative count during execution:

```asm
; A0 = current bytecode pointer
; ExprvmEvalRemaining = remaining byte count
; D0 = helper/top-level status only
```

Helpers update `ExprvmEvalRemaining` directly or use a dedicated scratch register.

Pros:

- Minimal register pressure changes.
- Keeps `d0` clean for status.
- Reduces save/restore around push/pop helpers.

Cons:

- Memory traffic remains.
- Bytecode readers become less register-only.

#### Option B: Use end pointer instead of remaining byte count

At function entry:

```asm
; A0 = current bytecode pointer
; A1/A6/etc = bytecode end pointer
```

Then readers check:

```asm
cmpa.l bytecodeEnd, a0
bhs.s fail
```

For multi-byte reads:

```asm
movea.l a0, scratchA
adda.l #8, scratchA
cmpa.l bytecodeEnd, scratchA
bhi.s fail
```

Pros:

- Natural VM cursor model.
- Removes repeated decrementing of a remaining counter.
- Lets `d0` be status/result only.
- Often improves readability of bytecode reading.

Cons:

- Requires reserving an address register.
- Need to audit current address register roles.
- Existing symbol tables and frame pointers already use several address registers.

#### Option C: Use a dedicated data register for remaining length

For example:

```asm
; D6 = remaining byte count
; D0 = status/result
```

Pros:

- Fast register-only loop.
- Keeps bytecode reader operations simple.

Cons:

- EXPRVM currently uses `d6` for opcode/operator scratch.
- PRVM and TKVM may already have their own register conventions.
- Could increase register pressure and movem saves.

For the current EXPRVM shape, Option B is probably the cleanest long-term VM-native design. Option A is probably the smallest low-risk refactor.

## 6. Similar issue class across the native tree

A repo search for the idiom `bsr.w` / `tst.l d0` shows this pattern is not isolated to EXPRVM. It appears across files including:

- `native/motorola68000/amigaos/opforge-cli/args.asm`
- `native/motorola68000/amigaos/exprvm/exprvm_runtime.asm`
- `native/motorola68000/amigaos/prvm/prvm_line_router.asm`
- `native/motorola68000/amigaos/prvm/prvm_line_iterator.asm`
- `native/motorola68000/amigaos/opforge-cli/prvm_bridge.asm`
- `native/motorola68000/amigaos/opcore/opcore_expr_bridge.asm`
- `native/motorola68000/amigaos/opforge-cli/source_reader.asm`
- `native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm`
- `native/motorola68000/amigaos/opforge-cli/line_text.asm`
- `native/motorola68000/amigaos/opforge-cli/line_processor.asm`
- `native/motorola68000/amigaos/opforge-cli/include_use.asm`
- `native/motorola68000/amigaos/opforge-cli/package_pipeline.asm`
- `native/motorola68000/amigaos/opforge-cli/directive_handlers.asm`
- `native/motorola68000/amigaos/opforge-cli/module_use.asm`
- `native/motorola68000/amigaos/prvm/prvm_runtime.asm`
- `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
- multiple test harness and generated/listing reference files

This should be treated as a repo-wide idiom review, not as a one-off micro-optimization.

## 7. Important distinction: redundant CCR refresh vs semantic test

Do not remove every `tst.l d0`.

There are at least three categories.

### 7.1 Category A: redundant CCR refresh after helper status return

Example:

```asm
bsr.w helper
tst.l d0
bmi.w fail
```

If `helper` returns through `moveq #-1,d0` / `moveq #0,d0`, this should become:

```asm
bsr.w helper
bmi.w fail
```

This is the cleanest class.

### 7.2 Category B: semantic zero/nonzero test after external call

Example from argument parsing:

```asm
jsr token_util.opforgeNativeCliTokenEquals
tst.l d0
bne.w parseHelp
```

This may be mechanically reducible if `opforgeNativeCliTokenEquals` is known to return through CCR-setting writes to `d0`. But semantically it reads as:

> Did the token match?

In such cases, leaving `tst.l d0` may be clearer unless the helper ABI is explicitly documented as “CCR-preserving return status.”

For non-hot CLI parsing, readability may matter more than saving one instruction.

### 7.3 Category C: tests after non-status data movement

Example:

```asm
move.l PRVM_FRAME_SOURCE_PTR(a4), d0
tst.l d0
beq invalidArgument
```

This is not a redundant post-call test. It is checking a loaded pointer value. It should stay unless rewritten into an instruction sequence that naturally sets equivalent flags and is clearer.

A possible alternative is:

```asm
move.l PRVM_FRAME_SOURCE_PTR(a4), d0
beq invalidArgument
```

because `move.l` sets flags. But that can be visually surprising when validating pointers. This is more a style choice than an urgent optimization.

## 8. Suggested repo-wide return convention

Document a native 680x0 convention like this:

### 8.1 Status-returning helpers

For internal native helpers that return a status in `d0`:

- `d0 == 0`: success
- `d0 > 0`: non-fatal / specific status, where applicable
- `d0 < 0`: internal helper failure, where applicable
- The helper must return with CCR reflecting `d0`.
- The final instruction before each `rts` should normally be the write to `d0`, or an instruction that intentionally sets CCR based on `d0`.
- Callers may branch directly on CCR after `bsr` / `jsr`.

Example:

```asm
someHelper
    ...
success
    moveq #0, d0
    rts

fail
    moveq #-1, d0
    rts
```

Caller:

```asm
bsr.w someHelper
bmi.w fail
```

### 8.2 Predicate-returning helpers

For helpers that return boolean truth in `d0`:

- `d0 == 0`: false
- `d0 != 0`: true
- CCR reflects `d0`
- Callers may use `beq` / `bne` directly after the call

Example:

```asm
jsr token_util.opforgeNativeCliTokenEquals
bne.w matched
```

However, for readability, using `tst.l d0` may still be allowed in non-hot code if the predicate nature is not obvious.

### 8.3 Multi-code status helpers

For helpers that return multiple positive status codes:

```asm
; 0 = ok
; 1 = missing
; 2 = capacity
; -1 = quoted/error
```

Use explicit compares when the distinction matters:

```asm
bsr.w copyRequiredPath
cmpi.l #1, d0
beq.w missingValue
cmpi.l #2, d0
beq.w capacity
bmi.w quoted
```

Avoid converting this into overly clever branch-on-flags unless it remains obvious.

## 9. Rewrite rules

### 9.1 Safe mechanical rewrite: remove redundant `tst` after known helper

Pattern:

```asm
bsr.w helper
tst.l d0
bmi.w fail
```

Conditions:

- `helper` is internal or otherwise verified.
- All return paths from `helper` set `d0` using a CCR-setting instruction.
- No CCR-clobbering instruction occurs between the final `d0` write and `rts`.
- The branch uses the same condition that the final `d0` write establishes.

Rewrite:

```asm
bsr.w helper
bmi.w fail
```

Similarly:

```asm
bsr.w helper
tst.l d0
bne.w fail
```

to:

```asm
bsr.w helper
bne.w fail
```

### 9.2 Safe local proof rewrite: remove impossible helper-failure branch

Pattern:

```asm
cmpi.l #1, d7
bne.w endStackFail
bsr.w popD3
tst.l d0
bmi.w popFail
```

If the helper can only fail when `d7 == 0`, and the caller just proved `d7 == 1`, rewrite to:

```asm
cmpi.l #1, d7
bne.w endStackFail
bsr.w popD3
```

Potentially then inline `popD3` if the site is performance-sensitive.

### 9.3 Do not mechanically rewrite unknown external calls

Pattern:

```asm
jsr external.module.fn
tst.l d0
bne.w fail
```

Only rewrite if the external function’s ABI is documented and stable.

This matters because not all subroutines are written with CCR return semantics in mind. A routine could theoretically do:

```asm
moveq #0, d0
move.l (sp)+, a0    ; may or may not affect CCR depending instruction
rts
```

or perform another instruction after writing `d0` that changes flags. In such cases, direct branching after the call would be wrong.

## 10. Static quality check proposal

Add a deterministic script that reports possible redundant `tst.l d0` after subroutine calls.

This should initially be advisory, not failing, because many matches require ABI knowledge.

### 10.1 First-pass grep-level detector

Search for:

```text
bsr.*\n\s*tst\.l\s+d0
jsr.*\n\s*tst\.l\s+d0
```

Limit to:

```text
native/motorola68000/amigaos/**/*.asm
```

Exclude:

```text
examples/reference/**/*.lst
```

because listing/reference files are generated artifacts or expected outputs.

### 10.2 Better detector

A small script can parse line windows:

- detect `bsr` / `jsr`
- skip blank/comment-only lines if desired
- detect immediate `tst.l d0`
- classify next branch:
  - `bmi` / `bpl`: negative status
  - `beq` / `bne`: zero/nonzero predicate
  - other: manual review
- emit file, line, callee, branch condition, and suggested rewrite

Example output:

```text
native/motorola68000/amigaos/exprvm/exprvm_runtime.asm:108
  bsr.w readI64Low32
  tst.l d0
  bmi.w literalReadFail

  Candidate:
  bsr.w readI64Low32
  bmi.w literalReadFail

  Confidence: high, helper is local and returns via CCR-setting d0 writes.
```

### 10.3 Optional helper ABI analyzer

For local labels in the same file:

- find helper block label
- inspect paths ending in `rts`
- check the previous meaningful instruction before each `rts`
- mark helper as CCR-return-safe if previous meaningful instruction writes `d0` with a CCR-setting instruction

Recognized safe final instructions:

```text
moveq #imm,d0
clr.l d0
clr.w d0
clr.b d0
move.l ...,d0
move.w ...,d0
move.b ...,d0
ext.l d0
ext.w d0
addq.*,d0
subq.*,d0
```

Recognized unsafe/unknown before `rts`:

```text
movem.l ...
lea ...
movea.l ...
adda/suba ...
pea
link/unlk
nop
```

Note: some instructions do not affect CCR, and preserving prior CCR may still be safe. But the analyzer should be conservative.

## 11. Suggested phased implementation plan

### Phase 1: EXPRVM surgical cleanup

Target:

```text
native/motorola68000/amigaos/exprvm/exprvm_runtime.asm
```

Actions:

1. Replace `bsr.w read*` + `tst.l d0` + `bmi` with direct `bmi`.
2. Replace `bsr.w pushD3` + `tst.l d0` + `bmi` with direct `bmi`.
3. Replace `bsr.w popD3` + `tst.l d0` + `bmi` with direct `bmi`.
4. In `opcodeEnd`, remove the impossible pop failure check after `d7 == 1` has already been verified.
5. Run existing native/reference tests.

Expected effect:

- Smaller code.
- Fewer instructions in the EXPRVM hot path.
- No semantic change.

### Phase 2: Document native status ABI

Add or update native coding guidelines:

```text
docs/native/motorola68000-status-return-conventions.md
```

or equivalent.

Document:

- `d0` status/result convention.
- CCR-after-return convention.
- When direct branch after `bsr/jsr` is allowed.
- When explicit `tst.l d0` should remain for readability or external ABI uncertainty.

### Phase 3: Advisory quality script

Add a script such as:

```text
scripts/check_68k_redundant_tst_d0.py
```

or integrate into the existing quality script layout.

Initial behavior:

- report candidates
- do not fail CI

Later behavior:

- fail only in hot VM directories once convention is adopted:
  - `native/motorola68000/amigaos/exprvm`
  - `native/motorola68000/amigaos/prvm`
  - `native/motorola68000/amigaos/tkvm`
  - possibly `native/motorola68000/amigaos/tkpkg`

### Phase 4: Repo-wide review

Review matches in:

- CLI parsing
- line processing
- package pipeline
- PRVM bridge
- opcore expression bridge
- source reader
- opasm assembly driver

Classify each as:

- hot path: prefer direct branch
- non-hot but obvious internal helper: direct branch optional
- external/unclear ABI: keep `tst`
- semantic pointer/value validation: keep or rewrite only if style supports it

### Phase 5: EXPRVM register/ABI cleanup

After the small cleanup, consider a deeper EXPRVM refactor:

- remove `d0` as the live bytecode remaining counter
- reserve `d0` for status/result
- use bytecode end pointer or explicit memory slot for remaining length
- remove repeated `ExprvmEvalRemaining` save/restore around stack helpers

This is the bigger performance/clarity win.

## 12. Codex implementation prompt

```text
Analyse the current pig-games/opForge repository, focusing on native/motorola68000/amigaos.

We have identified a 680x0-native cleanup opportunity: many call sites use:

    bsr.w helper
    tst.l d0
    bmi/bne/... target

even when the helper returns by writing its status/result to d0 using an instruction that already sets CCR, and rts preserves those flags. In those cases the tst.l d0 is redundant and the caller can branch directly after bsr/jsr.

Please implement a conservative first slice in native/motorola68000/amigaos/exprvm/exprvm_runtime.asm only.

Rules:
1. For local helpers pushD3, popD3, readU8, readU16, and readI64Low32, verify all return paths leave CCR reflecting d0.
2. Replace immediate patterns of:
       bsr.w <verified-helper>
       tst.l d0
       bmi.<size> <target>
   with:
       bsr.w <verified-helper>
       bmi.<size> <target>
3. In opcodeEnd, note that cmpi.l #1,d7 already proves popD3 cannot fail, because popD3 only fails when d7 == 0. Remove the redundant post-pop failure check there while preserving final success return behavior.
4. Do not modify other files in this slice.
5. Do not rewrite jsr calls or non-local helpers in this slice.
6. Do not change labels, public ABI names, or generated/reference output files unless the existing test workflow requires it.
7. Run the existing quality/test workflow and report results.

Expected outcome:
- Smaller EXPRVM runtime.
- No semantic change.
- Direct 680x0 CCR-aware branch usage after verified local status helpers.

Commit message:
Title: Optimize ExprVM status checks
Summary:
- Removed redundant tst.l d0 instructions after verified local ExprVM helpers that already return with CCR reflecting d0.
- Simplified opcodeEnd by dropping an impossible pop failure check after stack depth is proven to be one.
- Preserved existing ExprVM status ABI and behavior.
```

## 13. Recommended code style note

A useful comment near helper definitions:

```asm
; Helper status convention:
; - D0 = 0 on success.
; - D0 < 0 on local helper failure.
; - Return paths leave CCR reflecting D0.
;   Callers may branch directly after BSR/JSR.
```

For predicate helpers:

```asm
; Predicate convention:
; - D0 = 0 when false/no match.
; - D0 != 0 when true/match.
; - Return paths leave CCR reflecting D0.
```

This makes direct post-call branches self-documenting rather than clever.

## 14. Bottom line

The immediate `tst.l d0` after helpers like `popD3` is not wrong, but it is often unnecessary on 680x0. The helper already writes the result into `d0`, that write already sets the flags, and `rts` preserves those flags for the caller.

The highest-confidence first improvement is inside `exprvm_runtime.asm`, especially around local helpers with clear `moveq #0,d0` / `moveq #-1,d0` return paths.

The larger architectural improvement is to stop using `d0` simultaneously as:

- bytecode remaining count,
- helper status,
- final VM status.

That register-contract cleanup will probably matter more than the individual removed `tst.l` instructions, but the `tst.l` cleanup is an excellent low-risk first slice and a good way to make the native code more idiomatic for 680x0.
