; Compile bounded expression tokens once; evaluate through the shared ExprVM.
; @opforge-owner: opasm.amigaos.binary_expression
	.module opasm.amigaos.binary_expression
	.cpu 68020
	.use exprvm.amigaos.runtime as runtime
	.use opasm.amigaos.binary_fold as folder
	.use experimental.amigaos.binary_exvm as compiler
	.use experimental.amigaos.binary_exvm_lower as lowering
	.use exprvm.amigaos.values_runtime as typed
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_record_values as records
	.use experimental.amigaos.binary_struct_values as structs
	.use experimental.amigaos.binary_numeric_bindings as bindings
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
	.include "telemetry_macros.i"
	.include "memory_telemetry.i"
	.pub
COMPILED_TAG = $81
VALUE_TAG = $83
COMPOUND_BIT = 7
MAX_DEPTH = compiler.MAX_DEPTH
ARENA_BYTES = lowering.NODE_LIMIT*compiler.NODE_BYTES
ARENA = compiler.REQUEST_BYTES
COMPILER_WORK = ARENA+ARENA_BYTES
WORKSPACE_BYTES = COMPILER_WORK+compiler.SCRATCH_BYTES
STEP_BUDGET = 65536
STATUS_OK = 0
STATUS_MALFORMED = 1
STATUS_OUTPUT = 2
STATUS_DEPTH = 3
Frame	.struct
Values	.long ?
Defined	.long ?
Count	.long ?
Pc	.long ?
High	.long ?
Owner	.long ?
Kind	.long ?
SectionIds	.long ?
CurrentSection	.word ?
Reserved	.word ?
Provenance	.long ?
	.endstruct
FRAME_BYTES = Frame.Provenance+4
	.section code, kind=code
	.pub
; Install validated package grammar and session-owned transient workspace.
; A0/D0=program/bytes, A1/D1=workspace/bytes. D0/CCR=0; others preserved.
; Clearing all inputs invalidates the session; no pointers outlive its owner.
configure	.block
	move.l a0, Program
	move.l d0, ProgramBytes
	move.l a1, Workspace
	move.l d1, WorkspaceBytes
	clr.w DeferredCompile
	moveq #0, d0
	rts
	.bend  ; configure

; D0=zero normal folding,nonzero defer evaluation for this preparation line.
; Session-exclusive policy also covers nested/product compiler consumers.
; D0/CCR=0; other registers preserved. Caller resets on every exit path.
deferCompile	.block
	move.w d0, DeferredCompile
	moveq #0, d0
	rts
	.bend  ; deferCompile

; D0=validated package .len numeric name ID, or -1 when clearing a session.
; Preserves other registers; D0/CCR=0. No lexical name reaches evaluation.
configureBuiltin	.block
	move.l d0, LenName
	moveq #0, d0
	rts
	.bend  ; configureBuiltin

; D1=numeric name ID. D0/CCR=0 only for the configured builtin call name.
; Other registers preserved; syntax and call placement remain compiler-owned.
isBuiltin	.block
	cmp.l LenName, d1
	bne.w unknown
	tst.l d1
	bmi.w unknown
	moveq #0, d0
	rts
unknown
	moveq #1, d0
	rts
	.bend  ; isBuiltin

; A0/A1=bounded raw tokens,A2=bare-member callback,A6=callback context.
; Callback: A0=context,D0=source ID => D0=status,D1=global member ID;
; preserves D2-D7/A0-A6. D0/CCR=status; other registers preserved.
; NAME+OPEN_BRACE only selects candidates. The shared grammar must accept
; the candidate before its FIELD spans annotate exact source tokens. No other
; source names acquire a field role, and failed parses never annotate keys.
BIND_TOKEN_BYTES = source.MAX_LINE
bindFields	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.l #BIND_TOKEN_BYTES, sp
	move.l a6, d6
	movea.l a0, a5
	movea.l a1, a4
	movea.l a2, a6
scan
	cmpa.l a4, a5
	beq.w good
	bhi.w bad
	moveq #0, d0
	move.b (a5), d0
	cmpi.b #1, d0
	bhi.w advance
	move.l a4, d0
	sub.l a5, d0
	cmpi.l #source.NAME_TOKEN_BYTES+1, d0
	blo.w advance
	cmpi.b #source.TOKEN_OPEN_BRACE, source.NAME_TOKEN_BYTES(a5)
	bne.w advance
	cmpi.l #BIND_TOKEN_BYTES, d0
	bhi.w bad
	movea.l sp, a1
	movea.l a5, a0
	move.l d0, d7
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
	movea.l sp, a0
	movea.l sp, a2
	adda.l d7, a2
normalize
	cmpa.l a2, a0
	beq.w parse
	bhi.w bad
	cmpi.b #1, (a0)
	bhi.w normalizeNext
	move.l a2, d0
	sub.l a0, d0
	cmpi.l #source.NAME_TOKEN_BYTES, d0
	blo.w bad
	cmpi.b #source.FIELD_KEY_QUALIFIER, source.NameToken.Qualifier(a0)
	beq.w normalizeNext
	clr.b source.NameToken.Qualifier(a0)
normalizeNext
	movea.l a2, a1
	bsr.w rawTokenEnd
	bne.w bad
	bra.w normalize
parse
	movea.l Workspace, a3
	move.l a3, d0
	beq.w bad
	cmpi.l #WORKSPACE_BYTES, WorkspaceBytes
	blo.w bad
	move.l sp, compiler.Request.Tokens(a3)
	move.l d7, compiler.Request.TokenBytes(a3)
	move.l Program, compiler.Request.Program(a3)
	move.l ProgramBytes, compiler.Request.ProgramBytes(a3)
	lea ARENA(a3), a0
	move.l a0, compiler.Request.Arena(a3)
	move.l #ARENA_BYTES, compiler.Request.ArenaBytes(a3)
	move.l #STEP_BUDGET, compiler.Request.StepBudget(a3)
	lea COMPILER_WORK(a3), a0
	move.l a0, compiler.Request.Scratch(a3)
	move.l #compiler.SCRATCH_BYTES, compiler.Request.ScratchBytes(a3)
	movea.l a3, a0
	jsr compiler.compile
	tst.l d0
	bne.w advance
	move.l compiler.Request.Consumed(a3), d5
	move.l compiler.Request.Used(a3), d7
	movea.l compiler.Request.Arena(a3), a3
field
	tst.l d7
	beq.w candidateDone
	cmpi.b #compiler.KIND_FIELD, compiler.Node.Kind(a3)
	bne.w nextField
	moveq #0, d4
	move.w compiler.Node.SpanStart(a3), d4
	movea.l a5, a2
	adda.l d4, a2
	move.l a4, d0
	sub.l a2, d0
	cmpi.l #source.NAME_TOKEN_BYTES, d0
	blo.w bad
	cmpi.b #source.FIELD_KEY_QUALIFIER, source.NameToken.Qualifier(a2)
	beq.w nextField
	; Qualification is normalized only for the speculative grammar copy.
	; A literal key must retain its bare identity rather than its last component.
	tst.b source.NameToken.Qualifier(a2)
	bne.w bad
	move.l compiler.Node.First(a3), d0
	movea.l d6, a0
	jsr (a6)
	tst.l d0
	bne.w bad
	move.w d1, source.NameToken.Id(a2)
	move.b #source.FIELD_KEY_QUALIFIER, source.NameToken.Qualifier(a2)
nextField
	adda.l #compiler.NODE_BYTES, a3
	subi.l #compiler.NODE_BYTES, d7
	bra.w field
candidateDone
	tst.l d5
	beq.w bad
	adda.l d5, a5
	bra.w scan
advance
	movea.l a5, a0
	movea.l a4, a1
	bsr.w rawTokenEnd
	bne.w bad
	movea.l a0, a5
	bra.w scan
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	adda.l #BIND_TOKEN_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; bindFields
	.priv
; A0/A1=bounded raw token/end. D0/CCR=status,A0=next token; D1 scratch.
rawTokenEnd	.block
	moveq #0, d0
	move.b (a0), d0
	moveq #1, d1
	cmpi.b #1, d0
	bls.w name
	cmpi.b #2, d0
	beq.w number
	cmpi.b #source.TOKEN_WIDE_NUMBER, d0
	beq.w wide
	cmpi.b #7, d0
	beq.w number
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d0
	beq.w number
	cmpi.b #3, d0
	beq.w bytes
	cmpi.b #COMPILED_TAG, d0
	beq.w bytes
	cmpi.b #VALUE_TAG, d0
	beq.w bytes
	bra.w bounded
name
	moveq #source.NAME_TOKEN_BYTES, d1
	bra.w bounded
number
	moveq #5, d1
	bra.w bounded
wide
	moveq #9, d1
	bra.w bounded
bytes
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d1
	move.b 1(a0), d1
	addq.l #2, d1
bounded
	move.l a1, d0
	sub.l a0, d0
	cmp.l d0, d1
	bhi.w bad
	adda.l d1, a0
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; rawTokenEnd
	.pub

; A0=input tokens, A1=bounded end, A3=output, A4=bounded output end.
; Returns D0/CCR=status, A0=first delimiter/end, A3=after compiled wrapper.
; Preserves D1-D7/A1-A2/A4-A6. Failed output is uncommitted scratch.
; Wrapper: $81,u8 payload length, compact runtime expression (LE payloads).
; Installed workspace is transient and exclusive to the active session.
; Shared EXVM owns grammar, the offset arena is transient, and only compact
; evaluator bytes persist. Compiler/lowerer enforce independent syntax, arena,
; program-step, output and evaluator-stack bounds. No source text is read.
compile	.block
	moveq #0, d0
	bra.w compileMode
	.bend  ; compile

; Check the same syntax and lower operators, without constant evaluation.
compileUnfolded	.block
	moveq #1, d0
	bra.w compileMode
	.bend  ; compileUnfolded

; Force canonical typed lowering when a bound symbol may carry a compound value.
; Same ABI as compile; scalar arithmetic is then checked by the typed runtime.
compileValue	.block
	moveq #2, d0
	bra.w compileMode
	.bend  ; compileValue

; Typed lowering with all scalar constant evaluation deferred.
compileValueUnfolded	.block
	moveq #3, d0
	bra.w compileMode
	.bend  ; compileValueUnfolded
	.priv
compileMode	.block
	movem.l d1-d7/a1-a2/a4-a6, -(sp)
	movea.l Workspace, a2
	move.l a2, d1
	beq.w unavailable
	btst #0, d1
	bne.w unavailable
	move.l WorkspaceBytes, d1
	cmpi.l #WORKSPACE_BYTES, d1
	blo.w unavailable
	add.l a2, d1
	bcs.w unavailable
	move.l d0, d4
	tst.w DeferredCompile
	beq.w modeReady
	bset #0, d4
modeReady
	movea.l a0, a5
	movea.l a3, a6
	clr.l compiler.Request.Consumed(a2)
	move.l a4, d0
	sub.l a3, d0
	bcs.w output
	cmpi.l #3, d0
	blo.w output
	move.l a1, d1
	sub.l a0, d1
	bcs.w malformed
	move.l a0, compiler.Request.Tokens(a2)
	move.l d1, compiler.Request.TokenBytes(a2)
	move.l Program, compiler.Request.Program(a2)
	move.l ProgramBytes, compiler.Request.ProgramBytes(a2)
	lea ARENA(a2), a0
	move.l a0, compiler.Request.Arena(a2)
	move.l #ARENA_BYTES, compiler.Request.ArenaBytes(a2)
	move.l #STEP_BUDGET, compiler.Request.StepBudget(a2)
	lea COMPILER_WORK(a2), a0
	move.l a0, compiler.Request.Scratch(a2)
	move.l #compiler.SCRATCH_BYTES, compiler.Request.ScratchBytes(a2)
	movea.l a2, a0
	jsr compiler.compile
	tst.l d0
	beq.w lower
	cmpi.l #compiler.STATUS_DEPTH, d0
	beq.w depth
	cmpi.l #compiler.STATUS_STEPS, d0
	beq.w depth
	bra.w malformed
lower
	; The canonical payload must fit the existing one-byte wrapper before
	; folding. Limiting scratch output preserves the previous compiler bound.
	move.l a6, d0
	addi.l #257, d0
	bcs.w output
	cmp.l a4, d0
	bhi.w bounded
	movea.l d0, a4
bounded
	lea 2(a6), a3
	movea.l compiler.Request.Arena(a2), a0
	move.l compiler.Request.Used(a2), d0
	move.l compiler.Request.Root(a2), d1
	lea COMPILER_WORK(a2), a1
	move.l #compiler.SCRATCH_BYTES, d2
	btst #1, d4
	bne.w lowerValue
	jsr lowering.lower
	tst.l d0
	beq.w prepare
	cmpi.l #lowering.STATUS_MALFORMED, d0
	bne.w lowerFailed
lowerValue
	; Value selection and compound trees use canonical typed operations;
	; these preserve cell kind/provenance and bypass scalar constant folding.
	lea 2(a6), a3
	movea.l compiler.Request.Arena(a2), a0
	move.l compiler.Request.Used(a2), d0
	move.l compiler.Request.Root(a2), d1
	lea COMPILER_WORK(a2), a1
	move.l #compiler.SCRATCH_BYTES, d2
	move.l d4, -(sp)
	move.l LenName, d4
	move.l a6, -(sp)
	movea.l a2, a6
	jsr lowering.lowerValue
	movea.l (sp)+, a6
	move.l (sp)+, d4
	tst.l d0
	bne.w lowerFailed
	move.l a3, d1
	sub.l a6, d1
	subq.l #2, d1
	move.b #VALUE_TAG, (a6)
	move.b d1, 1(a6)
	bra.w compiledValue
lowerFailed
	cmpi.l #lowering.STATUS_OUTPUT, d0
	beq.w output
	cmpi.l #lowering.STATUS_DEPTH, d0
	beq.w depth
	bra.w malformed
prepare
	move.l a3, d0
	sub.l a6, d0
	subq.l #2, d0
	lea 2(a6), a0
	tst.l d4
	beq.w folded
	jsr folder.prepareUnfolded
	bra.w prepared
folded
	jsr folder.prepare
prepared
	tst.l d0
	bne.w malformed
	move.b #COMPILED_TAG, (a6)
	move.b d1, 1(a6)
	lea 2(a6), a3
	adda.l d1, a3
compiledValue
	.MEMORY_WORK #0, #1
	.MEMORY_WORK #2, d1
	moveq #STATUS_OK, d0
	bra.w done
output
	moveq #STATUS_OUTPUT, d0
	bra.w done
depth
	moveq #STATUS_DEPTH, d0
	bra.w done
malformed
	moveq #STATUS_MALFORMED, d0
done
	movea.l a5, a0
	adda.l compiler.Request.Consumed(a2), a0
	bra.w restore
unavailable
	moveq #STATUS_MALFORMED, d0
restore
	movem.l (sp)+, d1-d7/a1-a2/a4-a6
	tst.l d0
	rts
	.bend  ; compileMode

	.pub
; A0/A1=bounded wrapper/end,A2=Frame. D0/CCR=0 scalar,1 typed,2 malformed.
; Preserves every other register and the wrapper cursor. Typed wrappers and
; compact references to compound descriptors share one runtime-owned predicate.
requiresTyped	.block
	movem.l d1/d3/a0/a4-a5, -(sp)
	move.l a1, d0
	sub.l a0, d0
	bcs.w bad
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #VALUE_TAG, (a0)
	beq.w typed
	cmpi.b #COMPILED_TAG, (a0)
	bne.w bad
	movea.l a2, a5
	movea.l Frame.Owner(a5), a4
	move.l a4, d0
	beq.w scalar
	tst.l values.Owner.Kinds+memory.Block.Pointer(a4)
	bne.w haveTypedStorage
	tst.l values.Owner.Bindings+memory.Block.Used(a4)
	beq.w scalar
haveTypedStorage
	moveq #0, d3
	move.b 1(a0), d3
	beq.w bad
	addq.l #2, a0
	movea.l a0, a4
	adda.l d3, a4
	cmpa.l a1, a4
	bhi.w bad
	bsr.w compoundSymbols
	bra.w done
typed
	moveq #1, d0
	bra.w done
scalar
	moveq #0, d0
	bra.w done
bad
	moveq #2, d0
done
	movem.l (sp)+, d1/d3/a0/a4-a5
	tst.l d0
	rts
	.bend  ; requiresTyped

	.priv
; A0/A4=compact payload/end,A5=Frame. D0=0 scalar,1 compound,2 malformed.
; Preserve inputs; scan only owners with allocated compound-kind storage.
compoundSymbols	.block
	movem.l d1-d3/a0-a2, -(sp)
next
	cmpa.l a4, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	beq.w scalar
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, d0
	beq.w symbol
	cmpi.b #runtime.EXPRVM_OPCODE_BUILD_STRUCT, d0
	beq.w structLiteral
	moveq #0, d1
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d0
	beq.w skip
	cmpi.b #runtime.COMPACT_NEGATE, d0
	beq.w skip
	cmpi.b #runtime.COMPACT_ADD, d0
	beq.w skip
	cmpi.b #runtime.COMPACT_SUBTRACT, d0
	beq.w skip
	cmpi.b #runtime.COMPACT_MULTIPLY, d0
	beq.w skip
	cmpi.b #runtime.SELECT, d0
	beq.w skip
	moveq #1, d1
	cmpi.b #runtime.COMPACT_I8, d0
	beq.w skip
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_UNARY, d0
	beq.w skip
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_BINARY, d0
	beq.w skip
	moveq #2, d1
	cmpi.b #runtime.COMPACT_I16, d0
	beq.w skip
	moveq #4, d1
	cmpi.b #runtime.COMPACT_I32, d0
	beq.w skip
	cmpi.b #runtime.COMPACT_U32, d0
	beq.w skip
	moveq #8, d1
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_LITERAL, d0
	beq.w skip
	cmpi.b #runtime.COMPACT_I64, d0
	bne.w bad
skip
	adda.l d1, a0
	cmpa.l a4, a0
	bhs.w bad
	bra.w next
structLiteral
	; The two fixed words and each member ID must precede the final End.
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w bad
	moveq #0, d1
	move.b 3(a0), d1
	lsl.w #8, d1
	move.b 2(a0), d1
	add.l d1, d1
	addq.l #4, d1
	cmp.l d0, d1
	bhs.w bad
	bra.w typedSymbol
symbol
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	moveq #0, d1
	move.b 1(a0), d1
	lsl.w #8, d1
	move.b (a0), d1
	addq.l #2, a0
	movem.l d1/a0-a1, -(sp)
	movea.l Frame.Owner(a5), a0
	jsr bindings.getBinding
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	beq.w typedSymbol
	cmpi.l #values.BOUNDS, d0
	bne.w bad
	move.l a0, -(sp)
	movea.l Frame.Owner(a5), a0
	jsr values.getKind
	movea.l (sp)+, a0
	tst.l d0
	bne.w bad
	tst.l d2
	bne.w typedSymbol
	bra.w next
typedSymbol
	moveq #1, d0
	bra.w done
scalar
	cmpa.l a4, a0
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #2, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; compoundSymbols

; A0/D3=bounded payload/bytes,A2=Frame. D0/CCR=0 original type leaf,
; 1 otherwise. On success D1=size,D2=0,D4=1 and Frame scalar metadata clears.
; Others preserved. Source struct definitions also own an ordinary size symbol;
; typed aliases and compound expressions retain the VM's non-scalar type value.
bareTypeScalar	.block
	cmpi.l #4, d3
	bne.w notLeaf
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, (a0)
	bne.w notLeaf
	cmpi.b #runtime.EXPRVM_OPCODE_END, 3(a0)
	bne.w notLeaf
	movem.l d3/d5-d7/a0-a1/a5-a6, -(sp)
	movea.l a2, a5
	moveq #0, d6
	move.b 2(a0), d6
	lsl.w #8, d6
	move.b 1(a0), d6
	cmp.l Frame.Count(a5), d6
	bhs.w unavailable
	movea.l Frame.Defined(a5), a1
	move.l a1, d0
	beq.w unavailable
	tst.b 0(a1, d6.l)
	beq.w unavailable
	movea.l Frame.Owner(a5), a0
	move.l a0, d0
	beq.w unavailable
	move.l d6, d1
	jsr values.getKind
	tst.l d0
	bne.w unavailable
	cmpi.l #structs.TYPE, d2
	bne.w unavailable
	movea.l Frame.Values(a5), a1
	move.l a1, d0
	beq.w unavailable
	lsl.l #3, d1
	tst.l runtime.Value.High(a1, d1.l)
	bne.w unavailable
	move.l runtime.Value.Low(a1, d1.l), d1
	move.l d1, d0
	jsr records.validate
	tst.l d0
	bne.w unavailable
	jsr values.view
	tst.l d0
	bne.w unavailable
	cmpi.l #structs.TYPE, values.StructRecord.Kind(a1)
	bne.w unavailable
	cmp.l values.StructRecord.Nominal(a1), d6
	bne.w unavailable
	move.l values.StructRecord.Size(a1), d1
	clr.l Frame.Kind(a5)
	clr.l Frame.High(a5)
	clr.l Frame.Provenance(a5)
	moveq #0, d2
	moveq #1, d4
	moveq #STATUS_OK, d0
	bra.w done
unavailable
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d3/d5-d7/a0-a1/a5-a6
	tst.l d0
	rts
notLeaf
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; bareTypeScalar

; Generate both entry points from one body. The ordinary evaluator retains
; its register ABI and instruction sequence; the target predicate additionally
; consumes the symbol-presence result already computed by ExprVM.
EVALUATE	.macro saved, compound=0
	movem.l .saved, -(sp)
	.MEMORY_WORK #1, #1
	move.l a1, d0
	sub.l a0, d0
	bcs.w malformed
	cmpi.l #3, d0
	blo.w malformed
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #VALUE_TAG, d3
	beq.w typedValue
	cmpi.b #COMPILED_TAG, d3
	bne.w malformed
	moveq #0, d3
	move.b (a0)+, d3
	beq.w malformed
	subq.l #2, d0
	cmp.l d3, d0
	blo.w malformed
	movea.l a0, a4
	adda.l d3, a4
	cmpi.b #runtime.EXPRVM_OPCODE_END, -1(a4)
	bne.w malformed
	movea.l a2, a5
	clr.l Frame.Provenance(a5)
	.if .compound == 0
	bsr.w bareTypeScalar
	bne.w compactAlias
	movea.l a4, a0
	bra.w done
compactAlias
	; Compact bare record aliases project their canonical default cell.
	cmpi.l #4, d3
	bne.w compactScalar
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, (a0)
	bne.w compactScalar
	moveq #0, d1
	move.b 2(a0), d1
	lsl.w #8, d1
	move.b 1(a0), d1
	cmp.l Frame.Count(a5), d1
	bhs.w malformed
	movem.l d1/d3/a0, -(sp)
	movea.l Frame.Owner(a5), a0
	move.l a0, d0
	beq.w compactAliasChecked
	jsr values.getKind
	tst.l d0
	bne.w compactAliasChecked
	cmpi.l #records.ADDRESS_RECORD, d2
	bne.w compactAliasChecked
	movea.l Frame.Defined(a5), a6
	moveq #0, d6
	move.b 0(a6, d1.l), d6
	lsl.l #3, d1
	movea.l Frame.Values(a5), a6
	move.l runtime.Value.Low(a6, d1.l), d0
	jsr records.project
	tst.l d0
	bne.w compactAliasChecked
	move.l d3, Frame.High(a5)
	move.l d5, Frame.Provenance(a5)
	move.l d2, d7
	movem.l (sp)+, d1/d3/a0
	move.l d7, d1
	moveq #0, d2
	tst.l d6
	bne.w compactAliasResolved
	moveq #1, d2
compactAliasResolved
	moveq #1, d4  ; evaluateWithSymbols exposes the compact alias symbol read
	movea.l a4, a0
	bra.w done
compactAliasChecked
	movem.l (sp)+, d1/d3/a0
compactScalar
	.endif
	.if .compound
	clr.l Frame.Kind(a5)
	; A bare alias retains its immutable descriptor and explicit kind.
	; Captured literal aliases enforce the assembler absolute-field boundary.
	cmpi.l #4, d3
	bne.w scalarValue
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, (a0)
	bne.w scalarValue
	moveq #0, d1
	move.b 2(a0), d1
	lsl.w #8, d1
	move.b 1(a0), d1
	cmp.l Frame.Count(a5), d1
	bhs.w malformed
	movea.l Frame.Defined(a5), a6
	moveq #0, d5
	move.b 0(a6, d1.l), d5
	movea.l Frame.Owner(a5), a0
	move.l a0, d0
	beq.w scalarValue
	jsr values.getKind
	tst.l d0
	bne.w malformed
	tst.l d2
	beq.w scalarValue
	cmpi.l #structs.LITERAL, d2
	bhi.w malformed
	move.l d2, Frame.Kind(a5)
	lsl.l #3, d1
	movea.l Frame.Values(a5), a0
	tst.l runtime.Value.High(a0, d1.l)
	bne.w malformed
	move.l runtime.Value.Low(a0, d1.l), d1
	movem.l d1-d2/a0, -(sp)
	movea.l Frame.Owner(a5), a0
	cmpi.l #records.TYPED_LIST, d2
	blo.w aliasLegacy
	move.l d1, d0
	jsr records.validate
	tst.l d0
	bne.w aliasValidated
	cmpi.l #structs.LITERAL, d2
	bne.w aliasValidated
	move.l d1, d0
	jsr structs.assemblerLiteral
	bra.w aliasValidated
aliasLegacy
	jsr values.compoundLength
aliasValidated
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	bne.w malformed
	clr.l Frame.High(a5)
	moveq #0, d2
	tst.l d5
	bne.w aliasResolved
	moveq #1, d2
aliasResolved
	moveq #0, d0
	movea.l a4, a0
	bra.w done
scalarValue
	movea.l a4, a0
	suba.l d3, a0
	.endif
	; Scalar-only owners keep the existing compact evaluator path.
	movea.l Frame.Owner(a5), a0
	move.l a0, d0
	beq.w compactNumeric
	tst.l values.Owner.Kinds+memory.Block.Pointer(a0)
	bne.w compactTypedStorage
	tst.l values.Owner.Bindings+memory.Block.Used(a0)
	beq.w compactNumeric
compactTypedStorage
	movea.l a4, a0
	suba.l d3, a0
	bsr.w compoundSymbols
	cmpi.l #2, d0
	beq.w malformed
	tst.l d0
	beq.w compactNumeric
	.if .compound == 0
	movea.l Frame.Owner(a2), a6
	move.l values.Owner.Arena+memory.Block.Used(a6), d5
	.endif
	move.l d3, d0
	jsr typed.evaluateCompact
	tst.l d0
	bne.w done
	bra.w typedResult
compactNumeric
	movea.l a4, a0
	suba.l d3, a0
	move.l d3, d0
	move.l Frame.Count(a5), d1
	move.l Frame.Pc(a5), d2
	movea.l Frame.Values(a5), a2
	movea.l Frame.Defined(a5), a6
	jsr runtime.evalCompact64
	tst.l d0
	bne.w evaluated
	jsr runtime.exprvmGetLastResultHighV1
	move.l d1, Frame.High(a5)
evaluated
	movea.l a4, a0
	move.l d3, d1
	move.l d5, d2
	bra.w done
typedValue
	moveq #0, d3
	move.b (a0)+, d3
	beq.w malformed
	subq.l #2, d0
	cmp.l d3, d0
	blo.w malformed
	.if .compound == 0
	bsr.w bareTypeScalar
	bne.w canonicalValue
	adda.l d3, a0
	bra.w done
canonicalValue
	movea.l Frame.Owner(a2), a6
	move.l a6, d0
	beq.w malformed
	move.l values.Owner.Arena+memory.Block.Used(a6), d5
	.endif
	move.l d3, d0
	jsr typed.evaluateAssembler
	tst.l d0
	bne.w done
typedResult
	.if .compound
	cmpi.l #structs.LITERAL, d3
	bne.w capturedValue
	movem.l d1-d2/a0, -(sp)
	movea.l Frame.Owner(a2), a0
	move.l d1, d0
	jsr structs.assemblerLiteral
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	bne.w done
capturedValue
	move.l d3, Frame.Kind(a2)
	.else
	cmpi.l #records.ADDRESS_RECORD, d3
	bne.w typedNotRecord
	movem.l d1-d2/d5/a0, -(sp)
	move.l d1, d0
	movea.l Frame.Owner(a2), a0
	jsr records.project
	move.l d3, Frame.High(a2)
	move.l d5, Frame.Provenance(a2)
	move.l d2, d3
	movem.l (sp)+, d1-d2/d5/a0
	move.l d3, d1
	tst.l d0
	bne.w done
	bra.w typedScalar
typedNotRecord
	tst.l d3
	beq.w typedScalar
	move.l d5, values.Owner.Arena+memory.Block.Used(a6)
	bra.w malformed
typedScalar
	move.l d5, values.Owner.Arena+memory.Block.Used(a6)
	.endif
	bra.w done
malformed
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, .saved
	tst.l d0
	rts
.endmacro
	.pub

; A0=compiled wrapper, A1=bounded end, A2=Frame.
; Returns D0/CCR=status, D1=low scalar word, D2=unresolved, A0=after wrapper.
; On success, writes the signed scalar high word to Frame.High.
; Preserves D3-D7/A1-A6. No parser, lexical storage or source fallback.
evaluate	.block
	.EVALUATE d3-d7/a1-a6
	.bend  ; evaluate

; Same inputs and scalar outputs as evaluate, plus D4=nonzero if any symbol
; was read. D4 is clobbered; D3/D5-D7/A1-A6 are preserved. CCR reflects D0.
evaluateWithSymbols	.block
	.EVALUATE d3/d5-d7/a1-a6
	.bend  ; evaluateWithSymbols

; Declaration value variant: Kind is explicit; Low is an arena offset for
; compounds. Scalars retain the original signed64 cell representation.
; Lists must have scalar-projectable items at this assembler boundary; shared
; ExprVM construction retains nested shapes for intermediate expressions.
evaluateValue	.block
	.EVALUATE d3-d7/a1-a6, 1
	.bend  ; evaluateValue

	.endsection
	.section bss, kind=bss
DeferredCompile
	.res word, 1
	.align 4
Program
	.res long, 1
ProgramBytes
	.res long, 1
Workspace
	.res long, 1
WorkspaceBytes
	.res long, 1
LenName
	.res long, 1
	.endsection
	.endmodule
