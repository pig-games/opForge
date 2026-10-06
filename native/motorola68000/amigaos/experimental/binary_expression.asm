; Compile bounded scalar tokens once; evaluate through the shared ExprVM.
; @opforge-owner: opasm.amigaos.binary_expression
	.module opasm.amigaos.binary_expression
	.cpu 68020
	.use exprvm.amigaos.runtime as runtime
	.use opasm.amigaos.binary_fold as folder
	.use experimental.amigaos.binary_exvm as compiler
	.use experimental.amigaos.binary_exvm_lower as lowering
	.include "telemetry_macros.i"
	.include "memory_telemetry.i"
	.pub
COMPILED_TAG = $81
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
	.endstruct
FRAME_BYTES = Frame.High+4
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
	moveq #0, d0
	rts
	.bend  ; configure

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
	jsr lowering.lower
	tst.l d0
	beq.w prepare
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

	.priv
; Generate both entry points from one body. The ordinary evaluator retains
; its register ABI and instruction sequence; the target predicate additionally
; consumes the symbol-presence result already computed by ExprVM.
EVALUATE	.macro saved
	movem.l .saved, -(sp)
	.MEMORY_WORK #1, #1
	move.l a1, d0
	sub.l a0, d0
	bcs.w malformed
	cmpi.l #3, d0
	blo.w malformed
	cmpi.b #COMPILED_TAG, (a0)+
	bne.w malformed
	moveq #0, d3
	move.b (a0)+, d3
	beq.w malformed
	subq.l #2, d0
	cmp.l d3, d0
	blo.w malformed
	movea.l a0, a4
	adda.l d3, a4
	cmpi.b #runtime.EXPRVM_V2_OPCODE_END, -1(a4)
	bne.w malformed
	movea.l a2, a5
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

	.endsection
	.section bss, kind=bss
Program
	.res long, 1
ProgramBytes
	.res long, 1
Workspace
	.res long, 1
WorkspaceBytes
	.res long, 1
	.endsection
	.endmodule
