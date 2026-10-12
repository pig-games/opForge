; Validate focused request spans before any caller workspace is written.
; @opforge-owner: prvm.amigaos.statement_request
	.module prvm.amigaos.statement_request
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.priv
Check	.struct
Pointer	.word ?
Extent	.word ?
TokenCount	.word ?
Aligned	.word ?
Mutable	.word ?
	.endstruct
CHECK_BYTES = Check.Mutable+2
Bound	.struct
Start	.long ?
End	.long ?
Mutable	.long ?
	.endstruct
BOUND_BYTES = Bound.Mutable+4
BOUND_CAPACITY = 7
	.pub
	.section code, kind=code
; A0=request,D0=available bytes. D0=status/CCR; other registers preserved.
; Output/workspace must be disjoint from every readable span and each other.
compile	.block
	movem.l d1-d3/a1, -(sp)
	lea CompileChecks, a1
	moveq #5, d1
	moveq #abi.COMPILE_FRAME_BYTES, d2
	moveq #abi.CompileFrame.StepBudget, d3
	bsr.w validate
	tst.l d0
	movem.l (sp)+, d1-d3/a1
	rts
	.bend  ; compile
; A0=request,D0=available bytes. Same status/register/span contract as compile.
match	.block
	movem.l d1-d3/a1, -(sp)
	lea MatchChecks, a1
	moveq #6, d1
	moveq #abi.MATCH_FRAME_BYTES, d2
	moveq #abi.MatchFrame.StepBudget, d3
	bsr.w validate
	tst.l d0
	movem.l (sp)+, d1-d3/a1
	rts
	.bend  ; match
	.priv
; A1=checks,D1=count,D2=frame bytes,D3=budget field. Others as public entry.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -BOUND_CAPACITY*BOUND_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a4
	movea.l a1, a3
	move.l d1, d7
	cmp.l d2, d0
	blo.w invalid
	move.l a4, d1
	btst #0, d1
	bne.w invalid
	move.l d2, d0
	jsr inputs.span
	bne.w done
	tst.l 0(a4, d3.l)
	bmi.w invalid
	move.l a4, (a6)  ; Bound.Start (zero offset)
	move.l a4, d0
	add.l d2, d0
	move.l d0, Bound.End(a6)
	clr.l Bound.Mutable(a6)
	lea BOUND_BYTES(a6), a5
next
	moveq #0, d0
	move.w Check.Pointer(a3), d0
	movea.l 0(a4, d0.l), a0
	moveq #0, d0
	move.w Check.Extent(a3), d0
	move.l 0(a4, d0.l), d4
	tst.w Check.TokenCount(a3)
	beq.w extentReady
	cmpi.l #$03ffffff, d4
	bhi.w invalid
	lsl.l #5, d4  ; abi.TOKEN_BYTES = 32
extentReady
	move.l a0, d1
	tst.w Check.Aligned(a3)
	beq.w addressReady
	btst #0, d1
	bne.w invalid
addressReady
	move.l d4, d0
	jsr inputs.span
	bne.w done
	move.l a0, (a5)  ; Bound.Start (zero offset)
	move.l a0, d0
	add.l d4, d0
	move.l d0, Bound.End(a5)
	moveq #0, d6
	move.w Check.Mutable(a3), d6
	move.l d6, Bound.Mutable(a5)
	tst.l d4
	beq.w advance
	movea.l a6, a1
prior
	cmpa.l a5, a1
	beq.w advance
	move.l Bound.Mutable(a1), d0
	or.l d6, d0
	beq.w priorNext
	move.l (a1), d0  ; Bound.Start (zero offset)
	cmp.l Bound.End(a1), d0
	beq.w priorNext
	cmp.l Bound.End(a5), d0
	bhs.w priorNext
	move.l (a5), d0  ; Bound.Start (zero offset)
	cmp.l Bound.End(a1), d0
	blo.w invalid
priorNext
	adda.w #BOUND_BYTES, a1
	bra.w prior
advance
	adda.w #BOUND_BYTES, a5
	adda.w #CHECK_BYTES, a3
	subq.l #1, d7
	bne.w next
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	lea BOUND_CAPACITY*BOUND_BYTES(sp), sp
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; validate
	.endsection
	.section data, kind=data
	.align 2
CompileChecks
	.word abi.CompileFrame.ProgramPtr, abi.CompileFrame.ProgramBytes, 0, 0, 0
	.word abi.CompileFrame.TokenPtr, abi.CompileFrame.TokenCount, 1, 1, 0
	.word abi.CompileFrame.ArenaPtr, abi.CompileFrame.ArenaBytes, 0, 0, 0
	.word abi.CompileFrame.OutputPtr, abi.CompileFrame.OutputCapacity, 0, 1, 1
	.word abi.CompileFrame.WorkspacePtr, abi.CompileFrame.WorkspaceCapacity, 0, 1, 1
MatchChecks
	.word abi.MatchFrame.ProgramPtr, abi.MatchFrame.ProgramBytes, 0, 0, 0
	.word abi.MatchFrame.PlanPtr, abi.MatchFrame.PlanBytes, 0, 1, 0
	.word abi.MatchFrame.TokenPtr, abi.MatchFrame.TokenCount, 1, 1, 0
	.word abi.MatchFrame.ArenaPtr, abi.MatchFrame.ArenaBytes, 0, 0, 0
	.word abi.MatchFrame.OutputPtr, abi.MatchFrame.OutputCapacity, 0, 1, 1
	.word abi.MatchFrame.WorkspacePtr, abi.MatchFrame.WorkspaceCapacity, 0, 1, 1
	.endsection
	.endmodule
