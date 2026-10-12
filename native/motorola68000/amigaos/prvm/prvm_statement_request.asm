; Validate focused request spans before any caller workspace is written.
; @opforge-owner: prvm.amigaos.statement_request
	.module prvm.amigaos.statement_request
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.pub
Check	.struct
Pointer	.word ?
Extent	.word ?
CountShift	.word ?
Aligned	.word ?
Mutable	.word ?
	.endstruct
CHECK_BYTES = Check.Mutable+2
	.priv
Bound	.struct
Start	.long ?
End	.long ?
Mutable	.long ?
	.endstruct
BOUND_BYTES = Bound.Mutable+4
BOUND_CAPACITY = 12
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
; Same frame ABI,A1=optional shared budget cell. No cell/input/output overlap.
compileBudget	.block
	movem.l d1-d3/a1-a2, -(sp)
	movea.l a1, a2
	lea CompileChecks, a1
	moveq #5, d1
	moveq #abi.COMPILE_FRAME_BYTES, d2
	moveq #abi.CompileFrame.StepBudget, d3
	bsr.w validateBudget
	tst.l d0
	movem.l (sp)+, d1-d3/a1-a2
	rts
	.bend  ; compileBudget
matchBudget	.block
	movem.l d1-d3/a1-a2, -(sp)
	movea.l a1, a2
	lea MatchChecks, a1
	moveq #6, d1
	moveq #abi.MATCH_FRAME_BYTES, d2
	moveq #abi.MatchFrame.StepBudget, d3
	bsr.w validateBudget
	tst.l d0
	movem.l (sp)+, d1-d3/a1-a2
	rts
	.bend  ; matchBudget
; A0/D0=request/available bytes,A1=trusted Check table,D1=check count,
; D2=frame bytes,D3=budget field offset. D0=status/CCR; others preserved.
; Check.CountShift is log2(row stride), zero for byte extents. Tables belong to
; the calling request owner; mutable spans must be disjoint from all inputs.
validate	.block
	move.l a2, -(sp)
	suba.l a2, a2
	bsr.w validateBudget
	movea.l (sp)+, a2
	tst.l d0
	rts
	.bend  ; validate
; Same trusted-table ABI plus A2=optional shared cell. Validate the cell, then
; treat it as mutable against the frame and EVERY table span before any writes.
validateBudget	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -BOUND_CAPACITY*BOUND_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a4
	movea.l a1, a3
	move.l d1, d7
	tst.l d7
	ble.w invalid
	cmpi.l #BOUND_CAPACITY-1, d7
	bhi.w invalid
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
	move.l a2, d0
	beq.w next
	cmpi.l #BOUND_CAPACITY-2, d7
	bhi.w invalid
	movea.l a2, a0
	jsr inputs.budget
	bne.w done
	movea.l a2, a0
	moveq #4, d0
	movea.l a4, a1
	move.l d2, d1
	jsr inputs.separate
	bne.w done
	move.l a2, (a5)  ; Bound.Start (zero offset)
	move.l a2, d0
	addq.l #4, d0
	move.l d0, Bound.End(a5)
	move.l #1, Bound.Mutable(a5)
	adda.w #BOUND_BYTES, a5
next
	moveq #0, d0
	move.w Check.Pointer(a3), d0
	movea.l 0(a4, d0.l), a0
	moveq #0, d0
	move.w Check.Extent(a3), d0
	move.l 0(a4, d0.l), d4
	moveq #0, d0
	move.w Check.CountShift(a3), d0
	cmpi.l #5, d0
	bhi.w invalid
	move.l #$7fffffff, d1
	lsr.l d0, d1
	cmp.l d1, d4
	bhi.w invalid
	lsl.l d0, d4
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
	.bend  ; validateBudget
	.endsection
	.priv
	.section data, kind=data
	.align 2
CompileChecks
	.word abi.CompileFrame.ProgramPtr, abi.CompileFrame.ProgramBytes, 0, 0, 0
	.word abi.CompileFrame.TokenPtr, abi.CompileFrame.TokenCount, 5, 1, 0
	.word abi.CompileFrame.ArenaPtr, abi.CompileFrame.ArenaBytes, 0, 0, 0
	.word abi.CompileFrame.OutputPtr, abi.CompileFrame.OutputCapacity, 0, 1, 1
	.word abi.CompileFrame.WorkspacePtr, abi.CompileFrame.WorkspaceCapacity, 0, 1, 1
MatchChecks
	.word abi.MatchFrame.ProgramPtr, abi.MatchFrame.ProgramBytes, 0, 0, 0
	.word abi.MatchFrame.PlanPtr, abi.MatchFrame.PlanBytes, 0, 1, 0
	.word abi.MatchFrame.TokenPtr, abi.MatchFrame.TokenCount, 5, 1, 0
	.word abi.MatchFrame.ArenaPtr, abi.MatchFrame.ArenaBytes, 0, 0, 0
	.word abi.MatchFrame.OutputPtr, abi.MatchFrame.OutputCapacity, 0, 1, 1
	.word abi.MatchFrame.WorkspacePtr, abi.MatchFrame.WorkspaceCapacity, 0, 1, 1
	.endsection
	.endmodule
