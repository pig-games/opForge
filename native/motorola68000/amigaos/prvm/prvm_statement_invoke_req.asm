; Validate invocation frame spans before writing caller-owned staging storage.
; @opforge-owner: prvm.amigaos.statement_invoke_req
	.module prvm.amigaos.statement_invoke_req
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_request as request
	.use prvm.amigaos.statement_inputs as inputs
	.pub
	.section code, kind=code
; A0/D0=frame/available bytes. D0=status/CCR; other registers preserved.
route	.block
	movem.l d1-d3/a1, -(sp)
	lea RouteChecks, a1
	moveq #6, d1
	moveq #abi.ROUTE_FRAME_BYTES, d2
	moveq #abi.RouteFrame.StepBudget, d3
	jsr request.validate
	tst.l d0
	movem.l (sp)+, d1-d3/a1
	rts
	.bend
; Same frame ABI,A1=optional shared cell checked against every route span.
routeBudget	.block
	movem.l d1-d3/a1-a2, -(sp)
	movea.l a1, a2
	lea RouteChecks, a1
	moveq #6, d1
	moveq #abi.ROUTE_FRAME_BYTES, d2
	moveq #abi.RouteFrame.StepBudget, d3
	jsr request.validateBudget
	tst.l d0
	movem.l (sp)+, d1-d3/a1-a2
	rts
	.bend  ; routeBudget
fallbackBudget	.block
	jsr request.compileBudget
	rts
	.bend  ; fallbackBudget
; FallbackFrame has the same bounded span layout as CompileFrame.
fallback	.block
	jsr request.compile
	rts
	.bend
; Validate descriptor spans even when their plans are outside the selected group.
; Plan semantics are checked by the matcher only for the selected keyword group.
select	.block
	move.l a1, -(sp)
	suba.l a1, a1
	bsr.w selectBudget
	movea.l (sp)+, a1
	tst.l d0
	rts
	.bend  ; select
; A1=optional cell. Also reject overlap with EVERY indirectly addressed plan.
selectBudget	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a2
	movea.l a0, a4
	lea SelectChecks, a1
	moveq #10, d1
	moveq #abi.SELECT_FRAME_BYTES, d2
	moveq #abi.SelectFrame.StepBudget, d3
	jsr request.validateBudget
	bne.w done
	movea.l abi.SelectFrame.PlanRefsPtr(a4), a3
	move.l abi.SelectFrame.PlanRefsCount(a4), d7
	cmp.l abi.SelectFrame.StepBudget(a4), d7
	bhi.w exhausted
	move.l a2, d0
	beq.w next
	cmp.l (a2), d7
	bhi.w exhausted
next
	tst.l d7
	beq.w valid
	movea.l (a3), a5  ; PlanRef.Ptr (zero offset)
	move.l abi.PlanRef.Bytes(a3), d6
	move.l a5, d0
	btst #0, d0
	bne.w invalid
	movea.l a5, a0
	move.l d6, d0
	jsr inputs.span
	bne.w done
	move.l a2, d0
	beq.w cellSeparate
	movea.l a5, a0
	move.l d6, d0
	movea.l a2, a1
	moveq #4, d1
	jsr inputs.separate
	bne.w done
cellSeparate
	movea.l a5, a0
	move.l d6, d0
	movea.l abi.SelectFrame.OutputPtr(a4), a1
	move.l abi.SelectFrame.OutputCapacity(a4), d1
	jsr inputs.separate
	bne.w done
	movea.l a5, a0
	move.l d6, d0
	movea.l abi.SelectFrame.FallbackPtr(a4), a1
	move.l abi.SelectFrame.FallbackCapacity(a4), d1
	jsr inputs.separate
	bne.w done
	movea.l a5, a0
	move.l d6, d0
	movea.l abi.SelectFrame.WorkspacePtr(a4), a1
	move.l abi.SelectFrame.WorkspaceCapacity(a4), d1
	jsr inputs.separate
	bne.w done
	adda.w #abi.PLAN_REF_BYTES, a3
	subq.l #1, d7
	bra.w next
exhausted
	moveq #abi.BUDGET_EXCEEDED, d0
	bra.w done
valid
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend
	.priv
	.endsection
	.section data, kind=data
	.align 2
RouteChecks
	.word abi.RouteFrame.ProgramPtr, abi.RouteFrame.ProgramBytes, 0, 0, 0
	.word abi.RouteFrame.TokenPtr, abi.RouteFrame.TokenCount, 5, 1, 0
	.word abi.RouteFrame.ArenaPtr, abi.RouteFrame.ArenaBytes, 0, 0, 0
	.word abi.RouteFrame.ScopePtr, abi.RouteFrame.ScopeCount, 4, 1, 0
	.word abi.RouteFrame.KeywordsPtr, abi.RouteFrame.KeywordsBytes, 0, 0, 0
	.word abi.RouteFrame.OutputPtr, abi.RouteFrame.OutputCapacity, 0, 1, 1
SelectChecks
	.word abi.SelectFrame.ProgramPtr, abi.SelectFrame.ProgramBytes, 0, 0, 0
	.word abi.SelectFrame.ScopePtr, abi.SelectFrame.ScopeCount, 4, 1, 0
	.word abi.SelectFrame.KeywordsPtr, abi.SelectFrame.KeywordsBytes, 0, 0, 0
	.word abi.SelectFrame.PlanRefsPtr, abi.SelectFrame.PlanRefsCount, 3, 1, 0
	.word abi.SelectFrame.HeadPtr, abi.SelectFrame.HeadBytes, 0, 0, 0
	.word abi.SelectFrame.TokenPtr, abi.SelectFrame.TokenCount, 5, 1, 0
	.word abi.SelectFrame.ArenaPtr, abi.SelectFrame.ArenaBytes, 0, 0, 0
	.word abi.SelectFrame.OutputPtr, abi.SelectFrame.OutputCapacity, 0, 1, 1
	.word abi.SelectFrame.FallbackPtr, abi.SelectFrame.FallbackCapacity, 0, 1, 1
	.word abi.SelectFrame.WorkspacePtr, abi.SelectFrame.WorkspaceCapacity, 0, 1, 1
	.endsection
	.endmodule
