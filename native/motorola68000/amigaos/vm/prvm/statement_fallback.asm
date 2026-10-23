; Derive letter/digit fallback facets without rereading or retokenizing source.
; @opforge-owner: opforge.vm.prvm.statement_fallback
	.module opforge.vm.prvm.statement_fallback
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_inputs as inputs
	.use opforge.vm.prvm.statement_facets as facets
	.use opforge.vm.prvm.statement_policy as policy
	.use opforge.vm.prvm.statement_decimal as decimal
	.use opforge.vm.prvm.statement_invoke_req as request
	.include "../../debug/telemetry_macros.i"
	.priv
Local	.struct
Budget	.long ?
BudgetPtr	.long ?
Policy	.long ?
Count	.long ?
Bytes	.long ?
	.endstruct
LOCAL_BYTES = Local.Bytes+4
	.pub
	.section code, kind=code
; A0/D0=FallbackFrame/available bytes. D0=status/CCR,D1=view count,D2=bytes.
; Other registers preserved. Publication is atomic; literals borrow input arena.
run	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a4
	suba.l a1, a1
	bsr.w stage
	bne.w failed
	cmp.l abi.FallbackFrame.OutputCapacity(a4), d2
	bhi.w overflow
	movea.l abi.FallbackFrame.WorkspacePtr(a4), a0
	movea.l abi.FallbackFrame.OutputPtr(a4), a1
	move.l d2, d4
copy
	tst.l d4
	beq.w done
	move.b (a0)+, (a1)+
	subq.l #1, d4
	bra.w copy
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
failed
	moveq #0, d1
	moveq #0, d2
done
	tst.l d0
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend
; Same frame/status ABI, A1=shared budget cell (null selects frame budget).
; Writes only WorkspacePtr, never OutputPtr; callers publish only a winning pass.
; Shared cells are validated caller-owned storage disjoint from all frame spans.
stage	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movem.l d3-d7/a0-a6, -(sp)
	lea -LOCAL_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a4
	movea.l a1, a5
	jsr request.fallbackBudget
	bne.w finish
	move.l abi.FallbackFrame.StepBudget(a4), (a6)  ; Local.Budget (zero offset)
	move.l a5, Local.BudgetPtr(a6)
	move.l a5, d0
	bne.w budget
	move.l a6, Local.BudgetPtr(a6)
budget
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.budget
	bne.w finish
	movea.l abi.FallbackFrame.ProgramPtr(a4), a0
	move.l abi.FallbackFrame.ProgramBytes(a4), d0
	movea.l Local.BudgetPtr(a6), a2
	jsr policy.match
	bne.w finish
	movea.l a1, a5
	move.l a5, Local.Policy(a6)
	movea.l abi.FallbackFrame.TokenPtr(a4), a0
	move.l abi.FallbackFrame.TokenCount(a4), d0
	movea.l abi.FallbackFrame.ArenaPtr(a4), a1
	move.l abi.FallbackFrame.ArenaBytes(a4), d1
	jsr facets.validate
	bne.w finish
	move.l abi.FallbackFrame.TokenCount(a4), Local.Count(a6)
	move.l abi.FallbackFrame.TokenCount(a4), d7
	movea.l abi.FallbackFrame.TokenPtr(a4), a3
measure
	tst.l d7
	beq.w measured
	bsr.w eligible
	bne.w finish
	add.l d1, Local.Count(a6)
	adda.w #abi.TOKEN_BYTES, a3
	subq.l #1, d7
	bra.w measure
measured
	move.l Local.Count(a6), d0
	cmpi.l #$03ffffff, d0
	bhi.w overflow
	lsl.l #5, d0  ; abi.TOKEN_BYTES
	move.l d0, Local.Bytes(a6)
	cmp.l abi.FallbackFrame.WorkspaceCapacity(a4), d0
	bhi.w overflow
	move.l abi.FallbackFrame.TokenCount(a4), d7
	movea.l abi.FallbackFrame.TokenPtr(a4), a3
	movea.l abi.FallbackFrame.WorkspacePtr(a4), a2
next
	tst.l d7
	beq.w prepared
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	bsr.w eligible
	bne.w finish
	move.l d1, d6
	movea.l a3, a0
	moveq #abi.TOKEN_BYTES/4-1, d0
copyView
	move.l (a0)+, (a2)+
	dbra d0, copyView
	tst.l d6
	beq.w advance
	move.l abi.TokenView.ColStart(a3), d4
	addq.l #1, d4
	bcs.w invalid
	lea -abi.TOKEN_BYTES(a2), a1
	move.l #1, abi.TokenView.LiteralBytes(a1)
	move.l d4, abi.TokenView.ColEnd(a1)
	clr.b abi.TokenView.NumericStatus(a1)
	clr.l abi.TokenView.NumericHigh(a1)
	clr.l abi.TokenView.NumericLow(a1)
	movea.l a3, a0
	moveq #abi.TOKEN_BYTES/4-1, d0
copyNumber
	move.l (a0)+, (a2)+
	dbra d0, copyNumber
	lea -abi.TOKEN_BYTES(a2), a1
	move.b #abi.K_NUMBER, (a1)  ; TokenView.Kind (zero offset)
	addq.l #1, abi.TokenView.LiteralOffset(a1)
	subq.l #1, abi.TokenView.LiteralBytes(a1)
	move.l d4, abi.TokenView.ColStart(a1)
	movea.l abi.FallbackFrame.ArenaPtr(a4), a0
	adda.l abi.TokenView.LiteralOffset(a1), a0
	move.l abi.TokenView.LiteralBytes(a1), d0
	movem.l a1-a2, -(sp)
	movea.l a5, a1
	movea.l Local.BudgetPtr(a6), a2
	jsr decimal.prepare
	movem.l (sp)+, a1-a2
	bne.w finish
	move.b d3, abi.TokenView.NumericStatus(a1)
	move.l d1, abi.TokenView.NumericHigh(a1)
	move.l d2, abi.TokenView.NumericLow(a1)
advance
	adda.w #abi.TOKEN_BYTES, a3
	subq.l #1, d7
	bra.w next
prepared
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.tick
	bne.w finish
	move.l Local.Count(a6), d1
	move.l Local.Bytes(a6), d2
	moveq #abi.OK, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	bra.w finish
invalid
	moveq #abi.INVALID_ARGUMENT, d0
finish
	moveq #0, d1
	moveq #0, d2
done
	.TELEMETRY_VM_LEAVE
	lea LOCAL_BYTES(sp), sp
	tst.l d0
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend
	.priv
; A3=view,A4=frame,A5=A9,A6=locals. D0=status,D1=split; others preserved.
eligible	.block
	movem.l d2-d4/a0-a1, -(sp)
	moveq #0, d1
	btst #2, abi.InvocationPolicy.Flags(a5)
	beq.w valid
	cmpi.b #abi.K_REGISTER, (a3)
	bhi.w valid
	move.l abi.TokenView.LiteralBytes(a3), d4
	cmpi.l #2, d4
	blo.w valid
	movea.l abi.FallbackFrame.ArenaPtr(a4), a1
	adda.l abi.TokenView.LiteralOffset(a3), a1
	moveq #0, d2
	move.b (a1)+, d2
	cmp.b abi.InvocationPolicy.UpperFirst(a5), d2
	blo.w lower
	cmp.b abi.InvocationPolicy.UpperLast(a5), d2
	bls.w digits
lower
	cmp.b abi.InvocationPolicy.LowerFirst(a5), d2
	blo.w valid
	cmp.b abi.InvocationPolicy.LowerLast(a5), d2
	bhi.w valid
digits
	subq.l #1, d4
next
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.tick
	bne.w failed
	moveq #0, d2
	move.b (a1)+, d2
	cmp.b abi.InvocationPolicy.DigitFirst(a5), d2
	blo.w valid
	cmp.b abi.InvocationPolicy.DigitLast(a5), d2
	bhi.w valid
	subq.l #1, d4
	bne.w next
	moveq #1, d1
valid
	moveq #abi.OK, d0
	bra.w done
failed
	moveq #0, d1
done
	tst.l d0
	movem.l (sp)+, d2-d4/a0-a1
	rts
	.bend
	.endsection
	.endmodule
