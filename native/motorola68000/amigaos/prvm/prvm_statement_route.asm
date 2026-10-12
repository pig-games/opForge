; Route a prepared invocation through the longest registered keyword prefix.
; @opforge-owner: prvm.amigaos.statement_route
	.module prvm.amigaos.statement_route
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.use prvm.amigaos.statement_facets as facets
	.use prvm.amigaos.statement_policy as policy
	.use prvm.amigaos.statement_scope as scope
	.use prvm.amigaos.statement_invoke_req as request
	.include "telemetry_macros.i"
	.priv
Local	.struct
Frame	.long ?
Budget	.long ?
BudgetPtr	.long ?
Policy	.long ?
LabelToken	.long ?
HeadToken	.long ?
	.endstruct
LOCAL_BYTES = Local.HeadToken+4
	.pub
	.section code, kind=code
; A0/D0=RouteFrame/available bytes. D0=status/CCR,D1=found,D2=label,D3=head.
; Route12 is published only on success/found; absent/error outputs are zero.
; Other registers preserved. No original source or tokenizer access.
run	.block
	move.l a1, -(sp)
	suba.l a1, a1
	bsr.w runBudget
	movea.l (sp)+, a1
	tst.l d0
	rts
	.bend  ; run
; Same frame/output ABI,A1=shared budget cell (null selects frame budget).
; The cell is validated caller-owned mutable storage disjoint from frame spans.
; The caller retains the remaining value across preparation and selection.
runBudget	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movem.l d4-d7/a0-a6, -(sp)
	lea -LOCAL_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a5
	move.l a5, (a6)  ; Local.Frame (zero offset)
	jsr request.routeBudget
	bne.w finish
	move.l abi.RouteFrame.StepBudget(a5), Local.Budget(a6)
	move.l a1, Local.BudgetPtr(a6)
	tst.l Local.BudgetPtr(a6)
	bne.w budgetReady
	lea Local.Budget(a6), a0
	move.l a0, Local.BudgetPtr(a6)
budgetReady
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.budget
	bne.w finish
	movea.l abi.RouteFrame.ProgramPtr(a5), a0
	move.l abi.RouteFrame.ProgramBytes(a5), d0
	movea.l Local.BudgetPtr(a6), a2
	jsr policy.match
	bne.w finish
	move.l a1, Local.Policy(a6)
	movea.l abi.RouteFrame.TokenPtr(a5), a0
	move.l abi.RouteFrame.TokenCount(a5), d0
	movea.l abi.RouteFrame.ArenaPtr(a5), a1
	move.l abi.RouteFrame.ArenaBytes(a5), d1
	jsr facets.validate
	bne.w finish
	movea.l abi.RouteFrame.ScopePtr(a5), a0
	move.l abi.RouteFrame.ScopeCount(a5), d0
	movea.l abi.RouteFrame.KeywordsPtr(a5), a1
	move.l abi.RouteFrame.KeywordsBytes(a5), d1
	jsr scope.validate
	bne.w finish
	tst.l abi.RouteFrame.TokenCount(a5)
	beq.w absent
	movea.l abi.RouteFrame.TokenPtr(a5), a3
	cmpi.b #abi.K_REGISTER, (a3)  ; TokenView.Kind (zero offset)
	bhi.w absent
	move.l #abi.NONE, Local.LabelToken(a6)
	clr.l Local.HeadToken(a6)
	movea.l Local.Policy(a6), a0
	btst #1, abi.InvocationPolicy.Flags(a0)
	beq.w head
	cmpi.l #1, abi.TokenView.ColStart(a3)
	bne.w head
	clr.l Local.LabelToken(a6)
	addq.l #1, Local.HeadToken(a6)
	cmpi.l #1, abi.RouteFrame.TokenCount(a5)
	beq.w absent
	move.l abi.TokenView.ColEnd(a3), d4
	adda.w #abi.TOKEN_BYTES, a3
	cmpi.b #abi.K_COLON, (a3)
	bne.w head
	cmp.l abi.TokenView.ColStart(a3), d4
	bne.w head
	addq.l #1, Local.HeadToken(a6)
	adda.w #abi.TOKEN_BYTES, a3
head
	move.l Local.HeadToken(a6), d0
	cmp.l abi.RouteFrame.TokenCount(a5), d0
	bhs.w absent
	cmpi.b #abi.K_REGISTER, (a3)
	bhi.w absent
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movea.l abi.RouteFrame.ArenaPtr(a5), a0
	adda.l abi.TokenView.LiteralOffset(a3), a0
	move.l abi.TokenView.LiteralBytes(a3), d0
	movea.l abi.RouteFrame.ScopePtr(a5), a1
	move.l abi.RouteFrame.ScopeCount(a5), d1
	movea.l abi.RouteFrame.KeywordsPtr(a5), a2
	movea.l Local.Policy(a6), a3
	movea.l Local.BudgetPtr(a6), a4
	jsr scope.longest
	bne.w finish
	tst.l d1
	beq.w absent
	cmpi.l #abi.ROUTE_BYTES, abi.RouteFrame.OutputCapacity(a5)
	blo.w overflow
	movea.l a4, a0
	jsr inputs.tick
	bne.w finish
	movea.l abi.RouteFrame.OutputPtr(a5), a0
	move.l Local.LabelToken(a6), (a0)  ; Route.LabelToken (zero offset)
	move.l Local.HeadToken(a6), abi.Route.HeadToken(a0)
	move.l d1, abi.Route.PrefixBytes(a0)
	move.l Local.LabelToken(a6), d2
	move.l Local.HeadToken(a6), d3
	moveq #1, d1
	moveq #abi.OK, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	bra.w finish
absent
	moveq #abi.OK, d0
finish
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
done
	.TELEMETRY_VM_LEAVE
	lea LOCAL_BYTES(sp), sp
	tst.l d0
	movem.l (sp)+, d4-d7/a0-a6
	rts
	.bend
	.endsection
	.endmodule
