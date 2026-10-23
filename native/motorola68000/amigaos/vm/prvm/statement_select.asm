; Select scoped overloads, normal views first, then package numeric fallback.
; @opforge-owner: opforge.vm.prvm.statement_select
	.module opforge.vm.prvm.statement_select
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_inputs as inputs
	.use opforge.vm.prvm.statement_facets as facets
	.use opforge.vm.prvm.statement_policy as policy
	.use opforge.vm.prvm.statement_scope as scope
	.use opforge.vm.prvm.statement_text as text
	.use opforge.vm.prvm.statement_fallback as fallback
	.use opforge.vm.prvm.statement_overload as pass
	.use opforge.vm.prvm.statement_invoke_req as request
	.include "telemetry_macros.i"
	.priv
Local	.struct
Budget	.long ?
BudgetPtr	.long ?
Policy	.long ?
DerivedPtr	.long ?
DerivedCapacity	.long ?
FallbackCount	.long ?
CaptureBytes	.long ?
OutputBytes	.long ?
FallbackUsed	.long ?
	.endstruct
STATE_OFFSET = Local.FallbackUsed+4
FALLBACK_FRAME_OFFSET = STATE_OFFSET+pass.STATE_BYTES
LOCAL_BYTES = FALLBACK_FRAME_OFFSET+abi.FALLBACK_FRAME_BYTES
WORK_BYTES_PER_INPUT = 2*abi.TOKEN_BYTES+4*abi.CAPTURE_BYTES
MAX_WORK_INPUTS = $7fffffff/WORK_BYTES_PER_INPUT
	.pub
	.section code, kind=code
; A0/D0=SelectFrame/available bytes. D0=status/CCR,D1=matched,D2=captures,
; D3=fallback views. Others preserved. Publishes Selection20+Capture12 rows,
; plus FallbackPtr views only for a winning fallback. No-match/errors publish none.
; Workspace contract: reserve 64 bytes per input for derived views, 24 each for
; candidate/best captures, then remaining bytes for matcher validation/staging.
; This deliberate upper bound is not a claim of minimum workspace requirements.
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
	movea.l a0, a4
	jsr request.selectBudget
	bne.w finish
	move.l abi.SelectFrame.StepBudget(a4), (a6)  ; Local.Budget (zero offset)
	move.l a1, Local.BudgetPtr(a6)
	tst.l Local.BudgetPtr(a6)
	bne.w budgetReady
	move.l a6, Local.BudgetPtr(a6)
budgetReady
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.budget
	bne.w finish
	move.l abi.SelectFrame.PlanRefsCount(a4), d0
	cmp.l (a0), d0
	bhi.w exhausted
	sub.l d0, (a0)  ; structural plan-span preflight is charged once
	movea.l abi.SelectFrame.ProgramPtr(a4), a0
	move.l abi.SelectFrame.ProgramBytes(a4), d0
	movea.l Local.BudgetPtr(a6), a2
	jsr policy.match
	bne.w finish
	move.l a1, Local.Policy(a6)
	movea.l abi.SelectFrame.TokenPtr(a4), a0
	move.l abi.SelectFrame.TokenCount(a4), d0
	movea.l abi.SelectFrame.ArenaPtr(a4), a1
	move.l abi.SelectFrame.ArenaBytes(a4), d1
	jsr facets.validate
	bne.w finish
	movea.l abi.SelectFrame.ScopePtr(a4), a0
	move.l abi.SelectFrame.ScopeCount(a4), d0
	movea.l abi.SelectFrame.KeywordsPtr(a4), a1
	move.l abi.SelectFrame.KeywordsBytes(a4), d1
	jsr scope.validate
	bne.w finish
	movea.l abi.SelectFrame.HeadPtr(a4), a0
	move.l abi.SelectFrame.HeadBytes(a4), d0
	movea.l Local.BudgetPtr(a6), a1
	jsr text.validate
	bne.w finish
	movea.l abi.SelectFrame.ScopePtr(a4), a3
	move.l abi.SelectFrame.ScopeCount(a4), d7
ids
	tst.l d7
	beq.w prefix
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.tick
	bne.w finish
	move.l abi.ScopeRow.PlanId(a3), d0
	cmp.l abi.SelectFrame.PlanRefsCount(a4), d0
	bhs.w invalid
	adda.w #abi.SCOPE_ROW_BYTES, a3
	subq.l #1, d7
	bra.w ids
prefix
	movea.l abi.SelectFrame.HeadPtr(a4), a0
	move.l abi.SelectFrame.HeadBytes(a4), d0
	movea.l abi.SelectFrame.ScopePtr(a4), a1
	move.l abi.SelectFrame.ScopeCount(a4), d1
	movea.l abi.SelectFrame.KeywordsPtr(a4), a2
	movea.l Local.Policy(a6), a3
	move.l a4, -(sp)
	movea.l Local.BudgetPtr(a6), a4
	jsr scope.longest
	movea.l (sp)+, a4
	bne.w finish
	cmp.l abi.SelectFrame.PrefixBytes(a4), d1
	bne.w invalid
	tst.l d1
	beq.w absent
	move.l abi.SelectFrame.TokenCount(a4), d7
	cmpi.l #MAX_WORK_INPUTS, d7
	bhi.w overflow
	move.l d7, d0
	mulu.l #WORK_BYTES_PER_INPUT, d0
	cmp.l abi.SelectFrame.WorkspaceCapacity(a4), d0
	bhi.w overflow
	lea STATE_OFFSET(a6), a5
	move.l a4, (a5)  ; State.Frame (zero offset)
	move.l Local.BudgetPtr(a6), pass.State.Budget(a5)
	move.l Local.Policy(a6), pass.State.Policy(a5)
	move.l abi.SelectFrame.TokenPtr(a4), pass.State.Views(a5)
	move.l d7, pass.State.ViewCount(a5)
	movea.l abi.SelectFrame.WorkspacePtr(a4), a0
	move.l a0, Local.DerivedPtr(a6)
	move.l d7, d0
	lsl.l #6, d0  ; two abi.TOKEN_BYTES views per input
	move.l d0, Local.DerivedCapacity(a6)
	adda.l d0, a0
	move.l a0, pass.State.BestBuffer(a5)
	move.l d7, d0
	mulu.l #2*abi.CAPTURE_BYTES, d0
	move.l d0, pass.State.CaptureCapacity(a5)
	adda.l d0, a0
	move.l a0, pass.State.CandidateBuffer(a5)
	adda.l d0, a0
	move.l a0, pass.State.MatchWorkspace(a5)
	move.l abi.SelectFrame.WorkspaceCapacity(a4), d0
	move.l a0, d1
	sub.l abi.SelectFrame.WorkspacePtr(a4), d1
	sub.l d1, d0
	move.l d0, pass.State.MatchCapacity(a5)
	clr.l Local.FallbackUsed(a6)
	clr.l Local.FallbackCount(a6)
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movea.l a5, a0
	jsr pass.run
	bne.w finish
	tst.l d1
	bne.w publish
	lea FALLBACK_FRAME_OFFSET(a6), a3
	move.l abi.SelectFrame.ProgramPtr(a4), (a3)  ; FallbackFrame.ProgramPtr (zero offset)
	move.l abi.SelectFrame.ProgramBytes(a4), abi.FallbackFrame.ProgramBytes(a3)
	move.l abi.SelectFrame.TokenPtr(a4), abi.FallbackFrame.TokenPtr(a3)
	move.l abi.SelectFrame.TokenCount(a4), abi.FallbackFrame.TokenCount(a3)
	move.l abi.SelectFrame.ArenaPtr(a4), abi.FallbackFrame.ArenaPtr(a3)
	move.l abi.SelectFrame.ArenaBytes(a4), abi.FallbackFrame.ArenaBytes(a3)
	move.l abi.SelectFrame.FallbackPtr(a4), abi.FallbackFrame.OutputPtr(a3)
	move.l abi.SelectFrame.FallbackCapacity(a4), abi.FallbackFrame.OutputCapacity(a3)
	move.l Local.DerivedPtr(a6), abi.FallbackFrame.WorkspacePtr(a3)
	move.l Local.DerivedCapacity(a6), abi.FallbackFrame.WorkspaceCapacity(a3)
	move.l abi.SelectFrame.StepBudget(a4), abi.FallbackFrame.StepBudget(a3)
	movea.l a3, a0
	moveq #abi.FALLBACK_FRAME_BYTES, d0
	movea.l Local.BudgetPtr(a6), a1
	jsr fallback.stage
	bne.w finish
	move.l d1, Local.FallbackCount(a6)
	move.l Local.DerivedPtr(a6), pass.State.Views(a5)
	move.l d1, pass.State.ViewCount(a5)
	move.l #1, Local.FallbackUsed(a6)
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	movea.l a5, a0
	jsr pass.run
	bne.w finish
	tst.l d1
	beq.w absent
publish
	move.l pass.State.BestCount(a5), d0
	mulu.l #abi.CAPTURE_BYTES, d0
	move.l d0, Local.CaptureBytes(a6)
	addi.l #abi.SELECTION_BYTES, d0
	move.l d0, Local.OutputBytes(a6)
	cmp.l abi.SelectFrame.OutputCapacity(a4), d0
	bhi.w overflow
	move.l Local.FallbackCount(a6), d0
	lsl.l #5, d0  ; abi.TOKEN_BYTES
	cmp.l abi.SelectFrame.FallbackCapacity(a4), d0
	bhi.w overflow
	movea.l Local.BudgetPtr(a6), a0
	jsr inputs.tick
	bne.w finish
	movea.l abi.SelectFrame.OutputPtr(a4), a1
	move.l pass.State.BestScope(a5), (a1)+  ; Selection.ScopeIndex (zero offset)
	move.l pass.State.BestPlan(a5), (a1)+
	move.l pass.State.BestTemplate(a5), (a1)+
	move.l Local.FallbackUsed(a6), (a1)+
	move.l pass.State.BestCount(a5), (a1)+
	movea.l pass.State.BestBuffer(a5), a0
	move.l Local.CaptureBytes(a6), d0
copyCaptures
	tst.l d0
	beq.w fallbackOutput
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra.w copyCaptures
fallbackOutput
	movea.l Local.DerivedPtr(a6), a0
	movea.l abi.SelectFrame.FallbackPtr(a4), a1
	move.l Local.FallbackCount(a6), d0
	lsl.l #5, d0
copyViews
	tst.l d0
	beq.w matched
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra.w copyViews
matched
	moveq #abi.OK, d0
	moveq #1, d1
	move.l pass.State.BestCount(a5), d2
	move.l Local.FallbackCount(a6), d3
	bra.w done
exhausted
	moveq #abi.BUDGET_EXCEEDED, d0
	bra.w finish
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	bra.w finish
invalid
	moveq #abi.INVALID_ARGUMENT, d0
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
