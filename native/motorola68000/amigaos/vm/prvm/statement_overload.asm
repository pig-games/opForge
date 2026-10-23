; Match one prepared-view pass against a scoped keyword's signature overloads.
; @opforge-owner: opforge.vm.prvm.statement_overload
	.module opforge.vm.prvm.statement_overload
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_inputs as inputs
	.use opforge.vm.prvm.statement_text_match as text
	.use opforge.vm.prvm.statement_match as matcher
	.pub
State	.struct
Frame	.long ?
Budget	.long ?
Policy	.long ?
Views	.long ?
ViewCount	.long ?
BestBuffer	.long ?
CaptureCapacity	.long ?
CandidateBuffer	.long ?
MatchWorkspace	.long ?
MatchCapacity	.long ?
BestScope	.long ?
BestPlan	.long ?
BestTemplate	.long ?
BestCount	.long ?
BestLiteralScore	.long ?
BestAtomScore	.long ?
Tied	.long ?
Matched	.long ?
	.endstruct
MATCH_FRAME_OFFSET = State.Matched+4
STATE_BYTES = MATCH_FRAME_OFFSET+abi.MATCH_FRAME_BYTES
	.section code, kind=code
; A0=caller-owned State with validated SelectFrame/spans and all PlanIds in range.
; D0=status/CCR,D1=matched,D2=best captures; others preserved. State/buffers are
; staging storage only. Selected-group plan semantics are owned by matcher.
run	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l (a6), a4  ; State.Frame (zero offset)
	movea.l abi.SelectFrame.ScopePtr(a4), a5
	move.l abi.SelectFrame.ScopeCount(a4), d7
	moveq #0, d6
	clr.l State.Tied(a6)
	clr.l State.Matched(a6)
next
	tst.l d7
	beq.w complete
	movea.l State.Budget(a6), a0
	jsr inputs.tick
	bne.w finish
	move.l abi.ScopeRow.KeywordBytes(a5), d0
	cmp.l abi.SelectFrame.PrefixBytes(a4), d0
	bne.w advance
	movea.l abi.SelectFrame.HeadPtr(a4), a0
	movea.l abi.SelectFrame.KeywordsPtr(a4), a1
	adda.l (a5), a1  ; ScopeRow.KeywordOffset (zero offset)
	move.l abi.ScopeRow.KeywordBytes(a5), d0
	movea.l State.Policy(a6), a2
	movea.l State.Budget(a6), a3
	jsr text.equal
	bne.w finish
	tst.l d1
	beq.w advance
	move.l abi.ScopeRow.PlanId(a5), d0
	lsl.l #3, d0  ; abi.PLAN_REF_BYTES
	movea.l abi.SelectFrame.PlanRefsPtr(a4), a2
	adda.l d0, a2
	lea MATCH_FRAME_OFFSET(a6), a3
	move.l abi.SelectFrame.ProgramPtr(a4), (a3)  ; MatchFrame.ProgramPtr (zero offset)
	move.l abi.SelectFrame.ProgramBytes(a4), abi.MatchFrame.ProgramBytes(a3)
	move.l (a2), abi.MatchFrame.PlanPtr(a3)  ; PlanRef.Ptr (zero offset)
	move.l abi.PlanRef.Bytes(a2), abi.MatchFrame.PlanBytes(a3)
	move.l State.Views(a6), abi.MatchFrame.TokenPtr(a3)
	move.l State.ViewCount(a6), abi.MatchFrame.TokenCount(a3)
	move.l abi.SelectFrame.ArenaPtr(a4), abi.MatchFrame.ArenaPtr(a3)
	move.l abi.SelectFrame.ArenaBytes(a4), abi.MatchFrame.ArenaBytes(a3)
	move.l State.CandidateBuffer(a6), abi.MatchFrame.OutputPtr(a3)
	move.l State.CaptureCapacity(a6), abi.MatchFrame.OutputCapacity(a3)
	move.l State.MatchWorkspace(a6), abi.MatchFrame.WorkspacePtr(a3)
	move.l State.MatchCapacity(a6), abi.MatchFrame.WorkspaceCapacity(a3)
	move.l abi.SelectFrame.StepBudget(a4), abi.MatchFrame.StepBudget(a3)
	movea.l a3, a0
	moveq #abi.MATCH_FRAME_BYTES, d0
	movea.l State.Budget(a6), a1
	jsr matcher.runBudget
	bne.w finish
	tst.l d1
	beq.w advance
	move.l d2, d5
	movea.l abi.MatchFrame.PlanPtr(a3), a2
	move.l abi.PlanHeader.LiteralScore(a2), d3
	move.l abi.PlanHeader.AtomScore(a2), d4
	tst.l State.Matched(a6)
	beq.w winner
	cmp.l State.BestLiteralScore(a6), d3
	bhi.w winner
	blo.w advance
	cmp.l State.BestAtomScore(a6), d4
	bhi.w winner
	blo.w advance
	move.l #1, State.Tied(a6)
	bra.w advance
winner
	movea.l State.Budget(a6), a0
	jsr inputs.tick
	bne.w finish
	move.l d6, State.BestScope(a6)
	move.l abi.ScopeRow.PlanId(a5), State.BestPlan(a6)
	move.l abi.ScopeRow.TemplateId(a5), State.BestTemplate(a6)
	move.l d5, State.BestCount(a6)
	move.l d3, State.BestLiteralScore(a6)
	move.l d4, State.BestAtomScore(a6)
	move.l #1, State.Matched(a6)
	clr.l State.Tied(a6)
	movea.l State.CandidateBuffer(a6), a0
	movea.l State.BestBuffer(a6), a1
	move.l d5, d0
	mulu.l #abi.CAPTURE_BYTES, d0
copy
	tst.l d0
	beq.w advance
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra.w copy
advance
	adda.w #abi.SCOPE_ROW_BYTES, a5
	addq.l #1, d6
	subq.l #1, d7
	bra.w next
complete
	tst.l State.Tied(a6)
	bne.w ambiguous
	move.l State.Matched(a6), d1
	moveq #0, d2
	tst.l d1
	beq.w success
	move.l State.BestCount(a6), d2
success
	moveq #abi.OK, d0
	bra.w done
ambiguous
	moveq #abi.INVALID_TOKEN, d0
finish
	moveq #0, d1
	moveq #0, d2
done
	tst.l d0
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend
	.endsection
	.endmodule
