; Current invocation fixture layout and numeric-only service adapter.
; @opforge-owner: test.amigaos.statement_invocation_cases
	.module test.amigaos.statement_invocation_cases
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_route as route
	.use prvm.amigaos.statement_fallback as fallback
	.use prvm.amigaos.statement_select as selector
	.use prvm.amigaos.statement_invoke_req as request
	.use prvm.amigaos.statement_policy as policy
	.use prvm.amigaos.statement_scope as scope
	.use prvm.amigaos.statement_text as text
	.use test.amigaos.statement_budget as budgetTests
	.pub
Header	.struct
Mode	.long ?
Budget	.long ?
OutputCapacity	.long ?
WorkspaceCapacity	.long ?
ProgramBytes	.long ?
PlanBytes	.long ?
TokenCount	.long ?
ArenaBytes	.long ?
NamesCount	.long ?
ScopeCount	.long ?
KeywordsBytes	.long ?
PlanCount	.long ?
HeadBytes	.long ?
	.endstruct
HEADER_BYTES = Header.HeadBytes+4
Context	.struct
Program	.long ?
Tokens	.long ?
Arena	.long ?
Scope	.long ?
Keywords	.long ?
Plans	.long ?
Head	.long ?
Output	.long ?
Fallback	.long ?
Workspace	.long ?
	.endstruct
CONTEXT_BYTES = Context.Workspace+4
CASE_ROUTE = 6
CASE_FALLBACK = 7
CASE_SELECT = 8
	.priv
PREFIX_BUDGET_OFFSET = abi.SELECT_FRAME_BYTES
SHARED_BUDGET_OFFSET = PREFIX_BUDGET_OFFSET+4
LOCAL_BYTES = SHARED_BUDGET_OFFSET+4
	.pub
	.section code, kind=code
; A0=Context,A1=Header52. D0/status,D1/primary,D2/secondary,D3/tertiary.
; Other registers preserved. Fixture prefix derivation uses the SAME validated
; scope primitive; selector independently verifies longest prefix and its budget.
run	.block
	movem.l d4-d7/a0-a6, -(sp)
	lea -LOCAL_BYTES(sp), sp
	movea.l sp, a6
	movea.l a0, a4
	movea.l a1, a5
	cmpi.l #CASE_ROUTE, (a5)  ; Header.Mode (zero offset)
	beq.w routeCase
	cmpi.l #CASE_FALLBACK, (a5)
	beq.w fallbackCase
	cmpi.l #CASE_SELECT, (a5)
	beq.w selectCase
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w failed
routeCase
	move.l (a4), (a6)  ; Context.Program / RouteFrame.ProgramPtr (zero offsets)
	move.l Header.ProgramBytes(a5), abi.RouteFrame.ProgramBytes(a6)
	move.l Context.Tokens(a4), abi.RouteFrame.TokenPtr(a6)
	move.l Header.TokenCount(a5), abi.RouteFrame.TokenCount(a6)
	move.l Context.Arena(a4), abi.RouteFrame.ArenaPtr(a6)
	move.l Header.ArenaBytes(a5), abi.RouteFrame.ArenaBytes(a6)
	move.l Context.Scope(a4), abi.RouteFrame.ScopePtr(a6)
	move.l Header.ScopeCount(a5), abi.RouteFrame.ScopeCount(a6)
	move.l Context.Keywords(a4), abi.RouteFrame.KeywordsPtr(a6)
	move.l Header.KeywordsBytes(a5), abi.RouteFrame.KeywordsBytes(a6)
	move.l Context.Output(a4), abi.RouteFrame.OutputPtr(a6)
	move.l Header.OutputCapacity(a5), abi.RouteFrame.OutputCapacity(a6)
	move.l Header.Budget(a5), abi.RouteFrame.StepBudget(a6)
	movea.l a6, a0
	moveq #abi.ROUTE_FRAME_BYTES, d0
	move.l Header.Budget(a5), SHARED_BUDGET_OFFSET(a6)
	lea SHARED_BUDGET_OFFSET(a6), a1
	jsr route.runBudget
	bsr.w controls
	bra.w done
fallbackCase
	move.l (a4), (a6)  ; Context.Program / FallbackFrame.ProgramPtr (zero offsets)
	move.l Header.ProgramBytes(a5), abi.FallbackFrame.ProgramBytes(a6)
	move.l Context.Tokens(a4), abi.FallbackFrame.TokenPtr(a6)
	move.l Header.TokenCount(a5), abi.FallbackFrame.TokenCount(a6)
	move.l Context.Arena(a4), abi.FallbackFrame.ArenaPtr(a6)
	move.l Header.ArenaBytes(a5), abi.FallbackFrame.ArenaBytes(a6)
	move.l Context.Output(a4), abi.FallbackFrame.OutputPtr(a6)
	move.l Header.OutputCapacity(a5), abi.FallbackFrame.OutputCapacity(a6)
	move.l Context.Workspace(a4), abi.FallbackFrame.WorkspacePtr(a6)
	move.l Header.WorkspaceCapacity(a5), abi.FallbackFrame.WorkspaceCapacity(a6)
	move.l Header.Budget(a5), abi.FallbackFrame.StepBudget(a6)
	movea.l a6, a0
	moveq #abi.FALLBACK_FRAME_BYTES, d0
	jsr fallback.run
	moveq #0, d3
	bra.w done
selectCase
	move.l (a4), (a6)  ; Context.Program / SelectFrame.ProgramPtr (zero offsets)
	move.l Header.ProgramBytes(a5), abi.SelectFrame.ProgramBytes(a6)
	move.l Context.Scope(a4), abi.SelectFrame.ScopePtr(a6)
	move.l Header.ScopeCount(a5), abi.SelectFrame.ScopeCount(a6)
	move.l Context.Keywords(a4), abi.SelectFrame.KeywordsPtr(a6)
	move.l Header.KeywordsBytes(a5), abi.SelectFrame.KeywordsBytes(a6)
	move.l Context.Plans(a4), abi.SelectFrame.PlanRefsPtr(a6)
	move.l Header.PlanCount(a5), abi.SelectFrame.PlanRefsCount(a6)
	move.l Context.Head(a4), abi.SelectFrame.HeadPtr(a6)
	move.l Header.HeadBytes(a5), abi.SelectFrame.HeadBytes(a6)
	clr.l abi.SelectFrame.PrefixBytes(a6)
	move.l Context.Tokens(a4), abi.SelectFrame.TokenPtr(a6)
	move.l Header.TokenCount(a5), abi.SelectFrame.TokenCount(a6)
	move.l Context.Arena(a4), abi.SelectFrame.ArenaPtr(a6)
	move.l Header.ArenaBytes(a5), abi.SelectFrame.ArenaBytes(a6)
	move.l Context.Output(a4), abi.SelectFrame.OutputPtr(a6)
	move.l Header.OutputCapacity(a5), abi.SelectFrame.OutputCapacity(a6)
	move.l Context.Fallback(a4), abi.SelectFrame.FallbackPtr(a6)
	move.l Header.OutputCapacity(a5), abi.SelectFrame.FallbackCapacity(a6)
	move.l Context.Workspace(a4), abi.SelectFrame.WorkspacePtr(a6)
	move.l Header.WorkspaceCapacity(a5), abi.SelectFrame.WorkspaceCapacity(a6)
	move.l Header.Budget(a5), abi.SelectFrame.StepBudget(a6)
	movea.l a6, a0
	moveq #abi.SELECT_FRAME_BYTES, d0
	jsr request.select
	bne.w failed
	move.l #1000000, PREFIX_BUDGET_OFFSET(a6)
	lea PREFIX_BUDGET_OFFSET(a6), a2
	movea.l (a6), a0  ; SelectFrame.ProgramPtr (zero offset)
	move.l abi.SelectFrame.ProgramBytes(a6), d0
	jsr policy.match
	bne.w failed
	movea.l a1, a3
	movea.l abi.SelectFrame.ScopePtr(a6), a0
	move.l abi.SelectFrame.ScopeCount(a6), d0
	movea.l abi.SelectFrame.KeywordsPtr(a6), a1
	move.l abi.SelectFrame.KeywordsBytes(a6), d1
	jsr scope.validate
	bne.w failed
	movea.l abi.SelectFrame.HeadPtr(a6), a0
	move.l abi.SelectFrame.HeadBytes(a6), d0
	movea.l a2, a1
	jsr text.validate
	bne.w failed
	movea.l abi.SelectFrame.HeadPtr(a6), a0
	move.l abi.SelectFrame.HeadBytes(a6), d0
	movea.l abi.SelectFrame.ScopePtr(a6), a1
	move.l abi.SelectFrame.ScopeCount(a6), d1
	movea.l abi.SelectFrame.KeywordsPtr(a6), a2
	lea PREFIX_BUDGET_OFFSET(a6), a4
	jsr scope.longest
	bne.w failed
	move.l d1, abi.SelectFrame.PrefixBytes(a6)
	movea.l a6, a0
	moveq #abi.SELECT_FRAME_BYTES, d0
	move.l Header.Budget(a5), SHARED_BUDGET_OFFSET(a6)
	lea SHARED_BUDGET_OFFSET(a6), a1
	jsr selector.runBudget
	bsr.w controls
	bra.w done
failed
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
done
	lea LOCAL_BYTES(sp), sp
	tst.l d0
	movem.l (sp)+, d4-d7/a0-a6
	rts
	.bend
; Exercise public shared cells on each successful current semantic case.
; Preserve the live result; failed invariants become a detectable caller error.
controls	.block
	tst.l d0
	bne.w done
	movem.l d0-d3, -(sp)
	move.l SHARED_BUDGET_OFFSET(a6), d0
	cmp.l Header.Budget(a5), d0
	bhi.w failed
	movea.l a6, a0
	move.l Header.Mode(a5), d1
	moveq #abi.ROUTE_FRAME_BYTES, d0
	cmpi.l #CASE_ROUTE, d1
	beq.w ready
	moveq #abi.SELECT_FRAME_BYTES, d0
ready
	jsr budgetTests.run
	bne.w failed
	movem.l (sp)+, d0-d3
	bra.w done
failed
	movem.l (sp)+, d0-d3
	moveq #abi.INVALID_ARGUMENT, d0
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
done
	tst.l d0
	rts
	.bend  ; controls
	.endsection
	.endmodule
