; Own caller-visible statement rows; shared PRVM owns invocation semantics.
; @opforge-owner: opforge.assembler.language.statement_call
	.module opforge.assembler.language.statement_call
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.assembler.language.statements as store
	.use opforge.assembler.symbols.scopes as scopes
	.use opforge.assembler.symbols.imports as imports
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_inputs as inputs
	.use opforge.vm.prvm.statement_policy as policy
	.use opforge.vm.prvm.statement_text_match as text
	.use opforge.vm.prvm.statement_route as route
	.use opforge.vm.prvm.statement_select as selector
	.include "../../debug/telemetry_macros.i"
	.pub
BLOCK_BYTES = memory.Block.Used+4
State	.struct
Rows	.res BLOCK_BYTES
ImportOrders	.res BLOCK_BYTES
Keys	.res BLOCK_BYTES
Plans	.res BLOCK_BYTES
Head	.res BLOCK_BYTES
RowCount	.long ?
PlanCount	.long ?
Definitions	.long ?
Program	.long ?
ProgramBytes	.long ?
RemainingBudget	.long ?
Route	.res abi.ROUTE_BYTES
Found	.long ?
CallerScope	.long ?
Prepared	.long ?
	.endstruct
STATE_BYTES = State.Prepared+4
PrepareFrame	.struct
State	.long ?
Definitions	.long ?
CallerScope	.long ?
Program	.long ?
ProgramBytes	.long ?
HeadViews	.long ?
HeadCount	.long ?
HeadArena	.long ?
HeadArenaBytes	.long ?
StepBudget	.long ?
	.endstruct
PREPARE_FRAME_BYTES = PrepareFrame.StepBudget+4
SelectFrame	.struct
State	.long ?
Views	.long ?
Count	.long ?
Arena	.long ?
ArenaBytes	.long ?
Output	.long ?
OutputCapacity	.long ?
Fallback	.long ?
FallbackCapacity	.long ?
Workspace	.long ?
WorkspaceCapacity	.long ?
	.endstruct
SELECT_FRAME_BYTES = SelectFrame.WorkspaceCapacity+4
	.priv
IMPORT_ORDER_BYTES = 4
PHASE_IMPORTS = 0
PHASE_LOCALS = 1
Local	.struct
Policy	.long ?
Handle	.long ?
ClosedCount	.long ?
Phase	.long ?
BeforeKeyUsed	.long ?
Entry	.long ?
Plan	.long ?
PlanBytes	.long ?
Keyword	.long ?
KeywordBytes	.long ?
Name	.long ?
Template	.long ?
Visibility	.long ?
Cursor	.res imports.EXPOSURE_CURSOR_BYTES
Exposure	.res imports.TEMPLATE_EXPOSURE_FRAME_BYTES
Exposed	.res imports.EXPOSURE_ROW_BYTES
Route	.res abi.ROUTE_FRAME_BYTES
	.endstruct
LOCAL_BYTES = Local.Route+abi.ROUTE_FRAME_BYTES
	.pub
	.section code, kind=code
; A0=zeroed State or prior owned session. D0/CCR=zero; others preserved.
begin	.block
	bsr.w finish
	rts
	.bend  ; begin
; A0=State. Release only call-owner pools. Store/template ownership is external.
; D0/CCR=zero; others preserved. Borrowed plans expire on store mutation.
finish	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	moveq #4, d1
next
	jsr memory.release
	clr.l memory.Block.Used(a0)
	adda.w #BLOCK_BYTES, a0
	dbra d1, next
	lea State.RowCount(a1), a0
	moveq #(STATE_BYTES-State.RowCount)/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	moveq #abi.OK, d0
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; finish
; A0/D0=PrepareFrame/available bytes. D0=status/CCR,D1=found; others preserved.
; Rebuild private scope pools, then publish State.Route on a successful route.
; Inputs may not borrow this State's pools. Retained head bytes permit caller
; argument preparation to reuse its facet arena. No source acquisition occurs.
prepare	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_SELECTION
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l sp, a6
	moveq #PREPARE_FRAME_BYTES, d1
	bsr.w frame
	bne.w failed
	movea.l (a5), a4  ; PrepareFrame.State (zero offset)
	bsr.w prepareCheck
	bne.w failed
	bsr.w reset
	move.l PrepareFrame.Definitions(a5), State.Definitions(a4)
	move.l PrepareFrame.CallerScope(a5), State.CallerScope(a4)
	move.l PrepareFrame.Program(a5), State.Program(a4)
	move.l PrepareFrame.ProgramBytes(a5), State.ProgramBytes(a4)
	move.l PrepareFrame.StepBudget(a5), State.RemainingBudget(a4)
	movea.l State.Program(a4), a0
	move.l State.ProgramBytes(a4), d0
	lea State.RemainingBudget(a4), a2
	jsr policy.match
	bne.w ownedFailure
	move.l a1, (a6)  ; Local.Policy (zero offset)
	bsr.w build
	bne.w ownedFailure
	.TELEMETRY_COMPACT runtime_profile.compactTableRows, State.RowCount(a4)
	lea Local.Route(a6), a0
	move.l State.Program(a4), (a0)  ; RouteFrame.ProgramPtr (zero offset)
	move.l State.ProgramBytes(a4), abi.RouteFrame.ProgramBytes(a0)
	move.l PrepareFrame.HeadViews(a5), abi.RouteFrame.TokenPtr(a0)
	move.l PrepareFrame.HeadCount(a5), abi.RouteFrame.TokenCount(a0)
	move.l PrepareFrame.HeadArena(a5), abi.RouteFrame.ArenaPtr(a0)
	move.l PrepareFrame.HeadArenaBytes(a5), abi.RouteFrame.ArenaBytes(a0)
	move.l State.Rows+memory.Block.Pointer(a4), abi.RouteFrame.ScopePtr(a0)
	move.l State.RowCount(a4), abi.RouteFrame.ScopeCount(a0)
	move.l State.Keys+memory.Block.Pointer(a4), abi.RouteFrame.KeywordsPtr(a0)
	move.l State.Keys+memory.Block.Used(a4), abi.RouteFrame.KeywordsBytes(a0)
	lea State.Route(a4), a1
	move.l a1, abi.RouteFrame.OutputPtr(a0)
	move.l #abi.ROUTE_BYTES, abi.RouteFrame.OutputCapacity(a0)
	move.l State.RemainingBudget(a4), abi.RouteFrame.StepBudget(a0)
	lea State.RemainingBudget(a4), a1
	moveq #abi.ROUTE_FRAME_BYTES, d0
	jsr route.runBudget
	bne.w ownedFailure
	move.l d1, State.Found(a4)
	tst.l d1
	beq.w prepared
	bsr.w retainHead
	bne.w ownedFailure
prepared
	move.l #1, State.Prepared(a4)
	move.l State.Found(a4), d1
	moveq #abi.OK, d0
	bra.w done
ownedFailure
	clr.l State.Prepared(a4)
	clr.l State.Found(a4)
	clr.l State.Route+abi.Route.LabelToken(a4)
	clr.l State.Route+abi.Route.HeadToken(a4)
	clr.l State.Route+abi.Route.PrefixBytes(a4)
failed
	moveq #0, d1
done
	.TELEMETRY_SERVICE_LEAVE
	adda.w #LOCAL_BYTES, sp
	tst.l d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; prepare
; A0/D0=SelectFrame/available bytes. D0/status,D1/matched,D2/captures,D3/fallback
; views; other registers preserved. Selection.TemplateId is the external body
; carrier; PlanId indexes closed declarations. Numeric captures address its plan
; and selected views. Caller owns output buffers and packs/substitutes arguments.
select	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_SELECTION
	movem.l d4-d7/a0-a6, -(sp)
	suba.w #abi.SELECT_FRAME_BYTES, sp
	movea.l a0, a5
	movea.l sp, a6
	moveq #SELECT_FRAME_BYTES, d1
	bsr.w frame
	bne.w failed
	movea.l (a5), a4  ; SelectFrame.State (zero offset)
	bsr.w checkState
	bne.w failed
	tst.l State.Prepared(a4)
	beq.w invalid
	bsr.w selectCheck
	bne.w failed
	tst.l State.Found(a4)
	beq.w absent
	move.l State.Program(a4), (a6)  ; PRVM SelectFrame.ProgramPtr (zero offset)
	move.l State.ProgramBytes(a4), abi.SelectFrame.ProgramBytes(a6)
	move.l State.Rows+memory.Block.Pointer(a4), abi.SelectFrame.ScopePtr(a6)
	move.l State.RowCount(a4), abi.SelectFrame.ScopeCount(a6)
	move.l State.Keys+memory.Block.Pointer(a4), abi.SelectFrame.KeywordsPtr(a6)
	move.l State.Keys+memory.Block.Used(a4), abi.SelectFrame.KeywordsBytes(a6)
	move.l State.Plans+memory.Block.Pointer(a4), abi.SelectFrame.PlanRefsPtr(a6)
	move.l State.PlanCount(a4), abi.SelectFrame.PlanRefsCount(a6)
	move.l State.Head+memory.Block.Pointer(a4), abi.SelectFrame.HeadPtr(a6)
	move.l State.Head+memory.Block.Used(a4), abi.SelectFrame.HeadBytes(a6)
	move.l State.Route+abi.Route.PrefixBytes(a4), abi.SelectFrame.PrefixBytes(a6)
	move.l SelectFrame.Views(a5), abi.SelectFrame.TokenPtr(a6)
	move.l SelectFrame.Count(a5), abi.SelectFrame.TokenCount(a6)
	move.l SelectFrame.Arena(a5), abi.SelectFrame.ArenaPtr(a6)
	move.l SelectFrame.ArenaBytes(a5), abi.SelectFrame.ArenaBytes(a6)
	move.l SelectFrame.Output(a5), abi.SelectFrame.OutputPtr(a6)
	move.l SelectFrame.OutputCapacity(a5), abi.SelectFrame.OutputCapacity(a6)
	move.l SelectFrame.Fallback(a5), abi.SelectFrame.FallbackPtr(a6)
	move.l SelectFrame.FallbackCapacity(a5), abi.SelectFrame.FallbackCapacity(a6)
	move.l SelectFrame.Workspace(a5), abi.SelectFrame.WorkspacePtr(a6)
	move.l SelectFrame.WorkspaceCapacity(a5), abi.SelectFrame.WorkspaceCapacity(a6)
	move.l State.RemainingBudget(a4), abi.SelectFrame.StepBudget(a6)
	movea.l a6, a0
	moveq #abi.SELECT_FRAME_BYTES, d0
	lea State.RemainingBudget(a4), a1
	jsr selector.runBudget
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w failed
absent
	moveq #abi.OK, d0
failed
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
done
	.TELEMETRY_SERVICE_LEAVE
	adda.w #abi.SELECT_FRAME_BYTES, sp
	tst.l d0
	movem.l (sp)+, d4-d7/a0-a6
	rts
	.bend  ; select
; A0=prepared State,D1=Selection.PlanId. D0=status/CCR; D1 preserved.
; On success, returns store.view's borrowed outputs; invalid IDs clear them.
; The closed declaration index maps directly, without name-based resolution.
chosen	.block
	movem.l d1/a0, -(sp)
	cmp.l State.PlanCount(a0), d1
	bhs.w invalid
	addq.l #1, d1
	movea.l State.Definitions(a0), a0
	jsr store.view
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	moveq #0, d2
	moveq #0, d3
	suba.l a1, a1
	suba.l a2, a2
	suba.l a3, a3
done
	tst.l d0
	movem.l (sp)+, d1/a0
	rts
	.bend  ; chosen
	.priv
; Retain only the selected prepared head, never original source text.
retainHead	.block
	move.l State.Route+abi.Route.HeadToken(a4), d0
	lsl.l #5, d0
	movea.l PrepareFrame.HeadViews(a5), a0
	adda.l d0, a0
	move.l abi.TokenView.LiteralBytes(a0), d2
	cmpi.l #memory.LIMIT, d2
	bhi.w overflow
	cmp.l State.RemainingBudget(a4), d2
	bhi.w exhausted
	movea.l PrepareFrame.HeadArena(a5), a3
	adda.l abi.TokenView.LiteralOffset(a0), a3
	lea State.Head(a4), a0
	move.l d2, d0
	jsr memory.reserve
	bne.w storage
	sub.l d2, State.RemainingBudget(a4)
	movea.l State.Head+memory.Block.Pointer(a4), a1
	move.l d2, d0
copy
	tst.l d0
	beq.w retained
	move.b (a3)+, (a1)+
	subq.l #1, d0
	bra.w copy
retained
	move.l d2, State.Head+memory.Block.Used(a4)
	moveq #abi.OK, d0
	rts
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	rts
exhausted
	moveq #abi.BUDGET_EXCEEDED, d0
	rts
storage
	moveq #store.STORAGE_FAILURE, d0
	rts
	.bend  ; retainHead
	.priv
	.include "statement_call_scope.i"
	.include "statement_call_check.i"
	.endsection
	.endmodule
