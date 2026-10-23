; Caller ownership controls for shared A9 budget cells, using current fixtures.
; @opforge-owner: opforge.tests.assembler.prvm_stmt_budget_harness
	.module opforge.tests.assembler.prvm_stmt_budget_harness
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_route as route
	.use opforge.vm.prvm.statement_select as selector
	.pub
ROUTE = 6
SELECT = 8
	.section code, kind=code
; A0=successful current RouteFrame/SelectFrame,D0=frame bytes,D1=kind above.
; D0/status (zero pass,4 invariant failure); others preserved. Controls never
; alter semantic inputs. Mutable output/workspace words are restored exactly.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	move.l d0, d6
	move.l d1, d7
	clr.l -(sp)
	movea.l sp, a1
	moveq #abi.BUDGET_EXCEEDED, d5
	bsr.w probe
	addq.l #4, sp
	bne.w done
	cmpi.l #ROUTE, d7
	bne.w selectFrame
	lea abi.RouteFrame.StepBudget(a4), a1
	bra.w frameCell
selectFrame
	lea abi.SelectFrame.StepBudget(a4), a1
frameCell
	moveq #abi.INVALID_ARGUMENT, d5
	bsr.w probe
	bne.w done
	movea.l (a4), a1  ; both frames' ProgramPtr (zero offset)
	adda.w #abi.MATCH_HEADER_BYTES+abi.TypePolicy.Identifier, a1
	bsr.w probe
	bne.w done
	cmpi.l #ROUTE, d7
	bne.w selectInputs
	tst.l abi.RouteFrame.TokenCount(a4)
	beq.w routeScope
	movea.l abi.RouteFrame.TokenPtr(a4), a1
	adda.w #abi.TokenView.ColStart, a1
	bsr.w probe
	bne.w done
routeScope
	tst.l abi.RouteFrame.ScopeCount(a4)
	beq.w routeOutput
	movea.l abi.RouteFrame.ScopePtr(a4), a1
	bsr.w probe
	bne.w done
routeOutput
	cmpi.l #4, abi.RouteFrame.OutputCapacity(a4)
	blo.w passed
	movea.l abi.RouteFrame.OutputPtr(a4), a1
	bsr.w mutable
	bra.w done
selectInputs
	tst.l abi.SelectFrame.TokenCount(a4)
	beq.w selectScope
	movea.l abi.SelectFrame.TokenPtr(a4), a1
	adda.w #abi.TokenView.ColStart, a1
	bsr.w probe
	bne.w done
selectScope
	tst.l abi.SelectFrame.ScopeCount(a4)
	beq.w plan
	movea.l abi.SelectFrame.ScopePtr(a4), a1
	bsr.w probe
	bne.w done
plan
	tst.l abi.SelectFrame.PlanRefsCount(a4)
	beq.w selectOutput
	movea.l abi.SelectFrame.PlanRefsPtr(a4), a1
	bsr.w probe  ; immutable descriptor array
	bne.w done
	movea.l abi.SelectFrame.PlanRefsPtr(a4), a1
	cmpi.l #4, abi.PlanRef.Bytes(a1)
	blo.w selectOutput
	movea.l (a1), a1  ; PlanRef.Ptr / immutable extent word
	move.l (a1), d0
	cmp.l abi.SelectFrame.PlanRefsCount(a4), d0
	blo.w selectOutput  ; do not mask overlap with an earlier exhaustion preflight
	bsr.w probe
	bne.w done
selectOutput
	cmpi.l #4, abi.SelectFrame.OutputCapacity(a4)
	blo.w fallbackOutput
	movea.l abi.SelectFrame.OutputPtr(a4), a1
	bsr.w mutable
	bne.w done
fallbackOutput
	cmpi.l #4, abi.SelectFrame.FallbackCapacity(a4)
	blo.w workspace
	movea.l abi.SelectFrame.FallbackPtr(a4), a1
	bsr.w mutable
	bne.w done
workspace
	cmpi.l #4, abi.SelectFrame.WorkspaceCapacity(a4)
	blo.w passed
	movea.l abi.SelectFrame.WorkspacePtr(a4), a1
	bsr.w mutable
	bra.w done
passed
	moveq #abi.OK, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; run
	.priv
; A1=caller-owned mutable word. Seed a valid positive cell, restore every byte.
mutable	.block
	move.l (a1), -(sp)
	move.l #1000000, (a1)
	move.l a1, -(sp)
	bsr.w probe
	movea.l (sp)+, a1
	move.l (sp)+, (a1)
	tst.l d0
	rts
	.bend  ; mutable
; A1=shared cell,D5=expected status. Require exact rejection AND no cell mutation.
probe	.block
	movea.l a1, a5
	move.l (a5), -(sp)
	movea.l a4, a0
	move.l d6, d0
	cmpi.l #ROUTE, d7
	bne.w select
	jsr route.runBudget
	bra.w returned
select
	jsr selector.runBudget
returned
	cmp.l d5, d0
	bne.w failed
	move.l (sp), d0
	cmp.l (a5), d0
	bne.w failed
	moveq #abi.OK, d0
	bra.w done
failed
	moveq #abi.INVALID_ARGUMENT, d0
done
	addq.l #4, sp
	tst.l d0
	rts
	.bend  ; probe
	.endsection
	.endmodule
