; First-use remapping keeps the declaration writer and its numeric identities.
.if DEFER_MACRO_PLANS
DEFER_EAGER = 2
DEFER_BLOCK_BYTES = memory.Block.Used+4
	.priv
; A6=scratch. Inspect the same immutable VM recipe before deferred publication;
; numeric suffix selection needs spans, never a newly copied descriptor.
; D0=RECIPE_* status,A1=rows,D1=count,D2=spelling base; other registers kept.
capturedLineRecipes	.block
.if DEFER_MACRO_LINES
	move.l a1, -(sp)
	movea.l CAPTURE_RECORD(a6), a1
	cmpi.l #capture.LINE_PROOF, capture.Record.LineStatus(a1)
	movea.l (sp)+, a1
	bne.w ordinaryLineProof
	bra.w proofRecipes
ordinaryLineProof
.endif
	movem.l d3/a0, -(sp)
	movea.l CAPTURE_RECORD(a6), a1
	move.l a1, d0
	beq.w bad
	tst.l capture.Record.LineStatus(a1)
	bne.w bad
	move.l capture.Record.LineHandle(a1), d3
	suba.w #DEFER_BLOCK_BYTES, sp
	jsr capture.planView
	move.l a0, memory.Block.Pointer(sp)
	move.l d0, memory.Block.Capacity(sp)
	move.l d0, memory.Block.Used(sp)
	movea.l sp, a0
	move.l d3, d1
	jsr macro_plans.recipeView
	adda.w #DEFER_BLOCK_BYTES, sp
	bra.w done
bad
	moveq #macro_plans.RECIPE_INVALID, d0
done
	movem.l (sp)+, d3/a0
	tst.l d0
	rts
	.bend  ; capturedLineRecipes

; A5=session,A6=scratch,D4=candidate role. D0=status or DEFER_EAGER,
; D1=tagged recipe on success. Other registers preserved. Physical/generated
; input without an immutable capture owner retains the eager implementation.
deferCapturedPlan	.block
	movem.l d2-d7/a0-a4, -(sp)
	tst.l Frame.CaptureResolve(a5)
	beq.w eager
	tst.l CAPTURE_RECORD(a6)
	beq.w eager
	cmpi.l #1, d4
	bhi.w eager
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	jsr templates.openDef
	bne.w eager
	cmpi.w #templates.KIND_MACRO, templates.Def.Kind(a1)
	bne.w eager
	movea.l CAPTURE_RECORD(a6), a4
	tst.l d4
	beq.w linePlan
	tst.l capture.Record.CallStatus(a4)
	bne.w bad
	tst.l capture.Record.CallRecipeStatus(a4)
	bne.w bad
	move.l capture.Record.CallHandle(a4), d6
	bra.w selected
linePlan
	tst.l capture.Record.LineStatus(a4)
	bne.w bad
	move.l capture.Record.LineHandle(a4), d6
selected
	tst.l d6
	beq.w bad
	suba.w #DEFER_BLOCK_BYTES+deferred.CREATE_FRAME_BYTES, sp
	movea.l a4, a1
	jsr capture.planView
	move.l a0, memory.Block.Pointer(sp)
	move.l d0, memory.Block.Capacity(sp)
	move.l d0, memory.Block.Used(sp)
	lea DEFER_BLOCK_BYTES(sp), a0
	movea.l a6, a1
	adda.l #TEMPLATE_STATE+templates.DEFERRED, a1
	move.l a1, deferred.CreateFrame.Arena(a0)
	move.l sp, deferred.CreateFrame.Source(a0)
	move.l Frame.CaptureOffset(a5), deferred.CreateFrame.Capture(a0)
	move.l d6, deferred.CreateFrame.Handle(a0)
	lea PACKED_MAP(a6), a1
	move.l a1, deferred.CreateFrame.PackedMap(a0)
	move.l LINE_FRAME+writer.Frame.Count(a6), deferred.CreateFrame.TokenCount(a0)
	move.w #1, deferred.CreateFrame.IncludeRecipes(a0)
	clr.w deferred.CreateFrame.Reserved(a0)
	jsr deferred.create
	adda.w #DEFER_BLOCK_BYTES+deferred.CREATE_FRAME_BYTES, sp
	bra.w done
eager
	moveq #DEFER_EAGER, d0
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; deferCapturedPlan

; A0=session,D1=tagged recipe. D0/CCR=status,D1=normal plan handle;
; other registers preserved. The callback resolves a fresh immutable view.
bindDeferred	.block
	movem.l a0-a2, -(sp)
	movea.l a0, a2
	move.l a6, -(sp)
	movea.l Frame.Scratch(a2), a6
	.WORK_TEMPLATE_CONTEXT a6, TEMPLATE_STATE
	movea.l (sp)+, a6
	suba.w #deferred.REALIZE_FRAME_BYTES, sp
	movea.l sp, a0
	movea.l Frame.Scratch(a2), a1
	adda.l #TEMPLATE_STATE+templates.DEFERRED, a1
	move.l a1, deferred.RealizeFrame.Arena(a0)
	movea.l Frame.Scratch(a2), a1
	adda.l #MACRO_PLANS, a1
	move.l a1, deferred.RealizeFrame.Plans(a0)
	move.l d1, deferred.RealizeFrame.Handle(a0)
	move.l a2, deferred.RealizeFrame.Context(a0)
	move.l Frame.CaptureResolve(a2), deferred.RealizeFrame.Resolve(a0)
	.if ACQUIRE_MACRO_CANDIDATES
	move.l #acquireDeferred, deferred.RealizeFrame.Acquire(a0)
.endif
.if DEFER_MACRO_LINES
	move.l #acquireDeferredLine, deferred.RealizeFrame.AcquireLine(a0)
.endif
	jsr deferred.realize
	adda.w #deferred.REALIZE_FRAME_BYTES, sp
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; bindDeferred

	.pub
; A0=session. D1-D4=created/bound/cache hits/recipe bytes,D5=normal plan Used.
; Other registers preserved; CCR unspecified. Only gated reporting calls this.
deferredStats	.block
	move.l a0, -(sp)
	movea.l Frame.Scratch(a0), a0
	move.l MACRO_PLANS+memory.Block.Used(a0), d5
	adda.l #TEMPLATE_STATE+templates.DEFERRED, a0
	jsr deferred.getStats
	movea.l (sp)+, a0
	rts
	.bend  ; deferredStats
	.if ACQUIRE_MACRO_CANDIDATES
; A0=session. D1/D2=created/acquired first-use call recipes; other regs kept.
; CCR unspecified. Reporting is separately gated by the existing memory profile.
acquisitionStats	.block
	move.l a0, -(sp)
	movea.l Frame.Scratch(a0), a0
	adda.l #TEMPLATE_STATE+templates.DEFERRED, a0
	jsr deferred.getAcquisitionStats
	movea.l (sp)+, a0
	rts
	.bend  ; acquisitionStats
.endif
.if DEFER_MACRO_LINES
; A0=session. D1/D2=created/realized whole-line recipes; others preserved.
lineStats	.block
	move.l a0, -(sp)
	movea.l Frame.Scratch(a0), a0
	adda.l #TEMPLATE_STATE+templates.DEFERRED, a0
	jsr deferred.getLineStats
	movea.l (sp)+, a0
	rts
	.bend  ; lineStats
.endif
	.priv
.endif
