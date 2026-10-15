; Call candidates are validated during capture and declaration, then constructed
; at the first accepted macro invocation. Whole-line plans already own spelling;
; this adapter retains no second text copy and never tokenizes/reparses source.
; Included only by the candidate acquisition experiment.
Acquisition	.struct
Mode	.long ?
Map	.long ?
Count	.long ?
Recipes	.long ?
Source	.long ?
SourceBytes	.long ?
LineCount	.long ?
LineNumber	.long ?
View	.res memory.Block.Used+4
Tokens	.res TOKEN_CAPACITY*writer.TOKEN_BYTES
.endstruct
ACQUISITION_BYTES = Acquisition.Tokens+TOKEN_CAPACITY*writer.TOKEN_BYTES

; A5=session,A6=scratch. Selected pending call; numeric declaration map is live.
; D0/CCR=status,D1=normal or tagged plan handle; all other registers preserved.
; Only a physical macro-body call receives a first-use recipe. Other consumers
; acquire immediately. Invalid unused definitions still fail at declaration.
selectedAcquisition	.block
	movem.l d2-d7/a0-a4, -(sp)
	moveq #0, d6
	moveq #0, d3
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	jsr templates.hasOpen
	beq.w contextReady
	moveq #1, d3
	jsr templates.openDef
	bne.w bad
	cmpi.w #templates.KIND_MACRO, templates.Def.Kind(a1)
	bne.w contextReady
	tst.l Frame.CaptureResolve(a5)
	beq.w contextReady
	moveq #1, d6
contextReady
	movea.l CAPTURE_RECORD(a6), a4
	lea PACKED_MAP(a6), a1
	move.l LINE_FRAME+writer.Frame.Count(a6), d2
	move.l d6, d0
	bsr.w acquireCall
	bne.w bad
	tst.l d6
	beq.w done
	suba.w #deferred.CREATE_FRAME_BYTES, sp
	movea.l sp, a0
	movea.l a6, a1
	adda.l #TEMPLATE_STATE+templates.DEFERRED, a1
	move.l a1, deferred.CreateFrame.Arena(a0)
	clr.l deferred.CreateFrame.Source(a0)
	move.l Frame.CaptureOffset(a5), deferred.CreateFrame.Capture(a0)
	clr.l deferred.CreateFrame.Handle(a0)
	lea PACKED_MAP(a6), a1
	move.l a1, deferred.CreateFrame.PackedMap(a0)
	move.l d2, deferred.CreateFrame.TokenCount(a0)
	move.w #1, deferred.CreateFrame.IncludeRecipes(a0)
	clr.w deferred.CreateFrame.Reserved(a0)
	jsr deferred.createAcquired
	adda.w #deferred.CREATE_FRAME_BYTES, sp
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; selectedAcquisition

; Deferred owner callback. A0=session,A1=owned declaration map,D1=capture offset,
; D2=token count. D0/CCR=status,D1=normal plan handle; others preserved.
; Capture and deferred owners remain immutable while the normal arena grows.
acquireDeferred	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l Frame.Scratch(a5), a6
	movea.l a1, a3
	move.l Frame.CaptureResolve(a5), d0
	beq.w bad
	movea.l d0, a2
	jsr (a2)
	bne.w bad
	movea.l a1, a4
	movea.l a3, a1
	moveq #1, d3
	moveq #0, d0
	bsr.w acquireCall
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; acquireDeferred

; A4=validated capture,A1=declaration map,D2=count,D3=include recipes(0/1),
; D0=validate only(1) / construct(0),A5=session,A6=scratch.
; D0/CCR=status,D1=normal handle (zero for validation); other registers preserved.
; Shared binary services consume original TKVM rows and owned code spelling.
; Restore caller token rows/count/source; invalidate transient view on return.
; Macro request/events/frame scratch is caller-disposable, never retained.
acquireCall	.block
	movem.l d2-d7/a0-a4, -(sp)
	suba.w #ACQUISITION_BYTES, sp
	movea.l sp, a3
	move.l d0, Acquisition.Mode(a3)
	move.l a1, Acquisition.Map(a3)
	move.l d2, Acquisition.Count(a3)
	move.l d3, Acquisition.Recipes(a3)
	move.l Frame.Source(a5), Acquisition.Source(a3)
	move.l Frame.SourceBytes(a5), Acquisition.SourceBytes(a3)
	move.l LINE_FRAME+writer.Frame.Count(a6), Acquisition.LineCount(a3)
	move.l LINE_NUMBER(a6), Acquisition.LineNumber(a3)
	lea TOKENS(a6), a0
	lea Acquisition.Tokens(a3), a1
	move.l #TOKEN_CAPACITY*writer.TOKEN_BYTES, d0
	bsr.w copyCaptureBytes
	cmpi.l #capture.CALL_DEFERRED, capture.Record.CallStatus(a4)
	bne.w bad
	tst.l capture.Record.CallHandle(a4)
	bne.w bad
	moveq #0, d0
	move.b capture.Record.Count(a4), d0
	cmp.l Acquisition.Count(a3), d0
	bne.w bad
	tst.l Acquisition.Recipes(a3)
	beq.w recipesReady
	tst.l capture.Record.CallRecipeStatus(a4)
	bne.w bad
recipesReady
	tst.l capture.Record.LineStatus(a4)
	bne.w bad
	movea.l a4, a1
	jsr capture.planView
	move.l a0, Acquisition.View+memory.Block.Pointer(a3)
	move.l d0, Acquisition.View+memory.Block.Capacity(a3)
	move.l d0, Acquisition.View+memory.Block.Used(a3)
	lea Acquisition.View(a3), a0
	move.l capture.Record.LineHandle(a4), d1
	jsr macro_plans.resolve
	bne.w bad
	cmpi.l #1, macro_plans.Plan.Count(a1)
	bne.w bad
	lea macro_plans.HEADER_BYTES(a1), a1
	cmpi.w #parser_abi.PRVM_RESULT_MACRO_LINE, macro_plans.Row.Kind(a1)
	bne.w bad
	tst.l macro_plans.Row.PackedStart(a1)
	bne.w bad
	movea.l Acquisition.View+memory.Block.Pointer(a3), a0
	adda.l macro_plans.Row.SpellingStart(a1), a0
	move.l macro_plans.Row.SpellingEnd(a1), d0
	sub.l macro_plans.Row.SpellingStart(a1), d0
	move.l a0, Frame.Source(a5)
	move.l d0, Frame.SourceBytes(a5)
	lea capture.HEADER_BYTES(a4), a0
	lea TOKENS(a6), a1
	move.l Acquisition.Count(a3), d0
	move.l d0, LINE_FRAME+writer.Frame.Count(a6)
	mulu.w #writer.TOKEN_BYTES, d0
	bsr.w copyCaptureBytes
	movea.l a6, a0
	adda.l #BINARY_VIEW_READY, a0
	clr.l (a0)
	moveq #0, d0
	move.w capture.Record.Line(a4), d0
	move.l d0, LINE_NUMBER(a6)
	moveq #1, d4
	bsr.w sourceDescriptors
	bne.w bad
	tst.l d1
	beq.w bad
	lea MACRO_FRAME(a6), a0
	move.l d1, macro_plans.Frame.Count(a0)
	lea MACRO_PLANS(a6), a1
	move.l a1, macro_plans.Frame.Arena(a0)
	lea MACRO_EVENTS(a6), a1
	move.l a1, macro_plans.Frame.Events(a0)
	move.l Frame.Source(a5), macro_plans.Frame.Source(a0)
	move.l Frame.SourceBytes(a5), macro_plans.Frame.SourceBytes(a0)
	move.l Acquisition.Map(a3), macro_plans.Frame.PackedMap(a0)
	move.l Acquisition.Count(a3), macro_plans.Frame.TokenCount(a0)
	clr.l macro_plans.Frame.RecipeEvents(a0)
	clr.l macro_plans.Frame.RecipeCount(a0)
	tst.l Acquisition.Recipes(a3)
	beq.w selected
	bsr.w captureFragments
	bne.w bad
selected
	tst.l Acquisition.Mode(a3)
	beq.w construct
	jsr macro_plans.validate
	moveq #0, d1
	bra.w restore
construct
	.WORK_COUNT lazy_work.PLAN_BIND, #1
	.WORK_BOUND_BEGIN MACRO_PLANS+memory.Block.Used(a6)
	jsr macro_plans.create
	.WORK_BOUND_END MACRO_PLANS+memory.Block.Used(a6)
	bra.w restore
bad
	moveq #0, d1
	moveq #1, d0
restore
	move.l d0, d6
	move.l d1, d7
	lea Acquisition.Tokens(a3), a0
	lea TOKENS(a6), a1
	move.l #TOKEN_CAPACITY*writer.TOKEN_BYTES, d0
	bsr.w copyCaptureBytes
	move.l Acquisition.LineCount(a3), LINE_FRAME+writer.Frame.Count(a6)
	move.l Acquisition.LineNumber(a3), LINE_NUMBER(a6)
	move.l Acquisition.Source(a3), Frame.Source(a5)
	move.l Acquisition.SourceBytes(a3), Frame.SourceBytes(a5)
	movea.l a6, a0
	adda.l #BINARY_VIEW_READY, a0
	clr.l (a0)
	move.l d7, d1
	move.l d6, d0
	adda.w #ACQUISITION_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; acquireCall
