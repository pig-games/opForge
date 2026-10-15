; Compact physical fragment proof adapter. Required declaration checks consume
; the shared VM's captured spans; full whole-line execution plans wait for use.
; Reuse two disjoint portions of spelling scratch: published rows + staging.
.if spelling.SCRATCH_BYTES < 2*line_proofs.SCRATCH_BYTES
.error "whole-line proof scratch exceeds reusable spelling scratch"
.endif
LineProof	.struct
View	.res line_proofs.FRAME_BYTES
Block	.res memory.Block.Used+4
Mode	.long ?
Map	.long ?
Count	.long ?
.endstruct
LINE_PROOF_BYTES = LineProof.Count+4

; A6=scratch with current immutable capture. D0/CCR=RECIPE_* status,
; A1=expanded fragment rows,D1=count,D2=0 logical spelling base; others kept.
; Uses the existing fragment-result scratch; no retained copy is added.
proofRecipes	.block
	movem.l d3-d7/a0/a2-a5, -(sp)
	suba.w #LINE_PROOF_BYTES, sp
	movea.l sp, a3
	movea.l CAPTURE_RECORD(a6), a4
	bsr.w viewLineProof
	adda.w #LINE_PROOF_BYTES, sp
	movem.l (sp)+, d3-d7/a0/a2-a5
	tst.l d0
	rts
	.bend  ; proofRecipes

; A3=LineProof frame,A4=capture,A6=scratch. D0/statusCCR,D1=fragment count,
; A1=rows,D2=0,A2=source. Other registers preserved. Offsets remain relative
; to the owned spelling; borrowed pointers expire on capture arena mutation.
viewLineProof	.block
	movem.l d3/a0, -(sp)
	cmpi.l #capture.LINE_PROOF, capture.Record.LineStatus(a4)
	bne.w bad
	movea.l a4, a1
	jsr capture.planView
	move.l a0, LineProof.Block+memory.Block.Pointer(a3)
	move.l d0, LineProof.Block+memory.Block.Capacity(a3)
	move.l d0, LineProof.Block+memory.Block.Used(a3)
	lea LineProof.View(a3), a0
	lea LineProof.Block(a3), a1
	move.l a1, line_proofs.ViewFrame.Arena(a0)
	move.l capture.Record.LineHandle(a4), line_proofs.ViewFrame.Handle(a0)
	movea.l a6, a1
	adda.l #MACRO_SPELL_SCRATCH, a1
	move.l a1, line_proofs.ViewFrame.Rows(a0)
	adda.l #line_proofs.SCRATCH_BYTES, a1
	move.l a1, line_proofs.ViewFrame.Staging(a0)
	move.l #macro_plans.MAX_ROWS, line_proofs.ViewFrame.Capacity(a0)
	jsr line_proofs.view
	bne.w bad
	moveq #0, d3
	move.b capture.Record.Count(a4), d3
	cmp.l line_proofs.ViewFrame.TokenCount(a0), d3
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #macro_plans.RECIPE_INVALID, d0
done
	movem.l (sp)+, d3/a0
	tst.l d0
	rts
	.bend  ; viewLineProof

; A5=session,A6=scratch. Preserve lexical binding and validate the declaration
; map before publishing a numeric first-use recipe. Other template kinds keep
; immediate construction. D0/CCR=status,D1=plan handle; other registers kept.
selectedLineProof	.block
	movem.l d2-d7/a0-a4, -(sp)
	moveq #0, d6
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	jsr templates.openDef
	bne.w immediate
	cmpi.w #templates.KIND_MACRO, templates.Def.Kind(a1)
	bne.w immediate
	tst.l Frame.CaptureResolve(a5)
	beq.w immediate
	moveq #1, d6
immediate
	movea.l CAPTURE_RECORD(a6), a4
	lea PACKED_MAP(a6), a1
	move.l LINE_FRAME+writer.Frame.Count(a6), d2
	move.l d6, d0
	bsr.w constructLineProof
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
	move.l capture.Record.LineHandle(a4), deferred.CreateFrame.Handle(a0)
	lea PACKED_MAP(a6), a1
	move.l a1, deferred.CreateFrame.PackedMap(a0)
	move.l d2, deferred.CreateFrame.TokenCount(a0)
	move.w #1, deferred.CreateFrame.IncludeRecipes(a0)
	clr.w deferred.CreateFrame.Reserved(a0)
	jsr deferred.createLine
	adda.w #deferred.CREATE_FRAME_BYTES, sp
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; selectedLineProof

; Deferred callback. A0=session,A1=owned declaration map,D1=capture offset,
; D2=token count,D3=expected compact proof handle. D0/CCR=status,
; D1=normal handle; other registers preserved.
acquireDeferredLine	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l Frame.Scratch(a5), a6
	movea.l a1, a3
	move.l Frame.CaptureResolve(a5), d0
	beq.w bad
	movea.l d0, a2
	jsr (a2)
	tst.l d0
	bne.w bad
	movea.l a1, a4
	cmp.l capture.Record.LineHandle(a4), d3
	bne.w bad
	movea.l a3, a1
	moveq #0, d0
	bsr.w constructLineProof
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; acquireDeferredLine

; A4=capture,A1=declaration map,D2=count,D0=validate(1)/construct(0),
; A5=session,A6=scratch. D0/CCR=status,D1=normal handle or zero for validation;
; others preserved. Shared container/map checks do not rerun fragment grammar.
; Macro event/frame/result scratch is caller-disposable, never retained here.
constructLineProof	.block
	movem.l d2-d7/a0-a4, -(sp)
	suba.w #LINE_PROOF_BYTES, sp
	movea.l sp, a3
	move.l d0, LineProof.Mode(a3)
	move.l a1, LineProof.Map(a3)
	move.l d2, LineProof.Count(a3)
	bsr.w viewLineProof
	bne.w bad
	move.l LineProof.View+line_proofs.ViewFrame.TokenCount(a3), d0
	cmp.l LineProof.Count(a3), d0
	bne.w bad
	lea MACRO_EVENTS(a6), a1
	movea.l a1, a0
	moveq #macro_plans.ROW_BYTES/4-1, d0
clearRow
	clr.l (a0)+
	dbra d0, clearRow
	move.w #parser_abi.PRVM_RESULT_MACRO_LINE, macro_plans.Row.Kind(a1)
	move.l LineProof.Count(a3), macro_plans.Row.PackedEnd(a1)
	move.l LineProof.View+line_proofs.ViewFrame.SourceBytes(a3), macro_plans.Row.SpellingEnd(a1)
	lea MACRO_FRAME(a6), a0
	lea MACRO_PLANS(a6), a2
	move.l a2, macro_plans.Frame.Arena(a0)
	move.l a1, macro_plans.Frame.Events(a0)
	move.l #1, macro_plans.Frame.Count(a0)
	move.l LineProof.View+line_proofs.ViewFrame.Source(a3), macro_plans.Frame.Source(a0)
	move.l LineProof.View+line_proofs.ViewFrame.SourceBytes(a3), macro_plans.Frame.SourceBytes(a0)
	move.l LineProof.Map(a3), macro_plans.Frame.PackedMap(a0)
	move.l LineProof.Count(a3), macro_plans.Frame.TokenCount(a0)
	move.l LineProof.View+line_proofs.ViewFrame.Rows(a3), macro_plans.Frame.RecipeEvents(a0)
	move.l LineProof.View+line_proofs.ViewFrame.Count(a3), macro_plans.Frame.RecipeCount(a0)
	tst.l LineProof.Mode(a3)
	beq.w construct
	jsr macro_plans.validate
	moveq #0, d1
	bra.w done
construct
	.WORK_COUNT lazy_work.PLAN_BIND, #1
	.WORK_BOUND_BEGIN MACRO_PLANS+memory.Block.Used(a6)
	jsr macro_plans.create
	.WORK_BOUND_END MACRO_PLANS+memory.Block.Used(a6)
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	adda.w #LINE_PROOF_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; constructLineProof
