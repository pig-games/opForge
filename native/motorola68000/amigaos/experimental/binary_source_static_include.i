; Static source acquisition during owned capture, before graph ordering.
; The shared conditional owner executes preprocessor guards and retains scalar
; guard envelopes. FilePlan still parses every selected complete operand.

staticCaptureBegin	.block
	clr.w StaticIncludeSeen
	clr.w StaticTemplateDepth
	clr.w StaticLoopDepth
	clr.w StaticPending
	lea StaticConditions, a0
	jsr conditionals.beginAcquisition
	rts
	.bend  ; staticCaptureBegin

; CaptureLine owns one validated unbound record. D0/CCR=status,D1=one when
; this include is acquired and omitted before capture publication. Other regs
; preserved. No body directive semantics are executed here.
staticCaptureLine	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea CaptureLine, a0
	movea.l memory.Block.Pointer(a0), a1
	lea ConfigScan, a0
	jsr configuration.captureRole
	bne.w bad
	move.l d1, d7
	cmpi.l #configuration.TEMPLATE_CLOSE, d7
	beq.w closeTemplate
	cmpi.l #scopes.KEY_ENDMACRO, d7
	beq.w closeTemplate
	cmpi.l #scopes.KEY_ENDSEGMENT, d7
	beq.w closeTemplate
	tst.w StaticTemplateDepth
	bne.w templateBody
	cmpi.l #configuration.TEMPLATE_OPEN, d7
	beq.w openTemplate
	cmpi.l #scopes.KEY_MACRO, d7
	beq.w openTemplate
	cmpi.l #scopes.KEY_SEGMENT, d7
	beq.w openTemplate
	cmpi.l #configuration.LOOP_OPEN, d7
	beq.w openLoop
	cmpi.l #configuration.LOOP_CLOSE, d7
	beq.w closeLoop
	cmpi.l #scopes.KEY_IF, d7
	blo.w fileRole
	cmpi.l #scopes.KEY_ENDMATCH, d7
	bhi.w fileRole
	bsr.w bindStaticRecord
	bne.w bad
	lea Front, a0
	movea.l frontend.Frame.Output(a0), a0
	lea ConfigScan, a1
	movea.l configuration.Frame.Scope(a1), a1
	lea StaticConditions, a2
	jsr conditionals.line
	bne.w bad
	bra.w ordinary
closeTemplate
	tst.w StaticTemplateDepth
	beq.w ordinary
	subq.w #1, StaticTemplateDepth
	bra.w ordinary
openTemplate
	addq.w #1, StaticTemplateDepth
	beq.w bad
	bra.w ordinary
templateBody
	cmpi.l #configuration.TEMPLATE_OPEN, d7
	beq.w openTemplate
	cmpi.l #scopes.KEY_MACRO, d7
	beq.w openTemplate
	cmpi.l #scopes.KEY_SEGMENT, d7
	beq.w openTemplate
	bra.w fileRole
openLoop
	addq.w #1, StaticLoopDepth
	beq.w bad
	bra.w ordinary
closeLoop
	tst.w StaticLoopDepth
	beq.w ordinary
	subq.w #1, StaticLoopDepth
	bra.w ordinary
fileRole
	cmpi.l #configuration.SOURCE_INCLUDE, d7
	bne.w ordinary
	tst.w StaticConditions+conditionals.State.Active
	beq.w ordinary
	tst.w StaticTemplateDepth
	bne.w retain
	tst.w StaticLoopDepth
	bne.w retain
	tst.w StaticConditions+conditionals.State.Deferred
	bne.w retain
	tst.w StaticIncludeSeen
	bne.w ordinary
	bsr.w bindStaticRecord
	bne.w bad
	lea Front, a0
	jsr frontend.captureFile
	bne.w bad
	move.w #1, StaticPending
	moveq #1, d1
	moveq #0, d0
	bra.w done
retain
	move.w #1, StaticIncludeSeen
ordinary
	moveq #0, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; staticCaptureLine

; Bind the selected token copy under its indexed module. Binding may intern
; names but never executes declarations, instructions or include body records.
bindStaticRecord	.block
	movem.l d1/a0-a2, -(sp)
	lea ConfigScan, a0
	movea.l configuration.Frame.Scope(a0), a2
	move.w layout.State.Current(a2), -(sp)
	move.w configuration.Frame.IndexModule+2(a0), layout.State.Current(a2)
	lea Front, a0
	lea CaptureLine, a1
	moveq #1, d1
	jsr frontend.bindCapture
	move.w (sp)+, layout.State.Current(a2)
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; bindStaticRecord
