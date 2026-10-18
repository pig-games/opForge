; Template exposure request bounds and atomic publication helpers.
; Included by binary_template_exposure.i in the import owner.
.priv
exposureRequest	.block
	move.l a6, d0
	beq.w invalid
	move.l TemplateExposureFrame.DefinitionId(a5), d0
	cmpi.l #$ffff, d0
	bhi.w invalid
	sub.w layout.State.Base(a6), d0
	bcs.w invalid
	cmp.w layout.State.Count(a6), d0
	bhs.w invalid
	lea layout.LEXICAL_STATE(a6), a0
	lea ExposureLocal.Entry(a4), a1
	jsr lexical.readEntry
	bne.w invalid
	movea.l a1, a0
	btst #records.STATEMENT_DECLARED_BIT, records.Entry.TemplateFlags+1(a0)
	beq.w invalid
	move.l a0, ExposureLocal.Definition(a4)
	cmpi.l #1, TemplateExposureFrame.Visibility(a5)
	bhi.w invalid
	move.l TemplateExposureFrame.OriginalKeyBytes(a5), d0
	beq.w invalid
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w invalid
	move.l TemplateExposureFrame.OriginalKey(a5), d1
	beq.w invalid
	add.l d0, d1
	bcs.w invalid
	move.l TemplateExposureFrame.Cursor(a5), d0
	beq.w invalid
	addi.l #EXPOSURE_CURSOR_BYTES, d0
	bcs.w invalid
	move.l TemplateExposureFrame.RowOutput(a5), d0
	beq.w invalid
	addi.l #EXPOSURE_ROW_BYTES, d0
	bcs.w invalid
	cmpi.l #EXPOSURE_ROW_BYTES, TemplateExposureFrame.RowCapacity(a5)
	blo.w capacity
	move.l TemplateExposureFrame.KeyUsed(a5), d0
	cmp.l TemplateExposureFrame.KeyCapacity(a5), d0
	bhi.w invalid
	move.l TemplateExposureFrame.KeyArena(a5), d0
	beq.w invalid
	add.l TemplateExposureFrame.KeyCapacity(a5), d0
	bcs.w invalid
	bra.w exposureSpans
capacity
	moveq #EXPOSURE_CAPACITY, d0
	rts
invalid
	moveq #EXPOSURE_INVALID, d0
	rts
	.bend  ; exposureRequest

exposureCursorCheck	.block
	move.l ExposureLocal.Cursor+ExposureCursor.Phase(a4), d0
	cmpi.l #EXPOSURE_DONE, d0
	bhi.w invalid
	lea layout.IMPORT_STATE(a6), a0
	moveq #0, d1
	move.w COUNT(a0), d1
	cmpi.l #RECORD_LIMIT, d1
	bhi.w invalid
	cmp.l ExposureLocal.Cursor+ExposureCursor.Item(a4), d1
	blo.w invalid
	cmp.l ExposureLocal.Cursor+ExposureCursor.Visited(a4), d1
	blo.w invalid
	moveq #0, d1
	move.w SELECTED_COUNT(a0), d1
	cmpi.l #RECORD_LIMIT, d1
	bhi.w invalid
	cmp.l ExposureLocal.Cursor+ExposureCursor.Selection(a4), d1
	blo.w invalid
	cmp.l ExposureLocal.Cursor+ExposureCursor.SelectedVisited(a4), d1
	blo.w invalid
	cmpi.l #1, ExposureLocal.Cursor+ExposureCursor.Variant(a4)
	bhi.w invalid
	tst.l d0
	bne.w valid
	move.l ExposureLocal.Cursor+ExposureCursor.Item(a4), d0
	or.l ExposureLocal.Cursor+ExposureCursor.Selection(a4), d0
	or.l ExposureLocal.Cursor+ExposureCursor.Variant(a4), d0
	or.l ExposureLocal.Cursor+ExposureCursor.Visited(a4), d0
	or.l ExposureLocal.Cursor+ExposureCursor.SelectedVisited(a4), d0
	bne.w invalid
valid
	moveq #0, d0
	rts
invalid
	moveq #EXPOSURE_INVALID, d0
	rts
	.bend  ; exposureCursorCheck

exposureSpans	.block
	lea ExposureLocal.Bounds(a4), a1
	movea.l a5, a0
	moveq #TEMPLATE_EXPOSURE_FRAME_BYTES, d0
	moveq #1, d1
	bsr.w exposureSpanBound
	bne.w done
	movea.l TemplateExposureFrame.Cursor(a5), a0
	move.l a0, d0
	andi.l #3, d0
	bne.w invalid
	moveq #EXPOSURE_CURSOR_BYTES, d0
	bsr.w exposureSpanBound
	bne.w done
	movea.l TemplateExposureFrame.RowOutput(a5), a0
	move.l a0, d0
	andi.l #3, d0
	bne.w invalid
	move.l TemplateExposureFrame.RowCapacity(a5), d0
	bsr.w exposureSpanBound
	bne.w done
	movea.l TemplateExposureFrame.KeyArena(a5), a0
	move.l TemplateExposureFrame.KeyCapacity(a5), d0
	bsr.w exposureSpanBound
	bne.w done
	movea.l TemplateExposureFrame.OriginalKey(a5), a0
	move.l TemplateExposureFrame.OriginalKeyBytes(a5), d0
	moveq #0, d1
	bsr.w exposureSpanBound
	bne.w done
	lea ExposureLocal.Bounds(a4), a2
	moveq #EXPOSURE_BOUND_COUNT-1, d4
outer
	move.l ExposureBound.Start(a2), d0
	cmp.l ExposureBound.End(a2), d0
	beq.w nextOuter
	lea EXPOSURE_BOUND_BYTES(a2), a3
	move.l d4, d5
	subq.l #1, d5
	bmi.w valid
inner
	move.l ExposureBound.Start(a3), d0
	cmp.l ExposureBound.End(a3), d0
	beq.w nextInner
	move.l ExposureBound.Mutable(a2), d0
	or.l ExposureBound.Mutable(a3), d0
	beq.w nextInner
	move.l ExposureBound.End(a2), d0
	cmp.l ExposureBound.Start(a3), d0
	bls.w nextInner
	move.l ExposureBound.End(a3), d0
	cmp.l ExposureBound.Start(a2), d0
	bhi.w invalid
nextInner
	adda.w #EXPOSURE_BOUND_BYTES, a3
	dbra d5, inner
nextOuter
	adda.w #EXPOSURE_BOUND_BYTES, a2
	dbra d4, outer
valid
	moveq #0, d0
	bra.w done
invalid
	moveq #EXPOSURE_INVALID, d0
done
	rts
	.bend  ; exposureSpans

exposureSpanBound	.block
	move.l a0, d2
	move.l d2, ExposureBound.Start(a1)
	add.l d0, d2
	bcs.w invalid
	move.l d2, ExposureBound.End(a1)
	move.l d1, ExposureBound.Mutable(a1)
	adda.w #EXPOSURE_BOUND_BYTES, a1
	moveq #0, d0
	rts
invalid
	moveq #EXPOSURE_INVALID, d0
	rts
	.bend  ; exposureSpanBound

exposureCommitCursor	.block
	lea ExposureLocal.Cursor(a4), a0
	movea.l TemplateExposureFrame.Cursor(a5), a1
	moveq #EXPOSURE_CURSOR_BYTES/4-1, d0
next
	move.l (a0)+, (a1)+
	dbra d0, next
	rts
	.bend  ; exposureCommitCursor

exposurePublish	.block
	move.l ExposureLocal.KeyBytes(a4), d0
	beq.w invalid
	move.l ExposureLocal.PrefixBytes(a4), d1
	beq.w bytes
	addq.l #1, d1
	add.l d1, d0
	bcs.w invalid
bytes
	cmpi.l #layout.NAME_BYTES-1, d0
	bhi.w invalid
	move.l d0, ExposureLocal.Bytes(a4)
	add.l TemplateExposureFrame.KeyUsed(a5), d0
	bcs.w invalid
	cmp.l TemplateExposureFrame.KeyCapacity(a5), d0
	bhi.w capacity
	movea.l TemplateExposureFrame.KeyArena(a5), a1
	adda.l TemplateExposureFrame.KeyUsed(a5), a1
	move.l ExposureLocal.PrefixBytes(a4), d0
	beq.w key
	movea.l ExposureLocal.Prefix(a4), a0
	bsr.w exposureCopy
	move.b #'.', (a1)+
key
	movea.l ExposureLocal.Key(a4), a0
	move.l ExposureLocal.KeyBytes(a4), d0
	bsr.w exposureCopy
	movea.l TemplateExposureFrame.RowOutput(a5), a0
	move.l TemplateExposureFrame.KeyUsed(a5), ExposureRow.KeyOffset(a0)
	move.l ExposureLocal.Bytes(a4), ExposureRow.KeyBytes(a0)
	move.l TemplateExposureFrame.DefinitionId(a5), ExposureRow.DefinitionId(a0)
	move.l ExposureLocal.ImportOrder(a4), ExposureRow.ImportOrder(a0)
	move.l ExposureLocal.Bytes(a4), d0
	add.l d0, TemplateExposureFrame.KeyUsed(a5)
	bsr.w exposureCommitCursor
	moveq #0, d0
	rts
capacity
	moveq #EXPOSURE_CAPACITY, d0
	rts
invalid
	moveq #EXPOSURE_INVALID, d0
	rts
	.bend  ; exposurePublish

exposureCopy	.block
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; exposureCopy
.pub
; A0/D0=opaque keyword bytes,A1=scope. D0/status,A0=owned scope scratch,
; D1=complete canonical bytes; other registers preserved. No binding/allocation.
; Module and retained keyword facets are copied before a later binder can move
; its arena. Dots within the keyword remain literal; no source grammar is read.
statementKeywordBytes	.block
	suba.w #layout.NAME_BYTES, sp
	movem.l d2-d7/a1-a6, -(sp)
	movea.l a1, a6
	movea.l a0, a3
	move.l d0, d3
	beq.w invalid
	cmpi.l #layout.NAME_BYTES-1, d3
	bhi.w invalid
	move.l a3, d0
	beq.w invalid
	add.l d3, d0
	bcs.w invalid
	lea layout.BUFFER(a6), a2
	movea.l a2, a4
	moveq #0, d5
	moveq #0, d0
	move.w layout.MODULE_STATE+modules.State.Active(a6), d0
	beq.w keyword
	subq.l #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w invalid
	lea 48(sp), a1
	bsr.w entryName
	bne.w invalid
	move.l d0, d5
	beq.w invalid
	addq.l #1, d5
	move.l d5, d1
	add.l d3, d1
	cmpi.l #layout.NAME_BYTES-1, d1
	bhi.w invalid
moduleCopy
	move.b (a0)+, (a2)+
	subq.l #1, d0
	bne.w moduleCopy
	move.b #'.', (a2)+
keyword
	move.l d3, d0
keywordCopy
	move.b (a3)+, (a2)+
	subq.l #1, d0
	bne.w keywordCopy
	move.b #0, (a2)
	move.l d5, d1
	add.l d3, d1
	movea.l a4, a0
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a1-a6
	adda.w #layout.NAME_BYTES, sp
	tst.l d0
	rts
	.bend  ; statementKeywordBytes
.priv
