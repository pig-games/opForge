; Live-owner retained capture aggregation; included in lazy_work code.
; A0=structural State,A1=live capture.Region. Before actual owner release.
; Walk only existing validated numeric records. No borrowed pointer survives.
captureRegion	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	subq.l #8, sp
	clr.l (sp)
	movea.l a0, a5
	movea.l a1, a4
	lea State, a6
	clr.l WorkFields.FilterCount(a6)
	move.l Fields.Count(a5), d3
	cmpi.l #LIMIT, d3
	bhi.w invalidInput
	moveq #0, d2
	lea Fields.Events(a5), a0
	lea WorkFields.Filter(a6), a1
filter
	tst.l d3
	beq.w filtered
	move.l Event.CaptureOpen(a0), d0
	cmp.l capture.Region.Base(a4), d0
	blo.w previousRange
	cmp.l capture.Region.End(a4), d0
	blo.w included
previousRange
	move.l Event.PreviousCapture(a0), d0
	cmp.l capture.Region.Base(a4), d0
	blo.w nextFilter
	cmp.l capture.Region.End(a4), d0
	bhs.w nextFilter
included
	move.l d2, (a1)+
	addq.l #1, WorkFields.FilterCount(a6)
nextFilter
	adda.w #EVENT_BYTES, a0
	addq.l #1, d2
	subq.l #1, d3
	bra.w filter
filtered
	move.l capture.Region.Arena+memory.Block.Capacity(a4), d0
	add.l d0, WorkFields.Capacity(a6)
	bcc.w nextRecord
	ori.l #OVERFLOW, WorkFields.Errors(a6)
nextRecord
	move.l (sp), d1
	cmp.l capture.Region.Arena+memory.Block.Used(a4), d1
	beq.w done
	bhi.w invalidInput
	addq.l #1, d1
	lea capture.Region.Arena(a4), a0
	jsr capture.resolve
	bne.w invalidInput
	movea.l a1, a3
	addq.l #1, WorkFields.CaptureRecords(a6)
	bcs.w invalidInput
	move.l capture.Record.Bytes(a3), 4(sp)
	move.l (sp), d7
	add.l capture.Region.Base(a4), d7
	bcs.w invalidInput
	lea WorkFields.Record(a6), a0
	moveq #METRICS-1, d0
clearRecord
	clr.l (a0)+
	dbra d0, clearRecord
	lea WorkFields.Record(a6), a0
	move.l #capture.HEADER_BYTES, CAPTURE_HEADER(a0)
	moveq #0, d0
	move.b capture.Record.Count(a3), d0
	mulu.w #capture.TOKEN_BYTES, d0
	move.l d0, WorkFields.Record+CAPTURE_TOKENS(a6)
	move.l capture.Record.LexemeBytes(a3), WorkFields.Record+CAPTURE_LEXEMES(a6)
	move.l capture.Record.PlanBytes(a3), WorkFields.Record+CAPTURE_PLANS(a6)
	move.l capture.Record.Bytes(a3), WorkFields.Record+CAPTURE_BYTES(a6)
	move.l capture.Record.Bytes(a3), d1
	subi.l #capture.HEADER_BYTES, d1
	bcs.w invalidInput
	sub.l d0, d1
	bcs.w invalidInput
	sub.l capture.Record.LexemeBytes(a3), d1
	bcs.w invalidInput
	sub.l capture.Record.PlanBytes(a3), d1
	bcs.w invalidInput
	move.l d1, WorkFields.Record+CAPTURE_PADDING(a6)
	lea capture.Record.WorkCounts(a3), a0
	lea WorkFields.Record+CANDIDATE(a6), a2
	moveq #3, d0
packed
	moveq #0, d1
	move.b (a0)+, d1
	move.l d1, (a2)+
	dbra d0, packed
	; Capture bytes are counted once. Acquisition totals were counted at calls.
	moveq #CAPTURE_HEADER, d0
captureTotals
	lea WorkFields.Record(a6), a0
	move.l 0(a0, d0.l), d1
	lea WorkFields.Totals(a6), a0
	movea.l a6, a1
	bsr.w addScalar
	addq.l #4, d0
	cmpi.l #CANDIDATE, d0
	blo.w captureTotals
	moveq #0, d4 ; deepest physical owner, ordinal+1
	moveq #0, d5 ; greatest original prepared opening
	moveq #0, d2
	move.l WorkFields.FilterCount(a6), d3
unit
	tst.l d3
	beq.w owner
	move.l d2, d0
	lsl.l #2, d0
	lea WorkFields.Filter(a6), a0
	move.l 0(a0, d0.l), d6
	move.l d6, d0
	bsr.w rowAddress
	move.l d6, d0
	mulu.w #EVENT_BYTES, d0
	lea Fields.Events(a5), a0
	adda.l d0, a0
	; A preceding label is transferred only by authoritative reconciliation.
	cmp.l Event.PreviousCapture(a0), d7
	bne.w range
	move.l capture.Record.Origin(a3), d0
	cmp.l Event.PreviousOrigin(a0), d0
	bne.w range
	moveq #0, d0
	move.w capture.Record.Line(a3), d0
	cmp.l Event.PreviousLine(a0), d0
	bne.w range
	movem.l a0/a2, -(sp)
	lea WorkFields.Record(a6), a0
	adda.w #Row.Previous, a2
	bsr.w addVector
	movem.l (sp)+, a0/a2
range
	move.l Event.CaptureOpen(a0), d0
	cmp.l capture.Region.Base(a4), d0
	blo.w advance
	cmp.l capture.Region.End(a4), d0
	bhs.w advance
	move.l Event.CaptureClose(a0), d1
	cmp.l d0, d1
	blo.w badRange
	cmp.l capture.Region.End(a4), d1
	bhs.w badRange
	tst.l Event.OriginalEnd(a0)
	beq.w badRange
	cmp.l d0, d7
	blo.w advance
	cmp.l d1, d7
	bhi.w advance
	move.l capture.Record.Origin(a3), d0
	cmp.l Event.Origin(a0), d0
	bne.w badRange
	move.l Event.OriginalOpen(a0), d0
	cmp.l d5, d0
	blo.w advance
	move.l d0, d5
	move.l d6, d4
	addq.l #1, d4
	bra.w advance
badRange
	ori.l #BAD_RANGE, Row.Reasons(a2)
advance
	addq.l #1, d2
	subq.l #1, d3
	bra.w unit
owner
	tst.l d4
	beq.w advanceRecord
	move.l d4, d0
	subq.l #1, d0
	bsr.w rowAddress
	adda.w #Row.Values, a2
	lea WorkFields.Record(a6), a0
	bsr.w addVector
advanceRecord
	move.l 4(sp), d0
	add.l d0, (sp)
	bcs.w invalidInput
	bra.w nextRecord
invalidInput
	ori.l #INVALID, WorkFields.Errors(a6)
done
	addq.l #8, sp
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; captureRegion
