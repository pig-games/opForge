; Authoritative whole-unit selection and conservative work partition.
; A0=structural State,A1=authoritative selected rows,D0=count.
; Final ranges alone authorize preceding-entry transfer. Root Live governs
; actual whole-span omission; bounded Parent chains retain nested children.
selected	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	move.l d0, d7
	lea State, a6
	tst.l WorkFields.SelectionDone(a6)
	bne.w done
	move.l #1, WorkFields.SelectionDone(a6)
	move.l Fields.Errors(a5), d0
	or.l d0, WorkFields.Errors(a6)
	move.l Fields.Count(a5), d6
	cmpi.l #LIMIT, d6
	bhi.w invalidInput
	move.l d6, WorkFields.Count(a6)
	moveq #0, d5
	lea Fields.Events(a5), a3
next
	tst.l d6
	beq.w partition
	move.l d5, d0
	bsr.w rowAddress
	movea.l a2, a1
	move.l Event.Unit(a3), d0
	move.l d0, Row.Unit(a1)
	beq.w unmatchedUnit
	cmp.l d7, d0
	bhi.w unmatchedUnit
	subq.l #1, d0
	mulu.w #SPAN_BYTES, d0
	movea.l a4, a0
	adda.l d0, a0
	tst.l Event.Generated(a3)
	beq.w entry
	ori.l #GENERATED, Row.Reasons(a1)
entry
	move.l Span.Start(a0), d0
	cmp.l Event.FinalOpen(a3), d0
	beq.w classify
	cmp.l Event.FinalPrevious(a3), d0
	bne.w unknownPrevious
	; Verify source contains the entire previous vector before subtracting.
	move.l Row.PreviousOwner(a1), d0
	beq.w addPrevious
	subq.l #1, d0
	cmp.l d5, d0
	beq.w unknownPrevious
	cmpi.l #LIMIT, d0
	bhs.w unknownPrevious
	move.l a0, -(sp)
	bsr.w rowAddress
	movea.l (sp)+, a0
	lea Row.Previous(a1), a2
	move.l Row.PreviousOwner(a1), d0
	subq.l #1, d0
	mulu.l #ROW_BYTES, d0
	lea WorkFields.Rows+Row.Values(a6), a2
	adda.l d0, a2
	lea Row.Previous(a1), a0
	moveq #METRICS-1, d0
verifyPrevious
	move.l (a0)+, d1
	cmp.l (a2)+, d1
	bhi.w unknownPrevious
	dbra d0, verifyPrevious
	lea Row.Previous(a1), a0
	move.l Row.PreviousOwner(a1), d0
	subq.l #1, d0
	mulu.l #ROW_BYTES, d0
	lea WorkFields.Rows+Row.Values(a6), a2
	adda.l d0, a2
	moveq #METRICS-1, d0
subtractPrevious
	move.l (a0)+, d1
	sub.l d1, (a2)+
	dbra d0, subtractPrevious
addPrevious
	lea Row.Previous(a1), a0
	lea Row.Values(a1), a2
	bsr.w addVector
	bra.w classifyReload
unknownPrevious
	ori.l #PREVIOUS_UNKNOWN, Row.Reasons(a1)
	bra.w classifyReload
unmatchedUnit
	ori.l #UNMATCHED, Row.Reasons(a1)
	bra.w advance
classifyReload
	move.l Row.Unit(a1), d0
	subq.l #1, d0
	mulu.w #SPAN_BYTES, d0
	movea.l a4, a0
	adda.l d0, a0
classify
	move.l Row.Unit(a1), d2
ancestor
	moveq #0, d3
	move.w Span.Parent(a0), d3
	beq.w root
	cmp.l d7, d3
	bhi.w badParent
	cmp.l d2, d3
	bhs.w badParent ; canonical parents strictly precede children
	move.l d3, d2
	subq.l #1, d3
	mulu.w #SPAN_BYTES, d3
	movea.l a4, a0
	adda.l d3, a0
	bra.w ancestor
root
	move.l #2, Row.Class(a1)
	tst.w Span.Live(a0)
	beq.w advance
	move.l #1, Row.Class(a1)
	bra.w advance
badParent
	addq.l #1, WorkFields.RangeCounts+16(a6)
	ori.l #BAD_RANGE, Row.Reasons(a1)
advance
	adda.w #EVENT_BYTES, a3
	addq.l #1, d5
	subq.l #1, d6
	bra.w next
invalidInput
	ori.l #INVALID, WorkFields.Errors(a6)
partition
	bsr.w makePartition
	bsr.w unknownTop
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; selected
	.priv
; Selected/unreferenced are exclusive payloads; all remaining work is unknown.
; Retained template publication is always shared, regardless of containing unit.
makePartition	.block
	movem.l d0-d7/a0-a3, -(sp)
	lea State, a3
	lea WorkFields.Selected(a3), a0
	move.l #VECTOR_BYTES*4/4, d0
clear
	clr.l (a0)+
	subq.l #1, d0
	bne.w clear
	move.l WorkFields.Totals+TEMPLATE_BYTES(a3), WorkFields.Shared+TEMPLATE_BYTES(a3)
	tst.l WorkFields.Errors(a3)
	bne.w remainder
	move.l WorkFields.Count(a3), d7
	lea WorkFields.Rows(a3), a0
nextRow
	tst.l d7
	beq.w remainder
	move.l Row.Reasons(a0), d0
	or.l d0, WorkFields.UnknownReasons(a3)
	tst.l d0
	bne.w advance
	move.l Row.Class(a0), d0
	beq.w advance
	lea WorkFields.Selected(a3), a2
	cmpi.l #1, d0
	beq.w target
	lea WorkFields.Unreferenced(a3), a2
target
	movem.l a0/a2, -(sp)
	lea Row.Values(a0), a0
	moveq #0, d0
values
	cmpi.l #TEMPLATE_BYTES, d0
	beq.w nextValue
	move.l 0(a0, d0.l), d1
	movea.l a2, a1
	adda.l d0, a1
	add.l d1, (a1)
	bcc.w nextValue
	ori.l #OVERFLOW, WorkFields.Errors(a3)
nextValue
	addq.l #4, d0
	cmpi.l #VECTOR_BYTES, d0
	blo.w values
	movem.l (sp)+, a0/a2
advance
	adda.w #ROW_BYTES, a0
	subq.l #1, d7
	bra.w nextRow
remainder
	moveq #0, d0
nextMetric
	; Compact indexed EA has a signed byte displacement; first address each vector.
	lea WorkFields.Totals(a3), a0
	move.l 0(a0, d0.l), d1
	lea WorkFields.Selected(a3), a0
	sub.l 0(a0, d0.l), d1
	bcs.w invalidInput
	lea WorkFields.Unreferenced(a3), a0
	sub.l 0(a0, d0.l), d1
	bcs.w invalidInput
	lea WorkFields.Shared(a3), a0
	sub.l 0(a0, d0.l), d1
	bcs.w invalidInput
	lea WorkFields.Unknown(a3), a0
	move.l d1, 0(a0, d0.l)
	addq.l #4, d0
	cmpi.l #VECTOR_BYTES, d0
	blo.w nextMetric
	bra.w done
invalidInput
	ori.l #INVALID, WorkFields.Errors(a3)
	; Invalid partitions never claim opportunity: restore all totals as unknown.
	lea WorkFields.Selected(a3), a0
	move.l #VECTOR_BYTES*3/4, d0
reset
	clr.l (a0)+
	subq.l #1, d0
	bne.w reset
	lea WorkFields.Totals(a3), a0
	lea WorkFields.Unknown(a3), a1
	moveq #METRICS-1, d0
copy
	move.l (a0)+, (a1)+
	dbra d0, copy
done
	bsr.w unknownSplit
	movem.l (sp)+, d0-d7/a0-a3
	rts
	.bend  ; makePartition

; Unknown row-owned values exclude shared templates. Residual means only the
; arithmetic remainder: it is not evidence that work happened outside units.
unknownSplit	.block
	movem.l d0-d3/a0-a2/a4, -(sp)
	lea WorkFields.UnknownRows(a3), a0
	moveq #METRICS-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	tst.l WorkFields.Errors(a3)
	bne.w remainder
	move.l WorkFields.Count(a3), d3
	lea WorkFields.Rows(a3), a0
next
	tst.l d3
	beq.w remainder
	tst.l Row.Reasons(a0)
	bne.w included
	tst.l Row.Class(a0)
	bne.w advance
included
	moveq #0, d0
	lea Row.Values(a0), a1
	lea WorkFields.UnknownRows(a3), a2
values
	cmpi.l #TEMPLATE_BYTES, d0
	beq.w nextValue
	move.l 0(a1, d0.l), d1
	movea.l a2, a4
	adda.l d0, a4
	add.l d1, (a4)
	bcs.w invalidInput
nextValue
	addq.l #4, d0
	cmpi.l #VECTOR_BYTES, d0
	blo.w values
advance
	adda.w #ROW_BYTES, a0
	subq.l #1, d3
	bra.w next
remainder
	lea WorkFields.Unknown(a3), a0
	lea WorkFields.UnknownRows(a3), a1
	lea WorkFields.Residual(a3), a2
	moveq #METRICS-1, d0
subtract
	move.l (a0)+, d1
	sub.l (a1)+, d1
	bcs.w invalidInput
	move.l d1, (a2)+
	dbra d0, subtract
	bra.w done
invalidInput
	ori.l #INVALID, WorkFields.Errors(a3)
	lea WorkFields.UnknownRows(a3), a1
	lea WorkFields.Unknown(a3), a0
	lea WorkFields.Residual(a3), a2
	moveq #METRICS-1, d0
reset
	clr.l (a1)+
	move.l (a0)+, (a2)+
	dbra d0, reset
done
	movem.l (sp)+, d0-d3/a0-a2/a4
	rts
	.bend  ; unknownSplit
; Snapshot largest unknown row independently by capture/plan/bound metric.
; Stable ties retain the first event; structural state is borrowed only here.
unknownTop	.block
	movem.l d0-d7/a0-a4, -(sp)
	lea WorkFields.TopRows(a6), a4
	moveq #0, d7
metric
	moveq #CAPTURE_BYTES, d6
	tst.l d7
	beq.w ready
	moveq #PLAN_BIND, d6
	cmpi.l #1, d7
	beq.w ready
	moveq #BOUND_BYTES, d6
ready
	move.l d6, Top.Metric(a4)
	tst.l WorkFields.Errors(a6)
	bne.w nextMetric
	moveq #0, d5
	move.l WorkFields.Count(a6), d4
	lea WorkFields.Rows(a6), a0
nextTopRow
	tst.l d4
	beq.w nextMetric
	tst.l Row.Reasons(a0)
	bne.w included
	tst.l Row.Class(a0)
	bne.w advance
included
	lea Row.Values(a0), a1
	lea Top.Values(a4), a2
	move.l 0(a1, d6.l), d0
	cmp.l 0(a2, d6.l), d0
	bls.w advance
	move.l d5, d0
	addq.l #1, d0
	move.l d0, Top.Event(a4)
	move.l Row.Unit(a0), Top.Unit(a4)
	move.l Row.Reasons(a0), Top.Reasons(a4)
	move.l d5, d0
	mulu.w #EVENT_BYTES, d0
	lea Fields.Events(a5), a3
	adda.l d0, a3
	move.l Event.CaptureOpen(a3), Top.CaptureOpen(a4)
	move.l Event.CaptureClose(a3), Top.CaptureClose(a4)
	move.l Event.Origin(a3), Top.Origin(a4)
	move.l Event.Line(a3), Top.Line(a4)
	move.l Event.OriginalOpen(a3), Top.OriginalOpen(a4)
	moveq #METRICS-1, d0
copy
	move.l (a1)+, (a2)+
	dbra d0, copy
	clr.l Top.Values+TEMPLATE_BYTES(a4)
advance
	adda.w #ROW_BYTES, a0
	addq.l #1, d5
	subq.l #1, d4
	bra.w nextTopRow
nextMetric
	adda.w #TOP_BYTES, a4
	addq.l #1, d7
	cmpi.l #TOP_COUNT, d7
	blo.w metric
	movem.l (sp)+, d0-d7/a0-a4
	rts
	.bend  ; unknownTop
