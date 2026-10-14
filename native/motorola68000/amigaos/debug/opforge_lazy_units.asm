; Structural publication relationships from shared prepared block flags.
; No lexical parser, evaluator, activity decision or published symbol table.
; @opforge-owner: debug.amigaos.lazy_units
	.module debug.amigaos.lazy_units
	.cpu 68020
	.use experimental.amigaos.binary_source as source
	.include "binary_block_span.i"
	.include "lazy_work.i"
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.use debug.amigaos.memory_profile as memory_profile
.endif
.endif
.endif
	.pub
	.include "lazy_units_layout.i"
	.section code, kind=code
	.pub
; Clear the bounded unit observer. All passive routines preserve registers,
; CCR and stack depth; getState explicitly returns A0 only.
clear	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea State, a0
	move.l #STATE_BYTES/4, d0
next
	clr.l (a0)+
	subq.l #1, d0
	bne.w next
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; clear
getState	.block
	lea State, a0
	rts
	.bend  ; getState

; A0=prepared numeric records,D0=bounded bytes,D1=original prepared start,
; D2=active physical capture offset,D3=origin,D4=line,D5=template depth.
; Observe only shared flags. Retain one row per whole unit and a nesting stack.
; Preceding record provenance is a scalar; authoritative indexing decides whether
; it belongs to this unit. Template events remain deferred, never guessed names.
prepared	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a3, -(sp)
	lea State, a3
	movea.l a0, a2
	adda.l d0, a2
	cmpa.l a0, a2
	blo.w badExtent
line
	cmpa.l a2, a0
	beq.w done
	bhi.w badExtent
	moveq #0, d6
	move.b source.RecordHeader.Length(a0), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w badExtent
	move.l a2, d7
	sub.l a0, d7
	cmp.l d7, d6
	bhi.w badExtent
	btst #source.FLAG_BLOCK_OPEN_BIT, source.RecordHeader.Flags(a0)  ; shared BLOCK_OPEN bit
	beq.w close
	move.l Fields.Count(a3), d7
	cmpi.l #LIMIT, d7
	bhs.w tableFull
	move.l d7, d0
	mulu.w #EVENT_BYTES, d0
	movea.l a3, a1
	adda.l #Fields.Events, a1
	adda.l d0, a1
	move.l d1, Event.OriginalOpen(a1)
	move.l d1, Event.FinalOpen(a1)
	move.l Fields.Previous(a3), Event.OriginalPrevious(a1)
	move.l Fields.Previous(a3), Event.FinalPrevious(a1)
	move.l Fields.PreviousCapture(a3), Event.PreviousCapture(a1)
	move.l Fields.PreviousOrigin(a3), Event.PreviousOrigin(a1)
	move.l Fields.PreviousLine(a3), Event.PreviousLine(a1)
	move.l d2, Event.CaptureOpen(a1)
	move.l d3, Event.Origin(a1)
	move.l d4, Event.Line(a1)
	move.l d5, Event.Generated(a1)
	move.l Fields.Depth(a3), d0
	cmpi.l #LIMIT, d0
	bhs.w tableFull
	lsl.l #2, d0
	movea.l a3, a1
	adda.l #Fields.Stack, a1
	move.l d7, 0(a1, d0.l)
	addq.l #1, Fields.Depth(a3)
	addq.l #1, Fields.Count(a3)
close
	.WORK_PUBLICATION a3
	btst #source.FLAG_BLOCK_CLOSE_BIT, source.RecordHeader.Flags(a0)  ; shared BLOCK_CLOSE bit
	beq.w advance
	move.l Fields.Depth(a3), d0
	beq.w badExtent
	subq.l #1, d0
	move.l d0, Fields.Depth(a3)
	lsl.l #2, d0
	movea.l a3, a1
	adda.l #Fields.Stack, a1
	move.l 0(a1, d0.l), d0
	mulu.w #EVENT_BYTES, d0
	movea.l a3, a1
	adda.l #Fields.Events, a1
	adda.l d0, a1
	move.l d1, d0
	add.l d6, d0
	bcs.w badExtent
	move.l d0, Event.OriginalEnd(a1)
	move.l d0, Event.FinalEnd(a1)
	move.l d2, Event.CaptureClose(a1)
advance
	move.l d1, Fields.Previous(a3)
	move.l d2, Fields.PreviousCapture(a3)
	move.l d3, Fields.PreviousOrigin(a3)
	move.l d4, Fields.PreviousLine(a3)
	adda.l d6, a0
	add.l d6, d1
	bcs.w badExtent
	bra.w line
tableFull
	ori.l #EXHAUSTED, Fields.Errors(a3)
	bra.w done
badExtent
	ori.l #INVALID, Fields.Errors(a3)
done
	movem.l (sp)+, d0-d7/a0-a3
	move.w (sp)+, ccr
	rts
	.bend  ; prepared

; D0/D1=original ordered range,D2=materialized start. Record independent mapping
; for each boundary, including a preceding entry label in a different range.
remap	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a1, -(sp)
	lea State, a1
	move.l Fields.Count(a1), d7
	movea.l a1, a0
	adda.l #Fields.Events, a0
next
	tst.l d7
	beq.w done
	move.l Event.OriginalOpen(a0), d3
	bsr.w position
	tst.l d4
	beq.w previous
	move.l d3, Event.FinalOpen(a0)
previous
	move.l Event.OriginalPrevious(a0), d3
	bsr.w position
	tst.l d4
	beq.w endPosition
	move.l d3, Event.FinalPrevious(a0)
endPosition
	move.l Event.OriginalEnd(a0), d3
	cmp.l d0, d3
	bls.w advance
	cmp.l d1, d3
	bhi.w advance
	sub.l d0, d3
	add.l d2, d3
	bcs.w badExtent
	move.l d3, Event.FinalEnd(a0)
advance
	adda.w #EVENT_BYTES, a0
	subq.l #1, d7
	bra.w next
badExtent
	ori.l #INVALID, Fields.Errors(a1)
done
	movem.l (sp)+, d0-d7/a0-a1
	move.w (sp)+, ccr
	rts
	.bend  ; remap
	.priv
; Translate one original start iff this span contains it. CCR unspecified.
position	.block
	moveq #0, d4
	cmp.l d0, d3
	blo.w done
	cmp.l d1, d3
	bhs.w done
	sub.l d0, d3
	add.l d2, d3
	bcc.w translated
	ori.l #INVALID, Fields.Errors(a1)
	bra.w done
translated
	moveq #1, d4
done
	rts
	.bend  ; position
	.pub

; A0=authoritative block-index rows,D0=count. Link exact end and chosen start
; against publication events after ordered materialization. Existing block index
; owns preceding labels, nesting, entry identities and selection. No name lookup.
reconcile	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a3, -(sp)
	lea State, a3
	clr.l Fields.Matched(a3)
	clr.l Fields.Deferred(a3)
	clr.l Fields.Unmatched(a3)
	moveq #0, d7
unit
	tst.l d0
	beq.w done
	addq.l #1, d7
	move.l Fields.Count(a3), d6
	movea.l a3, a1
	adda.l #Fields.Events, a1
find
	tst.l d6
	beq.w unmatched
	move.l Event.FinalEnd(a1), d1
	cmp.l Span.End(a0), d1
	bne.w next
	move.l Event.FinalOpen(a1), d1
	cmp.l Span.Start(a0), d1
	beq.w linked
	move.l Event.FinalPrevious(a1), d1
	cmp.l Span.Start(a0), d1
	beq.w linked
next
	adda.w #EVENT_BYTES, a1
	subq.l #1, d6
	bra.w find
linked
	move.l d7, Event.Unit(a1)
	tst.l Event.Generated(a1)
	bne.w deferred
	addq.l #1, Fields.Matched(a3)
	bra.w advance
deferred
	addq.l #1, Fields.Deferred(a3)
	bra.w advance
unmatched
	addq.l #1, Fields.Unmatched(a3)
advance
	adda.w #SPAN_BYTES, a0
	subq.l #1, d0
	bra.w unit
done
	movem.l (sp)+, d0-d7/a0-a3
	move.w (sp)+, ccr
	rts
	.bend  ; reconcile

.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
; A0=DOS base. Aggregate coverage/errors remain complete; detail is first16
; rows explicitly counted. No truncation of the internal observer tables.
report	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a4
	lea State, a6
	move.l #710, d0
	move.l Fields.Matched(a6), d1
	move.l Fields.Deferred(a6), d2
	move.l Fields.Unmatched(a6), d3
	jsr memory_profile.progress
	move.l #711, d0
	move.l Fields.Errors(a6), d1
	move.l Fields.Count(a6), d2
	move.l Fields.Depth(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #712, d0
	move.l #STATE_BYTES, d1
	move.l #LIMIT, d2
	move.l #DETAIL_LIMIT, d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l Fields.Count(a6), d6
	cmpi.l #DETAIL_LIMIT, d6
	bls.w bounded
	move.l #DETAIL_LIMIT, d6
bounded
	move.l #1700, d4
	movea.l a6, a5
	adda.l #Fields.Events, a5
row
	tst.l d6
	beq.w done
	moveq #4, d5
triple
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d5, triple
	subq.l #1, d6
	bra.w row
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; report
.endif
.endif
.endif
	.endsection
	.section bss, kind=bss
	.priv
State	.res byte, STATE_BYTES
	.endsection
	.endmodule
