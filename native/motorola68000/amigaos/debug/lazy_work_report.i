; Separate work report schema v1; L1 phases remain unchanged.
	.pub
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
; A0=DOS base. Six triples per 18-long vector, seven per detail row.
report	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a4
	lea State, a6
	tst.l WorkFields.SelectionDone(a6)
	bne.w ready
	ori.l #SELECTION_MISSING, WorkFields.UnknownReasons(a6)
	bsr.w makePartition
ready
	move.l #2000, d0
	moveq #1, d1
	move.l WorkFields.Errors(a6), d2
	move.l WorkFields.Count(a6), d3
	jsr memory_profile.progress
	move.l #2001, d0
	move.l #WORK_STATE_BYTES, d1
	move.l #DETAIL_LIMIT, d2
	move.l WorkFields.Count(a6), d3
	cmpi.l #DETAIL_LIMIT, d3
	bls.w bounded
	move.l #DETAIL_LIMIT, d3
bounded
	move.l d3, d7
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #2002, d0
	move.l #capture.HEADER_BYTES, d1
	moveq #capture.WORK_COUNTER_BYTES, d2
	move.l WorkFields.UnknownReasons(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #2003, d0
	move.l WorkFields.Capacity(a6), d1
	move.l WorkFields.Totals+CAPTURE_BYTES(a6), d2
	move.l WorkFields.CaptureRecords(a6), d3
	lsl.l #2, d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #2010, d4
	lea WorkFields.Totals(a6), a5
	bsr.w vector
	move.l #2020, d4
	lea WorkFields.Selected(a6), a5
	bsr.w vector
	move.l #2030, d4
	lea WorkFields.Unreferenced(a6), a5
	bsr.w vector
	move.l #2040, d4
	lea WorkFields.Shared(a6), a5
	bsr.w vector
	move.l #2050, d4
	lea WorkFields.Unknown(a6), a5
	bsr.w vector
	moveq #1, d6
	move.l #2060, d4
	lea WorkFields.Rows(a6), a3
detailRow
	tst.l d7
	beq.w done
	move.l d4, d0
	move.l d6, d1
	move.l Row.Unit(a3), d2
	move.l Row.Reasons(a3), d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	lea Row.Values(a3), a5
	bsr.w vector
	adda.w #ROW_BYTES, a3
	addq.l #1, d6
	subq.l #1, d7
	bra.w detailRow
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; report
	.priv
vector	.block
	moveq #METRICS/3-1, d5
next
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d5, next
	rts
	.bend  ; vector
.endif
.endif
.endif
