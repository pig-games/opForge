; Terminal instrumented report. Count-only images contain no I/O or report code.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
LIFE_PROGRESS	.macro phaseId, first, second, third
	move.l .phaseId, d0
	move.l .first, d1
	move.l .second, d2
	move.l .third, d3
	movea.l a4, a0
	jsr memory_profile.progress
.endmacro
	.pub
; A0=DOS base; full passive preservation. Decimal phase band600000..619999.
; Hypothetical compact capacities preserve region partitions, and cannot predict
; growth overlap, a changed execution peak, or safe early retirement.
report	.block
	.LIFE_SAVE
	movea.l a0, a4
	lea State, a6
	bsr.w verifyReport
	.LIFE_PROGRESS #600000, #1, Fields.Errors(a6), Fields.Mode(a6)
	.LIFE_PROGRESS #600001, #STATE_BYTES, #PHASE_COUNT, #CURVE_LIMIT
	.LIFE_PROGRESS #600002, Fields.HeapLive(a6), Fields.HeapPeak(a6), Fields.CaptureLive(a6)
	.LIFE_PROGRESS #600003, Fields.CapturePeak(a6), Fields.Allocated(a6), Fields.Freed(a6)
	.LIFE_PROGRESS #600004, Fields.AllocCalls(a6), Fields.FreeCalls(a6), Fields.CurveCount(a6)
	.LIFE_PROGRESS #600005, Fields.Event(a6), Fields.Phase(a6), Fields.RegionCount(a6)
	move.l #600010, d4
	lea Fields.HeapPeakRow(a6), a5
	moveq #1, d5
	bsr.w triples
	move.l #600012, d4
	lea Fields.CapturePeakRow(a6), a5
	moveq #1, d5
	bsr.w triples
	.LIFE_PROGRESS #600020, Fields.Records(a6), Fields.Tokens(a6), Fields.OriginalBytes(a6)
	.LIFE_PROGRESS #600021, Fields.CompactBytes(a6), Fields.FallbackRecords(a6), Fields.FallbackTokens(a6)
	.LIFE_PROGRESS #600022, Fields.HeaderBytes(a6), Fields.LexemeBytes(a6), Fields.PlanBytes(a6)
	.LIFE_PROGRESS #600023, Fields.Maxima+Maxima.Count(a6), Fields.Maxima+Maxima.SourceBytes(a6), Fields.Maxima+Maxima.LexemeBytes(a6)
	.LIFE_PROGRESS #600024, Fields.Maxima+Maxima.Kind(a6), Fields.Maxima+Maxima.Reserved(a6), Fields.Maxima+Maxima.Start(a6)
	.LIFE_PROGRESS #600025, Fields.Maxima+Maxima.End(a6), Fields.Maxima+Maxima.Offset(a6), Fields.Maxima+Maxima.Length(a6)
	.LIFE_PROGRESS #600026, Fields.RegionCount(a6), Fields.ActualCapSum(a6), Fields.CompactCapSum(a6)
	.LIFE_PROGRESS #600027, Fields.RecordHeaderWidth(a6), #WIDE_TOKEN_BYTES, #COMPACT_TOKEN_BYTES
	move.l #600100, d4
	lea Fields.PhasePeaks(a6), a5
	moveq #PHASE_COUNT*2-1, d5
	bsr.w triples
	moveq #0, d7
	lea Fields.Curves(a6), a3
curveRow
	cmp.l Fields.CurveCount(a6), d7
	bhs.w regions
	cmpi.l #CURVE_LIMIT, d7
	bhs.w regions
	move.l d7, d4
	mulu.w #3, d4
	addi.l #601000, d4
	movea.l a3, a5
	moveq #2, d5
	bsr.w triples
	adda.w #CURVE_BYTES, a3
	addq.l #1, d7
	bra.w curveRow
regions
	moveq #0, d7
	lea Fields.Regions(a6), a3
regionRow
	cmp.l Fields.RegionCount(a6), d7
	bhs.w done
	cmpi.l #REGION_LIMIT, d7
	bhs.w done
	move.l d7, d4
	mulu.w #3, d4
	addi.l #602000, d4
	movea.l a3, a5
	moveq #2, d5
	bsr.w triples
	adda.w #REGION_BYTES, a3
	addq.l #1, d7
	bra.w regionRow
done
	.LIFE_RESTORE
	rts
	.bend  ; report
	.priv
; A5=scalar rows,D4=first phase,D5=number of triples minus1,A4=DOS base.
triples	.block
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
	.bend  ; triples
; Independent per-region conservation and terminal heap/mode checks. The six
; stack scalars match Records..FallbackTokens order; no persistent scratch view.
verifyReport	.block
	suba.w #6*4, sp
	movea.l sp, a5
	movea.l a5, a0
	moveq #5, d0
clear
	clr.l (a0)+
	dbra d0, clear
	clr.l Fields.ActualCapSum(a6)
	clr.l Fields.CompactCapSum(a6)
	tst.l Fields.Mode(a6)
	beq.w heap
	ori.l #BAD_MODE, Fields.Errors(a6)
heap
	move.l Fields.Allocated(a6), d0
	sub.l Fields.Freed(a6), d0
	bcs.w mismatch
	cmp.l Fields.HeapLive(a6), d0
	bne.w mismatch
	move.l Fields.HeapLive(a6), d0
	cmp.l Fields.CaptureLive(a6), d0
	bhs.w counts
mismatch
	ori.l #RECONCILE, Fields.Errors(a6)
counts
	cmpi.l #REGION_LIMIT, Fields.RegionCount(a6)
	bhi.w invalidCount
	cmpi.l #CURVE_LIMIT, Fields.CurveCount(a6)
	bls.w startRows
invalidCount
	ori.l #BOUNDS, Fields.Errors(a6)
startRows
	moveq #0, d7
	lea Fields.Regions(a6), a3
row
	cmp.l Fields.RegionCount(a6), d7
	bhs.w compare
	cmpi.l #REGION_LIMIT, d7
	bhs.w compare
	cmp.l RegionRow.Index(a3), d7
	bne.w invalidRegion
	move.l RegionRow.OriginalUsed(a3), d0
	cmp.l RegionRow.ActualCapacity(a3), d0
	bhi.w invalidRegion
	move.l RegionRow.CompactUsed(a3), d0
	cmp.l RegionRow.OriginalUsed(a3), d0
	bls.w capacity
invalidRegion
	ori.l #RECONCILE, Fields.Errors(a6)
capacity
	bsr.w compactCapacity
	cmp.l RegionRow.ActualCapacity(a3), d0
	bls.w compactBounded
	ori.l #RECONCILE, Fields.Errors(a6)
compactBounded
	.LIFE_ADD Fields.CompactCapSum, d0
	.LIFE_ADD Fields.ActualCapSum, RegionRow.ActualCapacity(a3)
	moveq #0, d6
scalar
	move.l d6, d2
	addi.l #RegionRow.Records, d2
	cmpi.l #4*4, d6
	blo.w contiguous
	addq.l #4, d2  ; ActualCapacity is reported separately, not a work total
contiguous
	move.l 0(a3, d2.l), d0
	movea.l a5, a0
	adda.l d6, a0
	bsr.w addScalar
	addq.l #4, d6
	cmpi.l #6*4, d6
	blo.w scalar
	adda.w #REGION_BYTES, a3
	addq.l #1, d7
	bra.w row
compare
	lea Fields.Records(a6), a0
	moveq #5, d2
next
	move.l (a5)+, d0
	cmp.l (a0)+, d0
	beq.w equal
	ori.l #RECONCILE, Fields.Errors(a6)
equal
	dbra d2, next
	adda.w #6*4, sp
	rts
	.bend  ; verifyReport
; A3=Region,D0=smallest power2 from256 covering CompactUsed, or0 empty.
; Saturate an unrepresentable capacity and invalidate, never silently clip.
compactCapacity	.block
	moveq #0, d0
	tst.l RegionRow.CompactUsed(a3)
	beq.w done
	move.l #256, d0
next
	cmp.l RegionRow.CompactUsed(a3), d0
	bhs.w done
	add.l d0, d0
	bcc.w next
	moveq #-1, d0
	ori.l #OVERFLOW, Fields.Errors(a6)
done
	rts
	.bend  ; compactCapacity
.endif
.endif
.endif
