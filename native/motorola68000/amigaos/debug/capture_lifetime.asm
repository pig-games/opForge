; Bounded capture lifetime/field investigation. No allocator, capture, or source
; imports: those owners call this passive module with scalar/typed snapshots.
; @opforge-owner: opforge.debug.capture_lifetime
	.module opforge.debug.capture_lifetime
	.cpu 68020
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.use opforge.debug.memory_profile as memory_profile
.endif
.endif
.endif
	.pub
	.include "capture_lifetime_layout.i"
LIFE_SAVE	.macro
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
.endmacro
LIFE_RESTORE	.macro
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
.endmacro
; Private update macro; A6=State. Field is an offset, amount may use D0.
LIFE_ADD	.macro field, amount
	move.l .amount, d0
	lea .field(a6), a0
	bsr.w addScalar
.endmacro
	.section code, kind=code
	.pub
; Every passive API preserves all registers, CCR and SP on every path.
clear	.block
	.LIFE_SAVE
	lea State, a0
	move.l #STATE_BYTES/4-1, d0
next
	clr.l (a0)+
	dbra d0, next
	.LIFE_RESTORE
	rts
	.bend  ; clear
; A0=borrowed scalar state for the isolated component probe; sole ABI output.
getState	.block
	lea State, a0
	rts
	.bend  ; getState
; D0=successful allocation capacity, in the existing owned-heap domain.
allocate	.block
	.LIFE_SAVE
	lea State, a6
	move.l d0, d7
	.LIFE_ADD Fields.Event, #1
	.LIFE_ADD Fields.AllocCalls, #1
	.LIFE_ADD Fields.Allocated, d7
	.LIFE_ADD Fields.HeapLive, d7
	tst.l Fields.Mode(a6)
	beq.w peaks
	cmpi.l #1, Fields.Mode(a6)
	bne.w invalid
	.LIFE_ADD Fields.CaptureLive, d7
peaks
	bsr.w updatePeaks
	bra.w done
invalid
	ori.l #BAD_MODE, Fields.Errors(a6)
done
	.LIFE_RESTORE
	rts
	.bend  ; allocate
; D0=capacity about to free. Retained classification brackets release as well
; as reserve, including old/new overlap during growth.
release	.block
	.LIFE_SAVE
	lea State, a6
	move.l d0, d7
	.LIFE_ADD Fields.Event, #1
	.LIFE_ADD Fields.FreeCalls, #1
	.LIFE_ADD Fields.Freed, d7
	move.l d7, d0
	lea Fields.HeapLive(a6), a0
	bsr.w subtractScalar
	tst.l Fields.Mode(a6)
	beq.w verifySubset
	cmpi.l #1, Fields.Mode(a6)
	bne.w invalid
	move.l d7, d0
	lea Fields.CaptureLive(a6), a0
	bsr.w subtractScalar
verifySubset
	move.l Fields.HeapLive(a6), d0
	cmp.l Fields.CaptureLive(a6), d0
	bhs.w done
	ori.l #RECONCILE, Fields.Errors(a6)
	bra.w done
invalid
	ori.l #BAD_MODE, Fields.Errors(a6)
done
	.LIFE_RESTORE
	rts
	.bend  ; release
; Begin a non-nesting synchronous retained reserve/release scope.
retainedBegin	.block
	.LIFE_SAVE
	lea State, a6
	tst.l Fields.Mode(a6)
	bne.w invalid
	move.l #1, Fields.Mode(a6)
	bra.w done
invalid
	ori.l #BAD_MODE, Fields.Errors(a6)
done
	.LIFE_RESTORE
	rts
	.bend  ; retainedBegin
; End the same scope even when the reserve failed.
retainedEnd	.block
	.LIFE_SAVE
	lea State, a6
	cmpi.l #1, Fields.Mode(a6)
	bne.w invalid
	clr.l Fields.Mode(a6)
	bra.w done
invalid
	ori.l #BAD_MODE, Fields.Errors(a6)
done
	.LIFE_RESTORE
	rts
	.bend  ; retainedEnd
; D0=coarse lifecycle phase, 0..9. Does not create heap events or a phase stack.
phase	.block
	.LIFE_SAVE
	lea State, a6
	cmpi.l #PHASE_COUNT, d0
	bhs.w invalid
	move.l d0, Fields.Phase(a6)
	bsr.w updatePhasePeak
	bra.w done
invalid
	ori.l #BOUNDS, Fields.Errors(a6)
done
	.LIFE_RESTORE
	rts
	.bend  ; phase
; D0=next sequential zero-based region index,D1=physical file identity.
region	.block
	.LIFE_SAVE
	lea State, a6
	cmp.l Fields.RegionCount(a6), d0
	bne.w invalid
	cmpi.l #REGION_LIMIT, d0
	bhs.w invalid
	move.l d0, d2
	mulu.w #REGION_BYTES, d2
	lea Fields.Regions(a6), a0
	adda.l d2, a0
	move.l d0, RegionRow.Index(a0)
	move.l d1, RegionRow.File(a0)
	addq.l #1, Fields.RegionCount(a6)
	bra.w done
invalid
	ori.l #BOUNDS, Fields.Errors(a6)
done
	.LIFE_RESTORE
	rts
	.bend  ; region
; D0=next 1-based completed L1 span,D1=eligible,D2=model capacity,
; D3=pins,D4=registered regions. Eligibility remains a shadow prediction.
curve	.block
	.LIFE_SAVE
	lea State, a6
	move.l Fields.CurveCount(a6), d5
	cmpi.l #CURVE_LIMIT, d5
	bhs.w invalid
	addq.l #1, d5
	cmp.l d5, d0
	bne.w invalid
	cmp.l d2, d1
	bhi.w mismatch
	cmp.l Fields.CaptureLive(a6), d2
	bne.w mismatch
	cmp.l Fields.RegionCount(a6), d4
	bne.w mismatch
	move.l d0, Fields.Span(a6)
	move.l d1, Fields.Eligible(a6)
	move.l Fields.CurveCount(a6), d5
	mulu.w #CURVE_BYTES, d5
	lea Fields.Curves(a6), a0
	adda.l d5, a0
	move.l d0, CurveRow.Span(a0)
	move.l Fields.Event(a6), CurveRow.Event(a0)
	move.l Fields.Phase(a6), CurveRow.Phase(a0)
	move.l Fields.HeapLive(a6), CurveRow.Heap(a0)
	move.l Fields.CaptureLive(a6), CurveRow.Capture(a0)
	move.l d1, CurveRow.Eligible(a0)
	move.l d2, CurveRow.ModelCapacity(a0)
	move.l d3, CurveRow.Pins(a0)
	move.l d4, CurveRow.Regions(a0)
	addq.l #1, Fields.CurveCount(a6)
	bra.w done
invalid
	ori.l #BOUNDS, Fields.Errors(a6)
	bra.w done
mismatch
	ori.l #RECONCILE, Fields.Errors(a6)
done
	.LIFE_RESTORE
	rts
	.bend  ; curve
	.priv
; A0=scalar,D0=unsigned increment,A6=State. Clobbers D1/CCR.
addScalar	.block
	move.l (a0), d1
	add.l d0, d1
	bcc.w store
	moveq #-1, d1
	ori.l #OVERFLOW, Fields.Errors(a6)
store
	move.l d1, (a0)
	rts
	.bend  ; addScalar
; A0=live scalar,D0=decrement,A6=State. Clamp underflow to zero and invalidate.
subtractScalar	.block
	cmp.l (a0), d0
	bhi.w invalid
	sub.l d0, (a0)
	rts
invalid
	clr.l (a0)
	ori.l #UNDERFLOW, Fields.Errors(a6)
	rts
	.bend  ; subtractScalar
; Update independent global/retained and current phase heap peaks. Equal peaks
; retain the first observation; rows are synchronized at allocation events.
updatePeaks	.block
	move.l Fields.HeapLive(a6), d0
	cmp.l Fields.CaptureLive(a6), d0
	bhs.w heap
	ori.l #RECONCILE, Fields.Errors(a6)
heap
	cmp.l Fields.HeapPeak(a6), d0
	bls.w retained
	move.l d0, Fields.HeapPeak(a6)
	lea Fields.HeapPeakRow(a6), a0
	bsr.w snapshotPeak
retained
	move.l Fields.CaptureLive(a6), d0
	cmp.l Fields.CapturePeak(a6), d0
	bls.w phasePeak
	move.l d0, Fields.CapturePeak(a6)
	lea Fields.CapturePeakRow(a6), a0
	bsr.w snapshotPeak
phasePeak
	bsr.w updatePhasePeak
	rts
	.bend  ; updatePeaks
; Include inherited residency on phase entry without advancing heap events.
updatePhasePeak	.block
	move.l Fields.Phase(a6), d0
	cmpi.l #PHASE_COUNT, d0
	bhs.w invalid
	mulu.w #PEAK_BYTES, d0
	lea Fields.PhasePeaks(a6), a0
	adda.l d0, a0
	move.l Fields.HeapLive(a6), d0
	cmp.l PeakRow.Heap(a0), d0
	bls.w done
	bsr.w snapshotPeak
	bra.w done
invalid
	ori.l #BOUNDS, Fields.Errors(a6)
done
	rts
	.bend  ; updatePhasePeak
; A0=owned Peak row,A6=State. No borrowed source pointer retained.
snapshotPeak	.block
	move.l Fields.Event(a6), PeakRow.Event(a0)
	move.l Fields.Phase(a6), PeakRow.Phase(a0)
	move.l Fields.HeapLive(a6), PeakRow.Heap(a0)
	move.l Fields.CaptureLive(a6), PeakRow.Capture(a0)
	move.l Fields.Eligible(a6), PeakRow.Eligible(a0)
	move.l Fields.Span(a6), PeakRow.Span(a0)
	rts
	.bend  ; snapshotPeak
	.include "capture_lifetime_inspect.i"
	.include "capture_lifetime_report.i"
	.endsection
	.section bss, kind=bss
	.priv
State	.res byte, STATE_BYTES
	.endsection
	.endmodule
