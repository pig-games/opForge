; Passive bounded L1 shadow tables. No source parser, allocation or retirement.
; Scheduled closure combines owned copies, remaining spans and parent pins.
; Unscheduled regions stay deferred; observed last use alone is not proof.
; @opforge-owner: debug.amigaos.lazy_audit
	.module debug.amigaos.lazy_audit
	.cpu 68020
	.use debug.amigaos.lazy_units as lazy_units
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_graph as graph
	.include "binary_block_span.i"
	.include "capture_lifetime.i"
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.use debug.amigaos.memory_profile as memory_profile
.endif
.endif
.endif
	.pub
REGION_LIMIT = 256
UNIT_LIMIT = 2048
MAP_LIMIT = 256
DETAIL_LIMIT = 16
OVERFLOW = 1
EXHAUSTED = 2
INVALID = 4
PIN_UNDERFLOW = 8
MASK_REGIONS = 1
MASK_SCHEDULE_BIT = 1
MASK_SCHEDULE = 1<<MASK_SCHEDULE_BIT
MASK_UNITS = 4
MASK_SELECTION = 8
RETAIN_TRANSFER = 1
RETAIN_UNSCHEDULED = 2
RETAIN_PIN = 4
CLOSED_BIT = 4
CLOSED = 1<<CLOSED_BIT
LATE = 32
DISCHARGE_MASK = $ffffffff ^ (RETAIN_TRANSFER | RETAIN_PIN)
LOW_BYTE = 4-1; bit tests on a stored big-endian long use its final byte
; Counter identities are byte offsets in the scalar prefix.
Region	.struct
Base	.long ?
End	.long ?
File	.long ?
Used	.long ?
Capacity	.long ?
Accesses	.long ?
LastSpan	.long ?  ; last scheduled intersecting observeSpan ordinal+1
ObservedSpan	.long ?  ; latest exercised lookup ordinal+1, not a proof
Reasons	.long ?  ; 1 transfer unverified, 2 unscheduled, 4 suspended cursor
.endstruct
REGION_BYTES = Region.Reasons+4
Unit	.struct
Start	.long ?
End	.long ?
Entry	.long ?
Parent	.long ?
Live	.long ?  ; authoritative row flag; mask bit8 confirms selection ran
.endstruct
UNIT_BYTES = Unit.Live+4
Mapping	.struct
Start	.long ?
End	.long ?
File	.long ?
PreparedStart	.long ?
PreparedEnd	.long ?
Boundary	.long ?
ActualUsed	.long ?
ActualCapacity	.long ?
Eligible	.long ?
Predicted	.long ?
.endstruct
MAPPING_BYTES = Mapping.Predicted+4
Fields	.struct
Errors	.long ?
Mask	.long ?
RegionCount	.long ?
Physical	.long ?
Process	.long ?
Names	.long ?
Scheduled	.long ?
Completed	.long ?
Includes	.long ?
Pins	.long ?
MaxPins	.long ?
Lookups	.long ?
Units	.long ?
Mappings	.long ?
Used	.long ?
Capacity	.long ?
Classified	.long ?
Unavailable	.long ?
Released	.long ?
Materialized	.long ?
Eligible	.long ?
ActiveMapping	.long ?
PeakEligible	.long ?
EligibleRegions	.long ?
Late	.long ?
Regions	.res REGION_LIMIT*REGION_BYTES
UnitRows	.res UNIT_LIMIT*UNIT_BYTES
MapRows	.res MAP_LIMIT*MAPPING_BYTES
.endstruct
PHYSICAL = Fields.Physical
PROCESS = Fields.Process
NAMES = Fields.Names
INCLUDES = Fields.Includes
CLASSIFIED = Fields.Classified
UNAVAILABLE = Fields.Unavailable
RELEASED = Fields.Released
MATERIALIZED = Fields.Materialized
STATE_BYTES = Fields.MapRows+MAP_LIMIT*MAPPING_BYTES
	.section code, kind=code
	.pub
; Clear all scalar/table state before acquisition. No inputs or outputs.
; Every passive API preserves all registers, CCR and stack depth.
clear	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea State, a0
	move.w #STATE_BYTES/4-1, d0
next
	clr.l (a0)+
	dbra d0, next
	lea State, a0
	move.l #-1, Fields.ActiveMapping(a0)
	jsr lazy_units.clear
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; clear

; Read-only state view for the isolated component probe. A0 is the sole output.
; All other registers and CCR survive; no owner view is returned.
getState	.block
	lea State, a0
	rts
	.bend  ; getState

; A0=selected authoritative rows,D0=count. Passive snapshot plus selection bit.
selected	.block
	move.w ccr, -(sp)
	move.l a0, -(sp)
	jsr blocks
	lea State, a0
	ori.l #MASK_SELECTION, Fields.Mask(a0)
	movea.l (sp)+, a0
	move.w (sp)+, ccr
	rts
	.bend  ; selected

; D0=scalar byte offset, D1=increment. Invalid IDs are observer errors only.
count	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea State, a0
	cmpi.l #Fields.Regions, d0
	bhs.w badExtent
	btst #0, d0
	bne.w badExtent
	btst #1, d0
	bne.w badExtent
	adda.l d0, a0
	add.l d1, (a0)
	bcc.w done
	move.l #-1, (a0)
	lea State, a0
	ori.l #OVERFLOW, Fields.Errors(a0)
	bra.w done
badExtent
	lea State, a0
	ori.l #INVALID, Fields.Errors(a0)
done
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; count

; A0=published capture descriptor, D0=stable owner index. Descriptor layout
; uses capture.Region and the shared memory.Block extent contract.
; Copies only owned scalar extents; no borrowed view survives this call.
observeRegion	.block
	move.w ccr, -(sp)
	movem.l d0-d4/a0-a2, -(sp)
	lea State, a2
	cmpi.l #REGION_LIMIT, d0
	bhs.w tableFull
	move.l d0, d4
	addq.l #1, d4
	cmp.l Fields.RegionCount(a2), d4
	bls.w existing
	move.l d4, Fields.RegionCount(a2)
existing
	mulu.w #REGION_BYTES, d0
	movea.l a2, a1
	adda.l #Fields.Regions, a1
	adda.l d0, a1
	move.l capture.Region.Base(a0), Region.Base(a1)
	move.l capture.Region.End(a0), Region.End(a1)
	move.l capture.Region.File(a0), Region.File(a1)
	move.l Region.Capacity(a1), d1
	sub.l d1, Fields.Capacity(a2)
	move.l Region.Used(a1), d1
	sub.l d1, Fields.Used(a2)
	move.l capture.Region.Arena+memory.Block.Capacity(a0), Region.Capacity(a1)
	move.l capture.Region.Arena+memory.Block.Used(a0), Region.Used(a1)
	move.l Region.Capacity(a1), d1
	add.l d1, Fields.Capacity(a2)
	bcs.w badExtent
	move.l Region.Used(a1), d1
	add.l d1, Fields.Used(a2)
	bcs.w badExtent
	move.l Region.End(a1), d1
	sub.l Region.Base(a1), d1
	bcs.w badExtent
	cmp.l Region.Used(a1), d1
	bne.w badExtent
	cmp.l Region.Capacity(a1), d1
	bhi.w badExtent
	ori.l #RETAIN_TRANSFER, Region.Reasons(a1)
	btst #MASK_SCHEDULE_BIT, Fields.Mask+LOW_BYTE(a2)
	beq.w acquired
	tst.l Region.LastSpan(a1)
	bne.w acquired
	ori.l #RETAIN_UNSCHEDULED, Region.Reasons(a1)
acquired
	ori.l #MASK_REGIONS, Fields.Mask(a2)
	bra.w done
tableFull
	ori.l #EXHAUSTED, Fields.Errors(a2)
	bra.w done
badExtent
	ori.l #INVALID, Fields.Errors(a2)
done
	movem.l (sp)+, d0-d4/a0-a2
	move.w (sp)+, ccr
	rts
	.bend  ; observeRegion

; A0=successful capture descriptor lookup, D1=logical offset.
; Later access is compared to planned span position without claiming closure.
lookup	.block
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a2, -(sp)
	lea State, a2
	move.l #Fields.Lookups, d0
	bsr.w increment
	move.l capture.Region.Base(a0), d2
	move.l Fields.RegionCount(a2), d3
	movea.l a2, a1
	adda.l #Fields.Regions, a1
next
	tst.l d3
	beq.w done
	cmp.l Region.Base(a1), d2
	bne.w advance
	move.l capture.Region.File(a0), d0
	cmp.l Region.File(a1), d0
	bne.w advance
	move.l Region.Accesses(a1), d0
	addq.l #1, d0
	bcc.w accessed
	moveq #-1, d0
	ori.l #OVERFLOW, Fields.Errors(a2)
accessed
	move.l d0, Region.Accesses(a1)
	btst #CLOSED_BIT, Region.Reasons+LOW_BYTE(a1)
	beq.w ordinary
	ori.l #LATE, Region.Reasons(a1)
	move.l #Fields.Late, d0
	bsr.w increment
ordinary
	move.l Fields.Completed(a2), d0
	addq.l #1, d0
	move.l d0, Region.ObservedSpan(a1)
	tst.l Fields.Pins(a2)
	beq.w done
	ori.l #RETAIN_PIN, Region.Reasons(a1)
	bra.w done
advance
	adda.w #REGION_BYTES, a1
	subq.l #1, d3
	bra.w next
done
	movem.l (sp)+, d0-d3/a0-a2
	move.w (sp)+, ccr
	rts
	.bend  ; lookup

; D0=+1 suspend or -1 restore. Pins cover every saved parent cursor, including
; failed include acquisition. Reason bits remain conservative after restore.
pin	.block
	move.w ccr, -(sp)
	movem.l d0-d1/a0-a2, -(sp)
	lea State, a0
	lea State, a2
	tst.l d0
	bmi.w restore
	move.l #Fields.Includes, d0
	bsr.w increment
	move.l #Fields.Pins, d0
	bsr.w increment
	move.l Fields.Pins(a0), d0
	cmp.l Fields.MaxPins(a0), d0
	bls.w done
	move.l d0, Fields.MaxPins(a0)
	bra.w done
restore
	tst.l Fields.Pins(a0)
	beq.w badExtent
	subq.l #1, Fields.Pins(a0)
	bra.w done
badExtent
	ori.l #PIN_UNDERFLOW, Fields.Errors(a0)
done
	movem.l (sp)+, d0-d1/a0-a2
	move.w (sp)+, ccr
	rts
	.bend  ; pin

; A0=CaptureOrder memory.Block,D0=count,D1=span stride. Records are validated
; by production; this independent bounded walk only observes intersections.
schedule	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a3, -(sp)
	lea State, a3
	move.l d0, Fields.Scheduled(a3)
	cmpi.l #MAP_LIMIT, d0
	bls.w countBounded
	ori.l #EXHAUSTED, Fields.Errors(a3)
	move.l #MAP_LIMIT, d0
countBounded
	cmpi.l #graph.SPAN_BYTES, d1
	bne.w invalidSchedule
	move.l d0, d7
	move.l d1, d6
	mulu.w #graph.SPAN_BYTES, d0
	cmp.l memory.Block.Capacity(a0), d0
	bhi.w invalidSchedule
	movea.l memory.Block.Pointer(a0), a0
	move.l a0, d0
	bne.w hasTable
	tst.l d7
	bne.w invalidSchedule
hasTable
	moveq #0, d5
spanLoop
	tst.l d7
	beq.w complete
	addq.l #1, d5
	move.l Fields.RegionCount(a3), d4
	movea.l a3, a1
	adda.l #Fields.Regions, a1
regionLoop
	tst.l d4
	beq.w nextSpan
	move.l graph.Span.Start(a0), d2
	cmp.l Region.End(a1), d2
	bhs.w nextRegion
	move.l graph.Span.End(a0), d2
	cmp.l Region.Base(a1), d2
	bls.w nextRegion
	move.l d5, Region.LastSpan(a1)
nextRegion
	adda.w #REGION_BYTES, a1
	subq.l #1, d4
	bra.w regionLoop
nextSpan
	adda.l d6, a0
	subq.l #1, d7
	bra.w spanLoop
complete
	move.l Fields.RegionCount(a3), d4
	movea.l a3, a1
	adda.l #Fields.Regions, a1
retention
	tst.l d4
	beq.w sealed
	tst.l Region.LastSpan(a1)
	bne.w scheduled
	ori.l #RETAIN_UNSCHEDULED, Region.Reasons(a1)
scheduled
	adda.w #REGION_BYTES, a1
	subq.l #1, d4
	bra.w retention
sealed
	ori.l #MASK_SCHEDULE, Fields.Mask(a3)
	bra.w scheduleDone
invalidSchedule
	ori.l #INVALID, Fields.Errors(a3)
scheduleDone
	movem.l (sp)+, d0-d7/a0-a3
	move.w (sp)+, ccr
	rts
	.bend  ; schedule

; D0/D1=capture range,D2=file,D3=original prepared offset. One row per
; scheduled whole span, not per line. Generated records share their parent span.
observeSpan	.block
	move.w ccr, -(sp)
	movem.l d4/a0-a1, -(sp)
	lea State, a1
	move.l #-1, Fields.ActiveMapping(a1)
	move.l Fields.Mappings(a1), d4
	cmpi.l #MAP_LIMIT, d4
	bhs.w tableFull
	move.l d4, Fields.ActiveMapping(a1)
	mulu.w #MAPPING_BYTES, d4
	movea.l a1, a0
	adda.l #Fields.MapRows, a0
	adda.l d4, a0
	move.l d0, Mapping.Start(a0)
	move.l d1, Mapping.End(a0)
	move.l d2, Mapping.File(a0)
	move.l d3, Mapping.PreparedStart(a0)
	move.l #-1, Mapping.PreparedEnd(a0)
	addq.l #1, Fields.Mappings(a1)
	bra.w done
tableFull
	ori.l #EXHAUSTED, Fields.Errors(a1)
done
	movem.l (sp)+, d4/a0-a1
	move.w (sp)+, ccr
	rts
	.bend  ; observeSpan

; D0=prepared end after successful span replay. Failed rows retain ffffffff.
spanEnd	.block
	move.w ccr, -(sp)
	movem.l d0-d1/a0-a2, -(sp)
	lea State, a1
	move.l Fields.ActiveMapping(a1), d1
	cmpi.l #MAP_LIMIT, d1
	bhs.w done
	move.l #-1, Fields.ActiveMapping(a1)
	mulu.w #MAPPING_BYTES, d1
	movea.l a1, a0
	adda.l #Fields.MapRows, a0
	adda.l d1, a0
	move.l d0, Mapping.PreparedEnd(a0)
	movea.l a1, a2
	move.l #Fields.Completed, d0
	bsr.w increment
	bsr.w closeConsumers
	move.l Fields.Completed(a2), Mapping.Boundary(a0)
	move.l Fields.Used(a2), Mapping.ActualUsed(a0)
	move.l Fields.Capacity(a2), Mapping.ActualCapacity(a0)
	move.l Fields.Eligible(a2), Mapping.Eligible(a0)
	move.l Fields.Capacity(a2), d0
	sub.l Fields.Eligible(a2), d0
	bcc.w residency
	ori.l #INVALID, Fields.Errors(a2)
	moveq #0, d0
residency
	move.l d0, Mapping.Predicted(a0)
	; Historical closure classification only: deferred recipes can retain captures.
	.CAPTURE_LIFE_CURVE Fields.Completed(a2), Fields.Eligible(a2), Fields.Capacity(a2), Fields.Pins(a2), Fields.RegionCount(a2)
done
	movem.l (sp)+, d0-d1/a0-a2
	move.w (sp)+, ccr
	rts
	.bend  ; spanEnd

; A0=authoritative first block row,D0=count; row width 14. Repeated snapshot
; after selection overwrites Live while preserving exact ranges and entry/parent.
blocks	.block
	move.w ccr, -(sp)
	movem.l d0-d2/a0-a2, -(sp)
	lea State, a2
	jsr lazy_units.reconcile
	cmpi.l #UNIT_LIMIT, d0
	bls.w bounded
	ori.l #EXHAUSTED, Fields.Errors(a2)
	move.l #UNIT_LIMIT, d0
bounded
	move.l d0, Fields.Units(a2)
	movea.l a2, a1
	adda.l #Fields.UnitRows, a1
next
	tst.l d0
	beq.w done
	move.l Span.Start(a0), (a1)+
	move.l Span.End(a0), (a1)+
	moveq #0, d1
	move.w Span.Entry(a0), d1
	move.l d1, (a1)+
	moveq #0, d1
	move.w Span.Parent(a0), d1
	move.l d1, (a1)+
	moveq #0, d1
	move.w Span.Live(a0), d1
	move.l d1, (a1)+
	adda.w #SPAN_BYTES, a0
	subq.l #1, d0
	bra.w next
done
	ori.l #MASK_UNITS, Fields.Mask(a2)
	movem.l (sp)+, d0-d2/a0-a2
	move.w (sp)+, ccr
	rts
	.bend  ; blocks

	.priv
; Successful replay has returned every bounded seek/peek and drained expansions.
; CAPTURE_RECORD is cleared; macro recipes and BODY records were copied, and
; diagnostic paths/peek records have independent owners. Combine those verified
; transfers with the completed schedule and suspended cursor pins. Last access
; is not the proof. Unscheduled child regions remain explicitly deferred.
closeConsumers	.block
	movem.l d0-d4/a0-a2, -(sp)
	tst.l Fields.Errors(a2)
	bne.w done
	tst.l Fields.Pins(a2)
	bne.w done
	move.l Fields.RegionCount(a2), d4
	movea.l a2, a1
	adda.l #Fields.Regions, a1
next
	tst.l d4
	beq.w done
	move.l Region.LastSpan(a1), d1
	beq.w advance
	cmp.l Fields.Completed(a2), d1
	bhi.w advance
	btst #CLOSED_BIT, Region.Reasons+LOW_BYTE(a1)
	bne.w advance
	andi.l #DISCHARGE_MASK, Region.Reasons(a1)  ; transfer/pin reasons discharged
	ori.l #CLOSED, Region.Reasons(a1)
	move.l Region.Capacity(a1), d1
	add.l d1, Fields.Eligible(a2)
	bcc.w eligible
	move.l #-1, Fields.Eligible(a2)
	ori.l #OVERFLOW, Fields.Errors(a2)
eligible
	move.l #Fields.EligibleRegions, d0
	bsr.w increment
	move.l Fields.Eligible(a2), d0
	cmp.l Fields.PeakEligible(a2), d0
	bls.w advance
	move.l d0, Fields.PeakEligible(a2)
advance
	adda.w #REGION_BYTES, a1
	subq.l #1, d4
	bra.w next
done
	movem.l (sp)+, d0-d4/a0-a2
	rts
	.bend  ; closeConsumers
	.pub
	.priv
; A2=scalar state,D0=field offset. Local saturating increment; preserves A0/D1.
increment	.block
	movem.l a0, -(sp)
	lea 0(a2, d0.l), a0
	addq.l #1, (a0)
	bcc.w done
	move.l #-1, (a0)
	ori.l #OVERFLOW, Fields.Errors(a2)
done
	movea.l (sp)+, a0
	rts
	.bend  ; increment
	.pub
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
; A0=DOS base. Bounded terminal report; phase 700=version/errors/mask,
; 701=state bytes/region limit/region count; 702..707 are six scalar triples.
; 708=printed detail limit/unit limit/map limit; unit observer adds710..712.
; Row groups: region 720+3*i, unit 1120+2*i, mapping 1400+2*i.
report	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a4
	lea State, a6
	move.l #700, d0
	moveq #1, d1
	move.l Fields.Errors(a6), d2
	move.l Fields.Mask(a6), d3
	jsr memory_profile.progress
	move.l #701, d0
	move.l #STATE_BYTES, d1
	move.l #REGION_LIMIT, d2
	move.l Fields.RegionCount(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	lea Fields.Physical(a6), a5
	move.l #702, d4
	moveq #5, d5
scalar
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d5, scalar
	movea.l a6, a5
	adda.l #Fields.Regions, a5
	move.l Fields.RegionCount(a6), d6
	move.l #720, d4
	bsr.w detailCount
	moveq #3, d7
	bsr.w rows
	movea.l a6, a5
	adda.l #Fields.UnitRows, a5
	move.l Fields.Units(a6), d6
	move.l #1120, d4
	move.l #UNIT_BYTES, d7
	bsr.w detailCount
	bsr.w pairs
	movea.l a6, a5
	adda.l #Fields.MapRows, a5
	move.l Fields.Mappings(a6), d6
	move.l #1400, d4
	move.l #MAPPING_BYTES, d7
	bsr.w detailCount
	bsr.w pairs
	movea.l a6, a5
	adda.l #Fields.MapRows+Mapping.Boundary, a5
	move.l Fields.Mappings(a6), d6
	move.l #1800, d4
	move.l #MAPPING_BYTES, d7
	bsr.w detailCount
	bsr.w pairs
	move.l #713, d0
	move.l Fields.Eligible(a6), d1
	move.l Fields.PeakEligible(a6), d2
	move.l Fields.Capacity(a6), d3
	sub.l d1, d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #714, d0
	move.l Fields.EligibleRegions(a6), d1
	move.l Fields.RegionCount(a6), d2
	sub.l d1, d2
	move.l Fields.Late(a6), d3
	movea.l a4, a0
	jsr memory_profile.progress
	move.l #708, d0
	move.l #DETAIL_LIMIT, d1
	move.l #UNIT_LIMIT, d2
	move.l #MAP_LIMIT, d3
	movea.l a4, a0
	jsr memory_profile.progress
	movea.l a4, a0
	jsr lazy_units.report
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; report
	.priv
; Limit printed detail only. Complete aggregate counts remain in the report.
detailCount	.block
	cmpi.l #DETAIL_LIMIT, d6
	bls.w done
	move.l #DETAIL_LIMIT, d6
done
	rts
	.bend  ; detailCount
; Five-word rows are emitted as two triples with a zero final field.
pairs	.block
	tst.l d6
	beq.w done
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	moveq #0, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	move.l d7, d0
	subi.l #5*4, d0
	adda.l d0, a5
	subq.l #1, d6
	bra.w pairs
done
	rts
	.bend  ; pairs
rows	.block
	tst.l d6
	beq.w done
	move.l d7, d5
	subq.l #1, d5
next
	move.l d4, d0
	move.l (a5)+, d1
	move.l (a5)+, d2
	move.l (a5)+, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d4
	dbra d5, next
	subq.l #1, d6
	bra.w rows
done
	rts
	.bend  ; rows
.endif
.endif
.endif
	.endsection
	.section bss, kind=bss
	.priv
State	.res byte, STATE_BYTES
	.endsection
	.endmodule
