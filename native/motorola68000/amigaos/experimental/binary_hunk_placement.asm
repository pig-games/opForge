; Bounded numeric placement, independent of Hunk serialization order.
; @opforge-owner: experimental.amigaos.binary_hunk_placement
	.module experimental.amigaos.binary_hunk_placement
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_package as pkg
	.pub
RegionSlot	.struct
Seen	.long ?
Base	.long ?
End	.long ?
Align	.long ?
Cursor	.long ?
	.endstruct
REGION_BYTES = RegionSlot.Cursor+4
PlaceEntry	.struct
Section	.word ?
Region	.word ?
Align	.long ?
	.endstruct
PLACE_BYTES = PlaceEntry.Align+4
REGIONS = sections.PLACEMENT_SCRATCH
PLACES = REGIONS+8*REGION_BYTES
BASES = PLACES+8*PLACE_BYTES
	.section code, kind=code
	.pub
; A0=records,D0=bytes,A1=section state,D1=max address.
; D0/CCR=status; all other registers preserved. Metadata is retained in state.
scan	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	move.l d1, d6
	move.l a0, d7
	add.l d0, d7
	bcs.w bad
	clr.w sections.State.PlaceCount(a6)
	clr.w sections.State.PlaceReady(a6)
	lea REGIONS(a6), a5
	move.w #sections.PLACEMENT_BYTES/2-1, d0
clear
	clr.w (a5)+
	dbra d0, clear
	moveq #0, d5
record
	cmp.l a0, d7
	beq.w finish
	blo.w bad
	moveq #0, d4
	move.b (a0), d4
	addq.w #1, d4
	cmpi.w #4, d4
	blo.w bad
	move.l d7, d0
	sub.l a0, d0
	cmp.l d0, d4
	bhi.w bad
	btst #4, 1(a0)
	beq.w next
	cmpi.w #5, d4
	blo.w bad
	moveq #0, d0
	move.b 4(a0), d0
	cmpi.w #4, d0
	beq.w region
	cmpi.w #8, d0
	beq.w region
	cmpi.w #22, d0
	beq.w region
	cmpi.w #5, d0
	beq.w place
	cmpi.w #9, d0
	beq.w place
	cmpi.w #23, d0
	bne.w next
place
	cmpi.w #11, d4
	bne.w bad
	moveq #0, d0
	move.b 5(a0), d0
	cmpi.w #8, d0
	bhs.w bad
	btst d0, d5
	bne.w bad
	bset d0, d5
	move.l d0, d2
	bsr.w sectionAddress
	tst.w sections.HunkSlot.Seen(a5)
	beq.w bad
	tst.w sections.HunkSlot.Target(a5)
	bne.w bad
	moveq #0, d3
	move.b 6(a0), d3
	cmpi.w #8, d3
	bhs.w bad
	move.l 7(a0), d0
	bsr.w alignment
	bne.w bad
	moveq #0, d1
	move.w sections.State.PlaceCount(a6), d1
	cmpi.w #8, d1
	bhs.w bad
	mulu.w #PLACE_BYTES, d1
	lea PLACES(a6), a5
	adda.l d1, a5
	move.w d2, PlaceEntry.Section(a5)
	move.w d3, PlaceEntry.Region(a5)
	move.l d0, PlaceEntry.Align(a5)
	addq.w #1, sections.State.PlaceCount(a6)
	bra.w next
region
	cmpi.w #18, d4
	bne.w bad
	moveq #0, d0
	move.b 13(a0), d0
	cmpi.w #8, d0
	bhs.w bad
	bsr.w regionAddress
	tst.l RegionSlot.Seen(a5)
	bne.w bad
	move.l 5(a0), d2
	move.l 9(a0), d3
	cmp.l d2, d3
	blo.w bad
	cmp.l d6, d3
	bhi.w bad
	move.l 14(a0), d0
	bsr.w alignment
	bne.w bad
	move.l d0, RegionSlot.Align(a5)
	move.l d2, RegionSlot.Base(a5)
	move.l d3, RegionSlot.End(a5)
	move.l #1, RegionSlot.Seen(a5)
	lea REGIONS(a6), a4
	moveq #7, d1
regionOverlap
	cmpa.l a4, a5
	beq.w nextRegion
	tst.l RegionSlot.Seen(a4)
	beq.w nextRegion
	cmp.l RegionSlot.End(a4), d2
	bhi.w nextRegion
	cmp.l RegionSlot.Base(a4), d3
	bhs.w bad
nextRegion
	adda.w #REGION_BYTES, a4
	dbra d1, regionOverlap
next
	adda.l d4, a0
	bra.w record
finish
	lea PLACES(a6), a4
	move.w sections.State.PlaceCount(a6), d4
	beq.w good
checkRegion
	moveq #0, d0
	move.w PlaceEntry.Region(a4), d0
	bsr.w regionAddress
	tst.l RegionSlot.Seen(a5)
	beq.w bad
	adda.w #PLACE_BYTES, a4
	subq.w #1, d4
	bne.w checkRegion
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	.TELEMETRY_SERVICE_LEAVE
	tst.l d0
	rts
	.bend  ; scan

; A0=state,A1=Context. D0/CCR=status,D1=origins changed (boolean).
; Uses high-water Allocation plus mapped Allocation. Others preserved; publishes bases.
freeze	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	moveq #0, d7
	lea REGIONS(a6), a5
	moveq #7, d4
resetRegions
	move.l RegionSlot.Base(a5), RegionSlot.Cursor(a5)
	adda.w #REGION_BYTES, a5
	dbra d4, resetRegions
	lea PLACES(a6), a4
	move.w sections.State.PlaceCount(a6), d4
	beq.w publish
placement
	moveq #0, d0
	move.w PlaceEntry.Region(a4), d0
	bsr.w regionAddress
	movea.l a5, a3
	moveq #0, d0
	move.w PlaceEntry.Section(a4), d0
	bsr.w sectionAddress
	movea.l a5, a2
	move.l RegionSlot.Align(a3), d2
	cmp.l sections.HunkSlot.Align(a2), d2
	bhs.w sectionAligned
	move.l sections.HunkSlot.Align(a2), d2
sectionAligned
	cmp.l PlaceEntry.Align(a4), d2
	bhs.w placeAligned
	move.l PlaceEntry.Align(a4), d2
placeAligned
	subq.l #1, d2
	move.l RegionSlot.Cursor(a3), d3
	add.l d2, d3
	bcs.w bad
	not.l d2
	and.l d2, d3
	movea.l pkg.Context.Package(a1), a5
	cmp.l pkg.Header.MaxAddress(a5), d3
	bhi.w bad
	cmp.l sections.HunkSlot.Origin(a2), d3
	beq.w sameOrigin
	moveq #1, d7
sameOrigin
	move.l d3, sections.HunkSlot.Origin(a2)
	move.l sections.HunkSlot.Allocation(a2), d2
	moveq #0, d6
	move.w PlaceEntry.Section(a4), d6
	addq.w #1, d6
	lea sections.HUNK_SLOTS(a6), a5
	moveq #7, d5
mappedSizes
	cmp.w sections.HunkSlot.Target(a5), d6
	bne.w nextMapped
	add.l sections.HunkSlot.Allocation(a5), d2
	bcs.w bad
nextMapped
	adda.w #sections.HUNK_SLOT_BYTES, a5
	dbra d5, mappedSizes
	add.l d3, d2
	bcs.w bad
	move.l d2, RegionSlot.Cursor(a3)
	tst.l d2
	beq.w checkEmpty
	subq.l #1, d2
checkEmpty
	cmp.l RegionSlot.End(a3), d2
	bhi.w bad
	adda.w #PLACE_BYTES, a4
	subq.w #1, d4
	bne.w placement
publish
	lea sections.HUNK_SLOTS(a6), a3
	movea.l pkg.Context.SectionBases(a1), a4
	moveq #7, d4
publishSlot
	moveq #0, d0
	move.w sections.HunkSlot.Target(a3), d0
	beq.w concrete
	subq.w #1, d0
	bsr.w sectionAddress
	move.l sections.HunkSlot.Origin(a5), d2
	cmp.l sections.HunkSlot.Origin(a3), d2
	beq.w mapSame
	moveq #1, d7
mapSame
	move.l d2, sections.HunkSlot.Origin(a3)
concrete
	move.l sections.HunkSlot.Origin(a3), (a4)+
	adda.w #sections.HUNK_SLOT_BYTES, a3
	dbra d4, publishSlot
	move.w #1, sections.State.PlaceReady(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d7, d1
	movem.l (sp)+, d2-d7/a0-a6
	.TELEMETRY_SERVICE_LEAVE
	tst.l d0
	rts
	.bend  ; freeze
	.priv
; D0=alignment. D1 scratch; CCR=zero iff nonzero power of two.
alignment	.block
	tst.l d0
	beq.w invalid
	move.l d0, d1
	subq.l #1, d1
	and.l d0, d1
	rts
invalid
	moveq #1, d1
	rts
	.bend  ; alignment
; D0=index,A6=state. A5=region; D0 consumed.
regionAddress	.block
	mulu.w #REGION_BYTES, d0
	lea REGIONS(a6), a5
	adda.l d0, a5
	rts
	.bend  ; regionAddress
; D0=index,A6=state. A5=section; D0 consumed.
sectionAddress	.block
	mulu.w #sections.HUNK_SLOT_BYTES, d0
	lea sections.HUNK_SLOTS(a6), a5
	adda.l d0, a5
	rts
	.bend  ; sectionAddress
	.endsection
	.endmodule
