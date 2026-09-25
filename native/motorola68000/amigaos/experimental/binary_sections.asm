; Bounded contiguous placed sections over numeric records.
; This bounded runtime consumes numeric controls only; no source names or paths.
; @opforge-owner: experimental.amigaos.binary_sections
	.module experimental.amigaos.binary_sections
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.pub
Slot	.struct
Base	.long ?
End	.long ?
After	.long ?
	.endstruct
SLOT_BYTES = Slot.After+4
State	.struct
Mode	.word ?
Active	.word ?
ActiveKind	.word ?
Started	.word ?
Placed	.word ?
ActiveSlot	.word ?
OrderCount	.word ?
OutputSeen	.word ?
HunkInvalid	.word ?
HunkCurrent	.word ?
	.endstruct
SLOTS = State.HunkCurrent+2
FIRST = SLOTS
SECOND = SLOTS+SLOT_BYTES
FIRST_BASE = FIRST+Slot.Base
FIRST_END = FIRST+Slot.End
FIRST_AFTER = FIRST+Slot.After
SECOND_BASE = SECOND+Slot.Base
SECOND_END = SECOND+Slot.End
SECOND_AFTER = SECOND+Slot.After
HunkSlot	.struct
Kind	.word ?
Seen	.word ?
Start	.long ?
Used	.long ?
Size	.long ?
	.endstruct
HUNK_SLOT_BYTES = HunkSlot.Size+4
HUNK_SLOTS = SLOTS+2*SLOT_BYTES
ORDER = HUNK_SLOTS+8*HUNK_SLOT_BYTES
SCRATCH_BYTES = ORDER+8
; Modes 3/4 use two placed slots, without/with two imported maps.
; Started/Placed use one bit per slot; After retains its last completed PC.
	.section code, kind=code
	.pub

; A0=packed records,D0=byte count,A1=State,D1=package maximum address.
; Find the literal regions before either assembly pass. Numeric source
; controls were validated during preparation. D0/CCR=status; others preserved.
scan	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	move.l d0, d5
	clr.w State.Mode(a6)
	clr.w State.Active(a6)
	clr.w State.ActiveKind(a6)
	clr.w State.Started(a6)
	clr.w State.Placed(a6)
	clr.w State.ActiveSlot(a6)
	clr.w State.OrderCount(a6)
	clr.w State.OutputSeen(a6)
	clr.w State.HunkInvalid(a6)
	clr.w State.HunkCurrent(a6)
	lea SLOTS(a6), a5
	move.w #(SCRATCH_BYTES-SLOTS)/2-1, d0
clearSlots
	clr.w (a5)+
	dbra d0, clearSlots
	move.l a0, d7
	add.l d5, d7
	bcs.w bad
	move.l d1, d6
record
	move.l a0, d1
	cmp.l d7, d1
	beq.w complete
	bhi.w bad
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #4, d0
	blo.w bad
	move.l d7, d1
	sub.l a0, d1
	cmp.l d1, d0
	bhi.w bad
	btst #4, 1(a0)
	beq.w next
	cmpi.w #5, d0
	blo.w bad
	move.w d0, -(sp)
	bsr.w scanHunkControl
	bne.w hunkScanBad
	move.w (sp)+, d0
	tst.w State.Mode(a6)
	bne.w modeSet
	move.w #1, State.Mode(a6)
modeSet
	cmpi.b #6, 4(a0)
	bne.w secondMode
	cmpi.w #4, State.Mode(a6)
	beq.w secondMode
	move.w #2, State.Mode(a6)  ; concrete source precedes mapped logical content
secondMode
	cmpi.b #7, 4(a0)
	beq.w twoSlots
	cmpi.b #10, 4(a0)
	beq.w twoMaps
	cmpi.b #11, 4(a0)
	bne.w regionCheck
twoMaps
	tst.w State.OutputSeen(a6)
	bne.w regionCheck
	move.w #4, State.Mode(a6)  ; paired concrete and logical sweeps
	bra.w regionCheck
twoSlots
	tst.w State.OutputSeen(a6)
	bne.w regionCheck
	cmpi.w #4, State.Mode(a6)
	beq.w regionCheck
	move.w #3, State.Mode(a6)  ; two concrete sections and regions
regionCheck
	cmpi.b #4, 4(a0)
	beq.w scanRegion
	cmpi.b #8, 4(a0)
	bne.w next
scanRegion
	cmpi.w #13, d0
	bne.w bad
	moveq #1, d3
	cmpi.b #8, 4(a0)
	bne.w firstRegion
	moveq #2, d3
firstRegion
	move.w State.Started(a6), d2
	and.w d3, d2
	bne.w bad
	moveq #0, d1
	move.b 5(a0), d1
	lsl.l #8, d1
	move.b 6(a0), d1
	lsl.l #8, d1
	move.b 7(a0), d1
	lsl.l #8, d1
	move.b 8(a0), d1
	lea FIRST(a6), a5
	cmpi.w #1, d3
	beq.w slotStart
	lea SECOND(a6), a5
slotStart
	move.l d1, Slot.Base(a5)
	moveq #0, d2
	move.b 9(a0), d2
	lsl.l #8, d2
	move.b 10(a0), d2
	lsl.l #8, d2
	move.b 11(a0), d2
	lsl.l #8, d2
	move.b 12(a0), d2
	cmp.l d1, d2
	blo.w bad
	cmp.l d6, d2
	bhi.w bad
	move.l d2, Slot.End(a5)
scanned
	or.w d3, State.Started(a6)
next
	adda.l d0, a0
	bra.w record
hunkScanBad
	addq.l #2, sp
	bra.w bad
complete
	tst.w State.OutputSeen(a6)
	beq.w flatComplete
	tst.w State.HunkInvalid(a6)
	bne.w bad
	tst.w State.Started(a6)
	bne.w bad
	move.w State.OrderCount(a6), d3
	beq.w bad
	lea ORDER(a6), a4
hunkCheck
	moveq #0, d0
	move.b (a4)+, d0
	bsr.w slotAddress
	tst.w HunkSlot.Seen(a5)
	beq.w bad
	subq.w #1, d3
	bne.w hunkCheck
	move.w #5, State.Mode(a6)
	bra.w ok
flatComplete
	tst.w State.Mode(a6)
	beq.w ok
	move.w State.Started(a6), d0
	cmpi.w #3, State.Mode(a6)
	beq.w twoRegions
	cmpi.w #4, State.Mode(a6)
	bne.w oneRegion
twoRegions
	cmpi.w #3, d0
	bne.w bad
	bra.w scanComplete
oneRegion
	btst #0, d0
	beq.w bad
scanComplete
	clr.w State.Started(a6)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; scan

; Inspect numeric section/output controls without changing flat scan behavior.
; A0=record,D0=record bytes,A6=State. All but D0/CCR preserved.
scanHunkControl	.block
	movem.l d1-d4/a0-a5, -(sp)
	moveq #0, d1
	move.b 4(a0), d1
	cmpi.w #20, d1
	beq.w output
	cmpi.w #2, d1
	beq.w first
	cmpi.w #7, d1
	beq.w second
	cmpi.w #12, d1
	beq.w extra
	cmpi.w #1, d1
	beq.w incompatible
	cmpi.w #6, d1
	beq.w incompatible
	cmpi.w #10, d1
	beq.w incompatible
	cmpi.w #11, d1
	beq.w incompatible
	cmpi.w #5, d1
	beq.w incompatible
	cmpi.w #9, d1
	beq.w incompatible
	bra.w ok
first
	moveq #0, d2
	bra.w section
second
	moveq #1, d2
	bra.w section
extra
	cmpi.w #7, d0
	bne.w bad
	moveq #0, d2
	move.b 6(a0), d2
	bra.w section
section
	cmpi.w #6, d0
	blo.w bad
	cmpi.w #8, d2
	bhs.w bad
	moveq #0, d3
	move.b 5(a0), d3
	cmpi.w #1, d3
	blo.w bad
	cmpi.w #3, d3
	bhi.w bad
	move.l d2, d0
	bsr.w slotAddress
	tst.w HunkSlot.Seen(a5)
	beq.w firstKind
	cmp.w HunkSlot.Kind(a5), d3
	bne.w bad
	bra.w ok
firstKind
	move.w d3, HunkSlot.Kind(a5)
	move.w #1, HunkSlot.Seen(a5)
	bra.w ok
output
	tst.w State.OutputSeen(a6)
	bne.w bad
	cmpi.w #6, d0
	blo.w bad
	moveq #0, d2
	move.b 5(a0), d2
	beq.w bad
	cmpi.w #8, d2
	bhi.w bad
	move.l d2, d3
	addq.l #6, d3
	cmp.l d0, d3
	bne.w bad
	move.w d2, State.OrderCount(a6)
	move.w #1, State.OutputSeen(a6)
	lea ORDER(a6), a4
	lea 6(a0), a1
	moveq #0, d4
copyOrder
	moveq #0, d3
	move.b (a1)+, d3
	cmpi.w #8, d3
	bhs.w bad
	btst d3, d4
	bne.w bad
	bset d3, d4
	move.b d3, (a4)+
	subq.w #1, d2
	bne.w copyOrder
	bra.w ok
incompatible
	move.w #1, State.HunkInvalid(a6)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a5
	tst.l d0
	rts
	.bend  ; scanHunkControl

; D0=slot 0..7,A6=State. A5=slot; caller validates index.
slotAddress	.block
	mulu.w #HUNK_SLOT_BYTES, d0
	lea HUNK_SLOTS(a6), a5
	adda.l d0, a5
	rts
	.bend  ; slotAddress

; A0=State. Reset the active control for the next assembly pass.
beginPass	.block
	movem.l d1/a1, -(sp)
	clr.w State.Active(a0)
	clr.w State.ActiveKind(a0)
	clr.w State.Started(a0)
	clr.w State.Placed(a0)
	clr.w State.ActiveSlot(a0)
	clr.l FIRST_AFTER(a0)
	clr.l SECOND_AFTER(a0)
	moveq #0, d0
	lea HUNK_SLOTS(a0), a1
	moveq #7, d1
clearHunkPass
	; Keep the section kind and declaration bit discovered by scan.
	clr.l HunkSlot.Start(a1)
	clr.l HunkSlot.Used(a1)
	clr.l HunkSlot.Size(a1)
	adda.w #HUNK_SLOT_BYTES, a1
	dbra d1, clearHunkPass
	moveq #0, d0
	movem.l (sp)+, d1/a1
	rts
	.bend  ; beginPass

; A0=State,A1=Context,A2=packed control record. D0/CCR=status.
; One section is emitted contiguously. The caller schedules concrete records
; before imported logical records for an explicit map.
control	.block
	movem.l d1-d3/a0-a2/a5-a6, -(sp)
	cmpi.w #5, State.Mode(a0)
	beq.w hunkControl
	moveq #0, d2
	move.b 4(a2), d2
	cmpi.w #1, d2
	beq.w open
	cmpi.w #2, d2
	beq.w open
	cmpi.w #6, d2
	beq.w open
	cmpi.w #7, d2
	beq.w secondOpen
	cmpi.w #10, d2
	beq.w secondOpen
	cmpi.w #11, d2
	beq.w secondOpen
	cmpi.w #3, d2
	beq.w close
	cmpi.w #4, d2
	beq.w region
	cmpi.w #5, d2
	beq.w place
	cmpi.w #8, d2
	beq.w region
	cmpi.w #9, d2
	beq.w secondPlace
	bra.w bad
open
	tst.w State.Active(a0)
	bne.w bad
	tst.w State.Placed(a0)
	bne.w bad
	bsr.w sectionKind
	bne.w bad
	move.w State.Started(a0), d1
	btst #0, d1
	bne.w started
	move.l FIRST_BASE(a0), pkg.Context.Pc(a1)
	ori.w #1, State.Started(a0)
started
	clr.w State.ActiveSlot(a0)
	move.w d2, State.Active(a0)
	bra.w ok
secondOpen
	cmpi.w #3, State.Mode(a0)
	beq.w secondMode
	cmpi.w #4, State.Mode(a0)
	bne.w bad
secondMode
	tst.w State.Active(a0)
	bne.w bad
	tst.w State.Placed(a0)
	bne.w bad
	bsr.w sectionKind
	bne.w bad
	move.w State.Started(a0), d1
	btst #0, d1
	beq.w bad
	btst #1, d1
	bne.w resumedSecond
	move.l FIRST_AFTER(a0), d1
	cmp.l SECOND_BASE(a0), d1
	bne.w bad  ; only contiguous flat binary output in this slice
	move.l d1, pkg.Context.Pc(a1)
	ori.w #2, State.Started(a0)
	bra.w secondStarted
resumedSecond
	move.l SECOND_AFTER(a0), d1
	cmp.l pkg.Context.Pc(a1), d1
	bne.w bad
secondStarted
	move.w #1, State.ActiveSlot(a0)
	move.w d2, State.Active(a0)
	bra.w ok
close
	tst.w State.Active(a0)
	beq.w bad
	tst.w State.ActiveSlot(a0)
	beq.w closeFirst
	move.l pkg.Context.Pc(a1), SECOND_AFTER(a0)
	bra.w closed
closeFirst
	move.l pkg.Context.Pc(a1), FIRST_AFTER(a0)
closed
	clr.w State.Active(a0)
	clr.w State.ActiveKind(a0)
	bra.w ok
region
	cmpi.b #12, (a2)
	bne.w bad
	bra.w ok
place
	tst.w State.Active(a0)
	bne.w bad
	move.w State.Started(a0), d1
	btst #0, d1
	beq.w bad
	move.w State.Placed(a0), d1
	btst #0, d1
	bne.w bad
	move.l FIRST_AFTER(a0), d1
	cmp.l FIRST_BASE(a0), d1
	blo.w bad
	move.l FIRST_END(a0), d2
	addq.l #1, d2
	bcs.w bad
	cmp.l d2, d1
	bhi.w bad
	ori.w #1, State.Placed(a0)
	bra.w ok
secondPlace
	cmpi.w #3, State.Mode(a0)
	beq.w twoPlace
	cmpi.w #4, State.Mode(a0)
	bne.w bad
twoPlace
	tst.w State.Active(a0)
	bne.w bad
	move.w State.Started(a0), d1
	btst #1, d1
	beq.w bad
	move.w State.Placed(a0), d1
	btst #0, d1
	beq.w bad
	btst #1, d1
	bne.w bad
	move.l SECOND_AFTER(a0), d1
	cmp.l SECOND_BASE(a0), d1
	blo.w bad
	move.l SECOND_END(a0), d2
	addq.l #1, d2
	bcs.w bad
	cmp.l d2, d1
	bhi.w bad
	ori.w #2, State.Placed(a0)
	bra.w ok
hunkControl
	moveq #0, d2
	move.b 4(a2), d2
	cmpi.w #20, d2
	beq.w ok
	cmpi.w #3, d2
	beq.w hunkClose
	moveq #0, d3
	cmpi.w #2, d2
	beq.w hunkOpen
	moveq #1, d3
	cmpi.w #7, d2
	beq.w hunkOpen
	cmpi.w #12, d2
	bne.w bad
	move.b 6(a2), d3
hunkOpen
	tst.w State.Active(a0)
	bne.w bad
	cmp.w State.HunkCurrent(a0), d3
	bne.w bad
	movea.l a0, a6
	move.l d3, d0
	bsr.w slotAddress
	moveq #0, d1
	move.b 5(a2), d1
	cmp.w HunkSlot.Kind(a5), d1
	bne.w bad
	move.w d1, State.ActiveKind(a0)
	move.w #1, State.Active(a0)
	bra.w ok
hunkClose
	tst.w State.Active(a0)
	beq.w bad
	clr.w State.Active(a0)
	clr.w State.ActiveKind(a0)
	bra.w ok
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2/a5-a6
	tst.l d0
	rts
	.bend  ; control

; The Hunk path has section-local PCs and no placement/region directives.
; beginHunkSlot and endHunkSlot bracket one ordered sweep of the packed records.
beginHunkSlot	.block
	movem.l d1/a0-a1/a5-a6, -(sp)
	movea.l a0, a6
	cmpi.w #5, State.Mode(a6)
	bne.w hunkBad
	cmpi.l #8, d0
	bhs.w hunkBad
	move.w d0, State.HunkCurrent(a6)
	bsr.w slotAddress
	tst.w HunkSlot.Seen(a5)
	beq.w hunkBad
	move.l d1, HunkSlot.Start(a5)
	move.l HunkSlot.Size(a5), pkg.Context.Pc(a1)
	moveq #0, d0
	bra.w hunkDone
hunkBad
	moveq #1, d0
hunkDone
	movem.l (sp)+, d1/a0-a1/a5-a6
	tst.l d0
	rts
	.bend  ; beginHunkSlot

endHunkSlot	.block
	movem.l d1/a0-a1/a5-a6, -(sp)
	movea.l a0, a6
	tst.w State.Active(a6)
	bne.w hunkBad
	cmp.w State.HunkCurrent(a6), d0
	bne.w hunkBad
	bsr.w slotAddress
	move.l pkg.Context.Pc(a1), HunkSlot.Size(a5)
	move.l d1, d0
	sub.l HunkSlot.Start(a5), d0
	bcs.w hunkBad
	move.l d0, HunkSlot.Used(a5)
	moveq #0, d0
	bra.w hunkDone
hunkBad
	moveq #1, d0
hunkDone
	movem.l (sp)+, d1/a0-a1/a5-a6
	tst.l d0
	rts
	.bend  ; endHunkSlot

; A0=State,A1=Context,D0=reservation bytes. BSS has no initialized payload.
reserve	.block
	movem.l d1-d2/a0-a2, -(sp)
	cmpi.w #5, State.Mode(a0)
	bne.w bad
	tst.w State.Active(a0)
	beq.w bad
	cmpi.w #3, State.ActiveKind(a0)
	bne.w bad
	move.l pkg.Context.Pc(a1), d1
	add.l d0, d1
	bcs.w bad
	movea.l pkg.Context.Package(a1), a2
	tst.l d0
	beq.w checked
	move.l d1, d2
	subq.l #1, d2
	cmp.l pkg.Header.MaxAddress(a2), d2
	bhi.w bad
checked
	move.l d1, pkg.Context.Pc(a1)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; reserve

; A0=State,A1=Context,D0=emitted byte count. Reject output outside the
; active section or beyond its placed region. D0/CCR=status; others preserved.
checkEmit	.block
	movem.l d1/a0-a2, -(sp)
	cmpi.w #5, State.Mode(a0)
	beq.w hunkEmit
	tst.w State.Mode(a0)
	beq.w ok
	tst.w State.Active(a0)
	beq.w bad
	cmpi.w #3, State.ActiveKind(a0)
	bne.w permittedKind
	tst.l d0
	bne.w bad  ; BSS reservations need a section-aware output path
permittedKind
	move.l pkg.Context.Pc(a1), d1
	tst.w State.ActiveSlot(a0)
	bne.w secondEmit
	cmp.l FIRST_BASE(a0), d1
	blo.w bad
	bra.w emitEnd
secondEmit
	cmp.l SECOND_BASE(a0), d1
	blo.w bad
emitEnd
	add.l d0, d1
	bcs.w bad
	tst.l d0
	beq.w ok
	subq.l #1, d1
	tst.w State.ActiveSlot(a0)
	bne.w secondEnd
	cmp.l FIRST_END(a0), d1
	bhi.w bad
	bra.w ok
hunkEmit
	tst.w State.Active(a0)
	beq.w bad
	cmpi.w #3, State.ActiveKind(a0)
	beq.w bad
	move.l pkg.Context.Pc(a1), d1
	add.l d0, d1
	bcs.w bad
	tst.l d0
	beq.w ok
	subq.l #1, d1
	movea.l pkg.Context.Package(a1), a2
	cmp.l pkg.Header.MaxAddress(a2), d1
	bhi.w bad
	bra.w ok
secondEnd
	cmp.l SECOND_END(a0), d1
	bhi.w bad
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; checkEmit

; A0=State. The sectioned subset requires a completed placement each pass.
finishPass	.block
	tst.w State.Mode(a0)
	beq.w ok
	tst.w State.Active(a0)
	bne.w bad
	cmpi.w #5, State.Mode(a0)
	beq.w ok
	move.w State.Placed(a0), d0
	cmpi.w #3, State.Mode(a0)
	beq.w twoPlaces
	cmpi.w #4, State.Mode(a0)
	bne.w onePlace
twoPlaces
	andi.w #3, d0
	cmpi.w #3, d0
	bne.w bad
	bra.w ok
onePlace
	btst #0, d0
	beq.w bad
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; finishPass

	.priv
; A0=state,A2=numeric section control. Retain kind across active emission.
; D0/CCR=status; other registers preserved.
sectionKind	.block
	cmpi.b #5, (a2)
	bne.w invalidKind
	moveq #0, d0
	move.b 5(a2), d0
	beq.w invalidKind
	cmpi.w #3, d0
	bhi.w invalidKind
	move.w d0, State.ActiveKind(a0)
	moveq #0, d0
	rts
invalidKind
	moveq #1, d0
	rts
	.bend  ; sectionKind
	.endsection
	.endmodule
