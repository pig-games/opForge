; Bounded contiguous placed sections over numeric records.
; This bounded runtime consumes numeric controls only; no source names or paths.
; @opforge-owner: experimental.amigaos.binary_sections
	.module experimental.amigaos.binary_sections
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.pub
State	.struct
Base	.long ?
End	.long ?
Mode	.word ?
Active	.word ?
Started	.word ?
Placed	.word ?
SecondBase	.long ?
SecondEnd	.long ?
FirstAfter	.long ?
SecondAfter	.long ?
	.endstruct
SCRATCH_BYTES = State.SecondAfter+4
; Mode 3 is the bounded two-concrete-section case. Started/Placed use one bit
; per section; FirstAfter/SecondAfter retain each completed section's PC.
	.section code, kind=code
	.pub

; A0=packed records,D0=byte count,A1=State,D1=package maximum address.
; Find the literal regions before either assembly pass. Numeric source
; controls were validated during preparation. D0/CCR=status; others preserved.
scan	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	clr.l State.Base(a6)
	clr.l State.End(a6)
	clr.w State.Mode(a6)
	clr.w State.Active(a6)
	clr.w State.Started(a6)
	clr.w State.Placed(a6)
	clr.l State.SecondBase(a6)
	clr.l State.SecondEnd(a6)
	clr.l State.FirstAfter(a6)
	clr.l State.SecondAfter(a6)
	move.l a0, d7
	add.l d0, d7
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
	tst.w State.Mode(a6)
	bne.w modeSet
	move.w #1, State.Mode(a6)
modeSet
	cmpi.b #6, 4(a0)
	bne.w secondMode
	move.w #2, State.Mode(a6)  ; concrete source precedes mapped logical content
secondMode
	cmpi.b #7, 4(a0)
	bne.w regionCheck
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
	cmpi.w #1, d3
	bne.w secondStart
	move.l d1, State.Base(a6)
	bra.w scanEnd
secondStart
	move.l d1, State.SecondBase(a6)
scanEnd
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
	cmpi.w #1, d3
	bne.w secondLimit
	move.l d2, State.End(a6)
	bra.w scanned
secondLimit
	move.l d2, State.SecondEnd(a6)
scanned
	or.w d3, State.Started(a6)
next
	adda.l d0, a0
	bra.w record
complete
	tst.w State.Mode(a6)
	beq.w ok
	move.w State.Started(a6), d0
	cmpi.w #3, State.Mode(a6)
	bne.w oneRegion
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

; A0=State. Reset the active control for the next assembly pass.
beginPass	.block
	clr.w State.Active(a0)
	clr.w State.Started(a0)
	clr.w State.Placed(a0)
	clr.l State.FirstAfter(a0)
	clr.l State.SecondAfter(a0)
	moveq #0, d0
	rts
	.bend  ; beginPass

; A0=State,A1=Context,A2=packed control record. D0/CCR=status.
; One section is emitted contiguously. The caller schedules concrete records
; before imported logical records for an explicit map.
control	.block
	movem.l d1-d2/a0-a2, -(sp)
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
	move.w State.Started(a0), d1
	btst #0, d1
	bne.w started
	move.l State.Base(a0), pkg.Context.Pc(a1)
	ori.w #1, State.Started(a0)
started
	move.w d2, State.Active(a0)
	bra.w ok
secondOpen
	cmpi.w #3, State.Mode(a0)
	bne.w bad
	tst.w State.Active(a0)
	bne.w bad
	tst.w State.Placed(a0)
	bne.w bad
	move.w State.Started(a0), d1
	btst #0, d1
	beq.w bad
	move.l State.FirstAfter(a0), d1
	cmp.l State.SecondBase(a0), d1
	bne.w bad  ; only contiguous flat binary output in this slice
	move.l d1, pkg.Context.Pc(a1)
	ori.w #2, State.Started(a0)
	move.w #7, State.Active(a0)
	bra.w ok
close
	tst.w State.Active(a0)
	beq.w bad
	cmpi.w #7, State.Active(a0)
	beq.w closeSecond
	move.l pkg.Context.Pc(a1), State.FirstAfter(a0)
	bra.w closed
closeSecond
	move.l pkg.Context.Pc(a1), State.SecondAfter(a0)
closed
	clr.w State.Active(a0)
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
	move.l State.FirstAfter(a0), d1
	cmp.l State.Base(a0), d1
	blo.w bad
	move.l State.End(a0), d2
	addq.l #1, d2
	bcs.w bad
	cmp.l d2, d1
	bhi.w bad
	ori.w #1, State.Placed(a0)
	bra.w ok
secondPlace
	cmpi.w #3, State.Mode(a0)
	bne.w bad
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
	move.l State.SecondAfter(a0), d1
	cmp.l State.SecondBase(a0), d1
	blo.w bad
	move.l State.SecondEnd(a0), d2
	addq.l #1, d2
	bcs.w bad
	cmp.l d2, d1
	bhi.w bad
	ori.w #2, State.Placed(a0)
	bra.w ok
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; control

; A0=State,A1=Context,D0=emitted byte count. Reject output outside the
; active section or beyond its placed region. D0/CCR=status; others preserved.
checkEmit	.block
	movem.l d1/a0-a1, -(sp)
	tst.w State.Mode(a0)
	beq.w ok
	tst.w State.Active(a0)
	beq.w bad
	move.l pkg.Context.Pc(a1), d1
	cmpi.w #7, State.Active(a0)
	bne.w firstEmit
	cmp.l State.SecondBase(a0), d1
	blo.w bad
	bra.w emitEnd
firstEmit
	cmp.l State.Base(a0), d1
	blo.w bad
emitEnd
	add.l d0, d1
	bcs.w bad
	tst.l d0
	beq.w ok
	subq.l #1, d1
	cmpi.w #7, State.Active(a0)
	bne.w firstEnd
	cmp.l State.SecondEnd(a0), d1
	bhi.w bad
	bra.w ok
firstEnd
	cmp.l State.End(a0), d1
	bhi.w bad
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; checkEmit

; A0=State. The sectioned subset requires a completed placement each pass.
finishPass	.block
	tst.w State.Mode(a0)
	beq.w ok
	tst.w State.Active(a0)
	bne.w bad
	move.w State.Placed(a0), d0
	cmpi.w #3, State.Mode(a0)
	bne.w onePlace
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
	.endsection
	.endmodule
