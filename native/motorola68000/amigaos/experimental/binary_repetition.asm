; Counted preparation-time repetition over packed, compiled records.
; The source text and package dictionary are absent during both assembly passes.
; @opforge-owner: experimental.amigaos.binary_repetition
	.module experimental.amigaos.binary_repetition
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use opasm.amigaos.binary_expression as expression
	.pub
LIMIT = 16
MAX_ITERATIONS = 65536
Slot	.struct
Body	.long ?
Remaining	.long ?
	.endstruct
State	.struct
Depth	.word ?
	.endstruct
SLOTS = State.Depth+2
STATE_BYTES = SLOTS+LIMIT*8
	.section code, kind=code

; A0=state. Reset the loop stack for one complete source sweep.
begin	.block
	clr.w State.Depth(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. A source sweep must not end inside a counted loop.
end	.block
	moveq #0, d0
	tst.w State.Depth(a0)
	beq.w done
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; end

; A0=current record,A1=bounded stream end,A2=assembly context,A3=state.
; D0=0 ordinary record,1 consumed loop control,2 invalid loop. For a consumed
; control A0 is the next record (possibly the first body record again).
; Preserves D1-D7/A1-A6. All stored addresses are transient execution state,
; never part of the binary source representation.
step	.block
	movem.l d1-d7/a1-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	cmpi.w #9, d0
	blo.w ordinary
	movea.l a5, a4
	adda.w d0, a4
	cmpa.l a6, a4
	bhi.w invalid
	cmpi.b #7, 4(a5)
	bne.w ordinary
	cmpi.b #1, 5(a5)
	bhi.w ordinary
	tst.b 8(a5)
	bne.w ordinary
	movea.l package.Context.Package(a2), a1
	move.w 6(a5), d1
	cmp.w package.Header.ForDirective(a1), d1
	beq.w opening
	cmp.w package.Header.EndforDirective(a1), d1
	beq.w closing
	bra.w ordinary
opening
	cmpi.w #12, d0
	blo.w invalid
	move.w State.Depth(a3), d7
	cmpi.w #LIMIT, d7
	bhs.w invalid
	lea 9(a5), a0
	movea.l a4, a1
	jsr expression.evaluate
	bne.w invalid
	tst.l d2
	bne.w invalid
	tst.l package.Context.High(a2)
	bne.w invalid
	cmpi.l #MAX_ITERATIONS, d1
	bhi.w invalid
	tst.l d1
	beq.w skipBody
	mulu.w #8, d7
	lea SLOTS(a3), a1
	move.l a4, Slot.Body(a1, d7.w)
	move.l d1, Slot.Remaining(a1, d7.w)
	addq.w #1, State.Depth(a3)
	movea.l a4, a0
	bra.w consumed
skipBody
	moveq #1, d7
	movea.l a4, a0
skipLine
	cmpa.l a6, a0
	bhs.w invalid
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #9, d0
	blo.w invalid
	movea.l a0, a4
	adda.w d0, a4
	cmpa.l a6, a4
	bhi.w invalid
	cmpi.b #7, 4(a0)
	bne.w skipNext
	cmpi.b #1, 5(a0)
	bhi.w skipNext
	tst.b 8(a0)
	bne.w skipNext
	movea.l package.Context.Package(a2), a1
	move.w 6(a0), d0
	cmp.w package.Header.ForDirective(a1), d0
	bne.w skipClose
	addq.w #1, d7
	bra.w skipNext
skipClose
	cmp.w package.Header.EndforDirective(a1), d0
	bne.w skipNext
	subq.w #1, d7
	beq.w skipped
skipNext
	movea.l a4, a0
	bra.w skipLine
skipped
	movea.l a4, a0
	bra.w consumed
closing
	cmpi.w #9, d0
	bne.w invalid
	move.w State.Depth(a3), d7
	beq.w invalid
	subq.w #1, d7
	mulu.w #8, d7
	lea SLOTS(a3), a1
	move.l Slot.Remaining(a1, d7.w), d1
	subq.l #1, d1
	beq.w exhausted
	move.l d1, Slot.Remaining(a1, d7.w)
	movea.l Slot.Body(a1, d7.w), a0
	bra.w consumed
exhausted
	subq.w #1, State.Depth(a3)
	movea.l a4, a0
consumed
	moveq #1, d0
	bra.w done
ordinary
	; Rust's unscoped .for rejects declarations/labels in its body. A
	; column-one name is a label; explicit ':' and '=' are labels at any indent.
	tst.w State.Depth(a3)
	beq.w ordinaryReady
	cmpi.b #1, 4(a5)
	bhi.w ordinaryReady
	btst #0, 1(a5)
	beq.w invalid
	cmpi.w #9, d0
	blo.w ordinaryReady
	cmpi.b #5, 8(a5)
	beq.w invalid
	cmpi.b #34, 8(a5)
	beq.w invalid
ordinaryReady
	movea.l a5, a0
	moveq #0, d0
	bra.w done
invalid
	moveq #2, d0
done
	movem.l (sp)+, d1-d7/a1-a6
	tst.l d0
	rts
	.bend  ; step
	.endsection
	.endmodule
