; Compact canonical signed-64 ranges. All pairs use low/high longword order.
; Descriptor addresses are ephemeral; compound owners serialize offsets only.
; @opforge-owner: experimental.amigaos.binary_ranges
	.module experimental.amigaos.binary_ranges
	.cpu 68020
	.use exprvm.amigaos.i64_math as math
	.pub
Record	.struct
Kind	.long ?
Reserved	.long ?
StartLow	.long ?
StartHigh	.long ?
EndLow	.long ?
EndHigh	.long ?
StepLow	.long ?
StepHigh	.long ?
.endstruct
BYTES = 32
KIND = 2
OK = 0
MALFORMED = 1
BOUNDS = 3
HAS_STEP = 1
INCLUSIVE = 2
	.section code, kind=code

; A1=external start/end/optional step pairs,A2=32-byte destination,
; D0=canonical flags (bit0 has step,bit1 inclusive),D1=source bytes.
; D0/CCR=status; others preserved. Source/destination must not overlap.
; Produces the exclusive endpoint used by AsmValue::try_range. Rejects zero
; step, inclusive endpoint overflow and direction mismatch before success.
normalize	.block
	movem.l d1-d7/a1-a2, -(sp)
	move.l d0, d7
	andi.l #$fffffffc, d0
	bne.w invalid
	moveq #16, d6
	btst #0, d7
	beq.w sizeReady
	moveq #24, d6
sizeReady
	cmp.l d6, d1
	blo.w invalid
	move.l a1, d0
	beq.w invalid
	btst #0, d0
	bne.w invalid
	add.l d6, d0
	bcs.w invalid
	move.l #KIND, Record.Kind(a2)
	clr.l Record.Reserved(a2)
	move.l (a1)+, Record.StartLow(a2)
	move.l (a1)+, Record.StartHigh(a2)
	move.l (a1)+, Record.EndLow(a2)
	move.l (a1)+, Record.EndHigh(a2)
	btst #0, d7
	beq.w defaultStep
	move.l (a1)+, Record.StepLow(a2)
	move.l (a1)+, Record.StepHigh(a2)
	bra.w stepReady
defaultStep
	move.l Record.StartLow(a2), d1
	move.l Record.StartHigh(a2), d2
	move.l Record.EndLow(a2), d3
	move.l Record.EndHigh(a2), d4
	bsr.w compare
	tst.l d0
	bgt.w descending
	move.l #1, Record.StepLow(a2)
	clr.l Record.StepHigh(a2)
	bra.w stepReady
descending
	move.l #-1, Record.StepLow(a2)
	move.l #-1, Record.StepHigh(a2)
stepReady
	move.l Record.StepLow(a2), d0
	or.l Record.StepHigh(a2), d0
	beq.w invalid
	btst #1, d7
	beq.w validateEnd
	move.l Record.EndLow(a2), d1
	move.l Record.EndHigh(a2), d2
	moveq #0, d3
	tst.l Record.StepHigh(a2)
	bmi.w lowerEnd
	addq.l #1, d1
	addx.l d3, d2
	bvs.w invalid
	bra.w storeEnd
lowerEnd
	subq.l #1, d1
	subx.l d3, d2
	bvs.w invalid
storeEnd
	move.l d1, Record.EndLow(a2)
	move.l d2, Record.EndHigh(a2)
validateEnd
	movea.l a2, a1
	bsr.w validate
	bra.w done
invalid
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a1-a2
	tst.l d0
	rts
	.bend  ; normalize

; A1=complete 32-byte descriptor. D0/CCR=status; others preserved.
; Caller validates memory bounds. Canonical empty ranges are permitted.
validate	.block
	movem.l d1-d4, -(sp)
	cmpi.l #KIND, Record.Kind(a1)
	bne.w invalid
	tst.l Record.Reserved(a1)
	bne.w invalid
	move.l Record.StepLow(a1), d0
	or.l Record.StepHigh(a1), d0
	beq.w invalid
	move.l Record.StartLow(a1), d1
	move.l Record.StartHigh(a1), d2
	move.l Record.EndLow(a1), d3
	move.l Record.EndHigh(a1), d4
	bsr.w compare
	tst.l Record.StepHigh(a1)
	bmi.w descending
	tst.l d0
	bgt.w invalid
	bra.w good
descending
	tst.l d0
	blt.w invalid
good
	moveq #OK, d0
	bra.w done
invalid
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d4
	tst.l d0
	rts
	.bend  ; validate

; A1=validated canonical descriptor. D0/CCR=OK,D1/D2=length low/high,
; saturated at i64::MAX as required by scalar Length. Others preserved.
; Unsigned distance and magnitude retain the full endpoint span and MIN step.
length	.block
	movem.l d3-d7, -(sp)
	move.l Record.EndLow(a1), d3
	move.l Record.EndHigh(a1), d2
	sub.l Record.StartLow(a1), d3
	move.l Record.StartHigh(a1), d0
	subx.l d0, d2
	move.l Record.StepLow(a1), d1
	move.l Record.StepHigh(a1), d0
	bpl.w magnitudeReady
	neg.l d1
	negx.l d0
	neg.l d3
	negx.l d2
magnitudeReady
	move.l d2, d4
	or.l d3, d4
	beq.w empty
	moveq #0, d4
	subq.l #1, d3
	subx.l d4, d2
	bsr.w unsignedDivide
	addq.l #1, d3
	moveq #0, d4
	addx.l d4, d2
	bcs.w saturate
	tst.l d2
	bmi.w saturate
	move.l d3, d1
	bra.w good
saturate
	move.l #$7fffffff, d2
	moveq #-1, d1
	bra.w good
empty
	moveq #0, d1
	moveq #0, d2
good
	moveq #OK, d0
	movem.l (sp)+, d3-d7
	rts
	.bend  ; length

; A1=validated canonical descriptor,D2/D3=index low/high.
; D0/CCR=status,D1/D2=scalar low/high on success; others preserved.
; Nonnegative signed-64 indices only. Checked multiplication and checked add
; match Rust's range_value_get, including intermediate multiplication overflow.
get	.block
	movem.l d3-d7/a2, -(sp)
	move.l d2, d4
	move.l d3, d5
	tst.l d5
	bmi.w outOfBounds
	move.l d4, d0
	or.l d5, d0
	beq.w startValue
	move.l Record.StepHigh(a1), d2
	move.l Record.StepLow(a1), d3
	move.l d5, d0
	move.l d4, d1
	jsr math.multiplyV1
	move.l d2, d7
	movea.l d3, a2
	move.l d5, d0
	move.l d4, d1
	moveq #0, d6
	jsr math.divideModuloV1
	tst.l d0
	bne.w outOfBounds
	cmp.l Record.StepHigh(a1), d2
	bne.w outOfBounds
	cmp.l Record.StepLow(a1), d3
	bne.w outOfBounds
	; Restore the product only after its signed quotient proved exact.
	move.l d7, d2
	move.l a2, d3
	add.l Record.StartLow(a1), d3
	move.l Record.StartHigh(a1), d0
	addx.l d0, d2
	bvs.w outOfBounds
	move.l d3, d1
	bra.w endpoint
startValue
	move.l Record.StartLow(a1), d1
	move.l Record.StartHigh(a1), d2
endpoint
	move.l Record.EndLow(a1), d3
	move.l Record.EndHigh(a1), d4
	bsr.w compare
	tst.l Record.StepHigh(a1)
	bmi.w descending
	tst.l d0
	bge.w outOfBounds
	bra.w good
descending
	tst.l d0
	ble.w outOfBounds
good
	moveq #OK, d0
	bra.w done
outOfBounds
	moveq #BOUNDS, d0
done
	movem.l (sp)+, d3-d7/a2
	tst.l d0
	rts
	.bend  ; get
	.priv

; Compare signed64 D2:D1 against D4:D3. D0=-1/0/1; others preserved.
; CCR reflects D0. Low halves compare unsigned when the signed highs match.
compare	.block
	cmp.l d4, d2
	blt.w less
	bgt.w greater
	cmp.l d3, d1
	blo.w less
	bhi.w greater
	moveq #0, d0
	rts
less
	moveq #-1, d0
	rts
greater
	moveq #1, d0
	rts
	.bend  ; compare

; Unsigned64 D2:D3 divided by nonzero D0:D1 (magnitude <= 2^63).
; D2:D3=quotient; clobbers D4-D5/D7/CCR. No signed narrowing.
unsignedDivide	.block
	moveq #0, d4
	moveq #0, d5
	moveq #63, d7
loop
	add.l d3, d3
	addx.l d2, d2
	addx.l d5, d5
	addx.l d4, d4
	cmp.l d0, d4
	blo.w next
	bhi.w subtract
	cmp.l d1, d5
	blo.w next
subtract
	sub.l d1, d5
	subx.l d0, d4
	addq.l #1, d3
next
	dbf d7, loop
	rts
	.bend  ; unsignedDivide
	.endsection
	.endmodule
