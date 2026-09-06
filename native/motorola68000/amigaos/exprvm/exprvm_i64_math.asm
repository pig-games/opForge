; Bounded signed 64-bit arithmetic helpers for the native expression VM.

	.module exprvm.amigaos.i64_math
	.cpu 68020
	.pub

	.section code, kind=code
	.pub

; Return the wrapping low 64 bits of a signed/unsigned pair multiplication.
; Inputs: D2:D3 = left, D0:D1 = right (high:low).
; Outputs: D0.L = 0; D2:D3 = wrapping product.
; Clobbers: D0/D2-D3/CCR.
; CCR: reflects D0.L on return.
multiplyV1	.block
	.priv
	movem.l d1/d4-d5, -(sp)
	bsr.w multiplyCore
	movem.l (sp)+, d1/d4-d5
	moveq #0, d0
	rts
	.bend  ; multiplyV1
	.pub

; Return a wrapping signed 64-bit base raised to an unsigned 32-bit exponent.
; Inputs: D2:D3 = base, D0:D1 = exponent (high:low).
; Outputs: D0.L = 0 success/1 invalid exponent; D2:D3 = result on success.
; Clobbers: D0/D2-D3/CCR.
; CCR: reflects D0.L on return.
powerV1	.block
	.priv
	movem.l d1/d4-d7/a0-a1, -(sp)
	tst.l d0
	bne.s invalid
	movea.l d2, a0
	movea.l d3, a1
	move.l d1, d6
	moveq #0, d2
	moveq #1, d3
loop
	tst.l d6
	beq.s success
	btst #0, d6
	beq.s square
	move.l a0, d0
	move.l a1, d1
	bsr.w multiplyCore
square
	lsr.l #1, d6
	beq.s success
	movem.l d2-d3, -(sp)
	move.l a0, d2
	move.l a1, d3
	move.l a0, d0
	move.l a1, d1
	bsr.w multiplyCore
	movea.l d2, a0
	movea.l d3, a1
	movem.l (sp)+, d2-d3
	bra.s loop
success
	moveq #0, d0
	bra.s return
invalid
	moveq #1, d0
return
	movem.l (sp)+, d1/d4-d7/a0-a1
	rts
	.bend  ; powerV1
	.pub

; Divide or modulo signed 64-bit pairs. Division truncates toward zero and the
; remainder retains the left operand's sign.
; Inputs: D2:D3 = left, D0:D1 = right; D6.L = 0 divide/1 modulo.
; Outputs: D0.L = 0 success/1 invalid; D2:D3 = quotient or remainder.
; Clobbers: D0/D2-D3/CCR.
; CCR: reflects D0.L on return.
divideModuloV1	.block
	.priv
	movem.l d1/d4-d7/a0-a1, -(sp)
	cmpi.l #1, d6
	bhi.w invalid
	move.l d0, d4
	or.l d1, d4
	beq.w invalid
	cmpi.l #$80000000, d2
	bne.s signs
	tst.l d3
	bne.s signs
	cmpi.l #$ffffffff, d0
	bne.s signs
	cmpi.l #$ffffffff, d1
	beq.w invalid
signs
	moveq #0, d4
	tst.l d2
	bpl.s leftMagnitude
	moveq #1, d4
	neg.l d3
	negx.l d2
leftMagnitude
	moveq #0, d5
	tst.l d0
	bpl.s rightMagnitude
	moveq #1, d5
	neg.l d1
	negx.l d0
rightMagnitude
	movea.l d4, a0
	eor.l d4, d5
	movea.l d5, a1
	moveq #0, d4
	moveq #0, d5
	moveq #63, d7
loop
	add.l d3, d3
	addx.l d2, d2
	addx.l d5, d5
	addx.l d4, d4
	cmp.l d0, d4
	blo.s next
	bhi.s subtract
	cmp.l d1, d5
	blo.s next
subtract
	sub.l d1, d5
	subx.l d0, d4
	addq.l #1, d3
next
	dbf d7, loop
	tst.l d6
	beq.s quotientSign
	move.l d4, d2
	move.l d5, d3
	move.l a0, d0
	beq.s success
	bra.s negateResult
quotientSign
	move.l a1, d0
	beq.s success
negateResult
	neg.l d3
	negx.l d2
success
	moveq #0, d0
	bra.s return
invalid
	moveq #1, d0
return
	movem.l (sp)+, d1/d4-d7/a0-a1
	rts
	.bend  ; divideModuloV1
	.pub

; Inputs and outputs match multiplyV1. D4-D5 are scratch.
	.priv
multiplyCore	.block
	move.l d2, d4
	mulu.l d1, d4
	move.l d0, d5
	mulu.l d3, d5
	add.l d5, d4
	move.l d3, d2
	swap d2
	mulu.w d1, d2
	move.l d1, d5
	swap d5
	mulu.w d3, d5
	move.l d3, d0
	swap d0
	mulu.w d1, d3
	swap d1
	mulu.w d1, d0
	move.l d5, d1
	swap d1
	andi.l #$ffff, d1
	add.l d1, d0
	swap d5
	clr.w d5
	add.l d5, d3
	bcc.s firstNoCarry
	addq.l #1, d0
firstNoCarry
	move.l d2, d1
	swap d1
	andi.l #$ffff, d1
	add.l d1, d0
	swap d2
	clr.w d2
	add.l d2, d3
	bcc.s secondNoCarry
	addq.l #1, d0
secondNoCarry
	add.l d4, d0
	move.l d0, d2
	rts
	.bend  ; multiplyCore
	.pub

	.endsection
	.endmodule
