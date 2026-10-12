; Prepare nonnegative numeric facets from the package-selected digit alphabet.
; @opforge-owner: prvm.amigaos.statement_decimal
	.module prvm.amigaos.statement_decimal
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.pub
	.section code, kind=code
; A0/D0=digit bytes/count,A1=validated InvocationPolicy,A2=budget cell.
; D0=status/CCR,D1/D2=signed-i64 high/low,D3=valid (0 on numeric overflow).
; Other registers preserved. No signs, radix prefixes, or source text parsing.
; The caller has validated the readable span and current policy. Every byte is
; still checked against its digit alphabet; overflow retains lexical validity.
prepare	.block
	movem.l d4-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	move.l d0, d7
	moveq #0, d1
	moveq #0, d2
	moveq #1, d3
	tst.l d7
	ble.w invalid
	movea.l a2, a0
	jsr inputs.budget
	bne.w failed
next
	movea.l a2, a0
	jsr inputs.tick
	bne.w failed
	moveq #0, d4
	move.b (a3)+, d4
	cmp.b abi.InvocationPolicy.DigitFirst(a4), d4
	blo.w invalid
	cmp.b abi.InvocationPolicy.DigitLast(a4), d4
	bhi.w invalid
	tst.l d3
	beq.w advance
	moveq #0, d5
	move.b abi.InvocationPolicy.DigitFirst(a4), d5
	sub.l d5, d4
	movea.l d4, a5  ; save this digit while bounded multiplication uses D4-D6
	moveq #0, d6
	move.b abi.InvocationPolicy.DigitLast(a4), d6
	sub.l d5, d6
	addq.l #1, d6  ; radix = last-first+1, including a one-digit alphabet
	move.l d1, d4
	move.l d2, d5
	moveq #0, d1
	moveq #0, d2
multiply
	btst #0, d6
	beq.w shift
	add.l d5, d2
	addx.l d4, d1
	bcs.w overflow
	tst.l d1
	bmi.w overflow
shift
	lsr.l #1, d6
	beq.w digit
	add.l d5, d5
	addx.l d4, d4
	bcs.w overflow
	tst.l d4
	bmi.w overflow
	bra.w multiply
digit
	move.l a5, d0
	add.l d0, d2
	moveq #0, d0
	addx.l d0, d1
	bcs.w overflow
	tst.l d1
	bpl.w advance
overflow
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
advance
	subq.l #1, d7
	bne.w next
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_TOKEN, d0
failed
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
done
	tst.l d0
	movem.l (sp)+, d4-d7/a0-a6
	rts
	.bend  ; prepare
	.endsection
	.endmodule
