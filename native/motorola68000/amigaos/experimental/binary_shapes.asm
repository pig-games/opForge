; Conservative structural facts over packed tokens, independent of target semantics.
; @opforge-owner: experimental.amigaos.binary_shapes
	.module experimental.amigaos.binary_shapes
	.cpu 68020
	.use opasm.amigaos.binary_expression as expression
	.section code, kind=code
	.pub
; A0/A1=bounded operand. D0=1 only for a complete parenthesized member root,
; otherwise 0 (unknown/nonmember). Other registers preserved; CCR reflects D0.
; No values, names or package data are consulted.
isMember	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #8, d0
	blo.w no
	cmpi.b #14, (a0)+
	bne.w no
	movea.l a1, a2
	subq.l #6, a2
	cmpi.b #15, (a2)
	bne.w no
	cmpi.b #7, 1(a2)
	bne.w no
	cmpi.b #1, 2(a2)
	bhi.w no
	tst.b 5(a2)
	bne.w no
	; A compiled expression is opaque: literal bytes must never be scanned
	; as punctuation or symbol tokens.
	cmpi.b #expression.COMPILED_TAG, (a0)+
	bne.w no
	cmpa.l a2, a0
	bhs.w no
	moveq #0, d1
	move.b (a0)+, d1
	beq.w no
	adda.l d1, a0
	cmpa.l a2, a0
	bne.w no
	moveq #1, d0
	bra.w done
no
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	rts
	.bend  ; isMember
	.endsection
	.endmodule
