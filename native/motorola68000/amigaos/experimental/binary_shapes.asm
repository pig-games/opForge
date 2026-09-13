; Conservative structural facts over packed tokens, independent of target semantics.
; @opforge-owner: experimental.amigaos.binary_shapes
	.module experimental.amigaos.binary_shapes
	.cpu 68020
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
	moveq #1, d2
	moveq #0, d3
scan
	cmpa.l a2, a0
	beq.w complete
	bhi.w no
	moveq #0, d0
	move.b (a0)+, d0
	addq.l #1, d3
	cmpi.b #14, d0
	bne.w close
	addq.l #1, d2
close
	cmpi.b #15, d0
	bne.w payload
	subq.l #1, d2
	beq.w no  ; a complete group before the suffix is not the root group
payload
	cmpi.b #2, d0
	bhi.w scan
	moveq #3, d1
	cmpi.b #2, d0
	blo.w advance
	moveq #4, d1
advance
	adda.l d1, a0
	bra.w scan
complete
	tst.l d3
	beq.w no
	cmpi.l #1, d2
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
