; Bounded package-driven mask plus unary-indirect encoding fragment.
; @opforge-owner: experimental.amigaos.binary_mask_unary
	.module experimental.amigaos.binary_mask_unary
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.section bss, kind=bss
	.priv
Bytes	.res byte, 4
	.endsection
	.section code, kind=code
	.pub

; A0/A1=packed name-list bounds,A2/A3=packed unary-indirect bounds,
; A4=validated 16-byte recipe,A5=validated BSP3 package.
; D0/CCR=status; on success D1=4,A1=owned output bytes. Other regs preserved.
encode	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
	movea.l a5, a6
	movea.l a4, a5
	cmpi.w #1, 12(a5)
	bne.w bad
	tst.w 14(a5)
	bne.w bad
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #7, d0
	bne.w bad
	cmpi.b #19, (a2)
	bne.w bad
	cmpi.b #14, 1(a2)
	bne.w bad
	cmpi.b #15, 6(a2)
	bne.w bad
	movem.l a0-a1, -(sp)
	lea 2(a2), a0
	lea 6(a2), a1
	bsr.w register
	movem.l (sp)+, a0-a1
	bne.w bad
	cmp.w 4(a5), d1
	bne.w bad
	cmpi.w #7, d2
	bhi.w bad
	move.w (a5), d5
	or.w d2, d5
	clr.l d6
nextItem
	bsr.w register
	bne.w bad
	bsr.w bitIndex
	bne.w bad
	move.w d3, d4
	move.w d1, d7
	cmpa.l a1, a0
	beq.w oneBit
	cmpi.b #19, (a0)
	bne.w oneBit
	addq.l #1, a0
	bsr.w register
	bne.w bad
	bsr.w bitIndex
	bne.w bad
	cmp.w d7, d1
	bne.w bad
	cmp.w d4, d3
	blo.w bad
rangeBit
	bset d4, d6
	cmp.w d3, d4
	beq.w itemEnd
	addq.w #1, d4
	bra.w rangeBit
oneBit
	bset d3, d6
itemEnd
	cmpa.l a1, a0
	beq.w reverse
	cmpi.b #22, (a0)+
	bne.w bad
	bra.w nextItem
reverse
	moveq #0, d4
	moveq #15, d3
reverseBit
	lsl.w #1, d4
	lsr.w #1, d6
	bcc.w zeroBit
	addq.w #1, d4
zeroBit
	dbra d3, reverseBit
	move.w d5, Bytes
	move.w d4, Bytes+2
	lea Bytes, a1
	moveq #4, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
	suba.l a1, a1
done
	movem.l (sp)+, d2-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; encode

	.priv
; A0/A1=one bounded numeric name,A6=package. Return D1=class,D2=index,
; A0 advanced by four; D0/CCR=status. Other registers preserved.
register	.block
	movem.l d3-d4/a3, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w invalid
	cmpi.b #1, (a0)
	bhi.w invalid
	tst.b 3(a0)
	bne.w invalid
	moveq #0, d1
	move.w 1(a0), d1
	addq.l #4, a0
	move.l pkg.Header.RegisterRows(a6), d0
	move.l pkg.Header.RegisterCount(a6), d3
	cmpi.l #$ffff, d3
	bhi.w invalid
	move.l d3, d4
	mulu.w #6, d4
	add.l d0, d4
	bcs.w invalid
	cmp.l pkg.Header.Bytes(a6), d4
	bhi.w invalid
	movea.l a6, a3
	adda.l d0, a3
find
	tst.l d3
	beq.w invalid
	cmp.w (a3), d1
	beq.w found
	addq.l #6, a3
	subq.l #1, d3
	bra.w find
found
	moveq #0, d1
	move.w 2(a3), d1
	moveq #0, d2
	move.w 4(a3), d2
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	movem.l (sp)+, d3-d4/a3
	tst.l d0
	rts
	.bend  ; register

; D1=package class,D2=opaque index,A5=recipe. Return D3=mask bit or D0=1.
bitIndex	.block
	cmp.w 6(a5), d1
	beq.w first
	cmp.w 10(a5), d1
	bne.w invalid
	moveq #0, d3
	move.b 9(a5), d3
	bra.w mapped
first
	moveq #0, d3
	move.b 8(a5), d3
mapped
	add.w d2, d3
	cmpi.w #15, d3
	bhi.w invalid
	moveq #0, d0
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; bitIndex
	.endsection
	.endmodule
