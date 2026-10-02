; Packed structural wrappers; spelling and target modes never enter this helper.
; @opforge-owner: experimental.amigaos.binary_operand_wrappers
	.module experimental.amigaos.binary_operand_wrappers
	.cpu 68020
	.use opasm.amigaos.binary_expression as expression
	.pub
OPEN = 14
CLOSE = 15
COMMA = 4
NAME_MAX = 1
	.section code, kind=code
; A0/A1=raw operand, A3/A4=bounded output. Preserve A1-A2/A4-A6,D1-D7.
; A0/A3 advance; D0/CCR=status. Preserve the outer scalar/tuple syntax while
; compiling its first scalar once; output is uncommitted caller scratch.
prepare	.block
	movem.l d1-d7/a1-a2/a4-a6, -(sp)
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #OPEN, (a0)+
	bne.w bad
	cmpa.l a4, a3
	bhs.w bad
	move.b #OPEN, (a3)+
	jsr expression.compile
	bne.w bad
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #CLOSE, (a0)
	beq.w single
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #6, d0
	blo.w bad
	cmpi.b #COMMA, (a0)
	bne.w bad
	cmpi.b #NAME_MAX, 1(a0)
	bhi.w bad
	tst.b 4(a0)
	bne.w bad
	cmpi.b #CLOSE, 5(a0)
	bne.w bad
	moveq #6, d1
	bra.w tail
single
	moveq #1, d1
tail
	move.l a4, d0
	sub.l a3, d0
	cmp.l d1, d0
	blo.w bad
	subq.l #1, d1
copy
	move.b (a0)+, (a3)+
	dbra d1, copy
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a1-a2/a4-a6
	tst.l d0
	rts
	.bend

; A0/A1=prepared complete wrapper. Return exact scalar bounds in A0/A1,
; A6=tuple numeric-name cursor (tuple only). Other registers preserved.
; Structural validation only: ordinary ExprVM owns scalar validity and values.
scalar	.block
	moveq #1, d0
	bra.w bounds
	.bend
tuple	.block
	moveq #6, d0
	bra.w bounds
	.bend
	.priv
bounds	.block
	movem.l d1-d3, -(sp)
	move.l d0, d3
	move.l a1, d1
	sub.l a0, d1
	cmpi.l #5, d1
	blo.w bad
	cmpi.b #OPEN, (a0)+
	bne.w bad
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w bad
	moveq #0, d2
	move.b 1(a0), d2
	beq.w bad
	addq.l #2, d2
	move.l d2, d0
	add.l d3, d0
	addq.l #1, d0
	cmp.l d1, d0
	bne.w bad
	lea 0(a0, d2.l), a6
	cmpi.l #1, d3
	beq.w single
	cmpi.b #COMMA, (a6)+
	bne.w bad
	cmpi.b #NAME_MAX, (a6)
	bhi.w bad
	tst.b 3(a6)
	bne.w bad
	cmpi.b #CLOSE, 4(a6)
	bne.w bad
	lea -1(a6), a1
	bra.w ok
single
	cmpi.b #CLOSE, (a6)
	bne.w bad
	movea.l a6, a1
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3
	tst.l d0
	rts
	.bend
	.endsection
	.endmodule
