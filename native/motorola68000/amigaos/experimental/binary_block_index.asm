; Index named block spans in prepared binary records without source text.
; @opforge-owner: experimental.amigaos.binary_block_index
	.module experimental.amigaos.binary_block_index
	.cpu 68020
	.use experimental.amigaos.binary_source as source
	.pub
LIMIT = 512
Span	.struct
Start	.long ?
End	.long ?
Entry	.word ?
Parent	.word ?
	.endstruct
SPAN_BYTES = Span.Parent+2
STACK = LIMIT*SPAN_BYTES
SCRATCH_BYTES = STACK+LIMIT*2
	.section code, kind=code

; A0=prepared records, D0=bytes, A1=caller-owned SCRATCH_BYTES scratch.
; Return D0/CCR=status, D1=span count. All other registers preserved.
; Start/End are offsets from A0; Parent is the enclosing span index+1.
; This indexes every named block, including nested blocks, and fails closed
; on malformed or unbalanced markers. It does not choose reachable blocks.
index	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a0, a3
	movea.l a1, a4
	move.l a0, d2
	add.l d0, d2
	bcs.w bad
	movea.l d2, a2
	moveq #0, d1
	moveq #0, d2
line
	cmpa.l a2, a3
	beq.w complete
	bhi.w bad
	moveq #0, d3
	move.b (a3), d3
	addq.w #1, d3
	cmpi.w #4, d3
	blo.w bad
	move.l a2, d4
	sub.l a3, d4
	cmp.l d4, d3
	bhi.w bad
	moveq #0, d4
	move.b 1(a3), d4
	move.l d4, d5
	andi.w #$f8, d5
	bne.w bad
	btst #1, d4
	beq.w closing
	btst #2, d4
	bne.w bad
	cmpi.w #9, d3
	bne.w bad
	cmpi.b #1, 4(a3)
	bhi.w bad
	cmpi.b #5, 8(a3)
	bne.w bad
	cmpi.w #LIMIT, d1
	bhs.w bad
	move.l d1, d5
	mulu.w #SPAN_BYTES, d5
	lea 0(a4, d5.l), a5
	move.l a3, d5
	sub.l a6, d5
	move.l d5, Span.Start(a5)
	clr.l Span.End(a5)
	moveq #0, d5
	move.b 5(a3), d5
	lsl.w #8, d5
	move.b 6(a3), d5
	move.w d5, Span.Entry(a5)
	clr.w Span.Parent(a5)
	tst.w d2
	beq.w push
	move.w d2, d5
	subq.w #1, d5
	add.w d5, d5
	lea STACK(a4), a0
	moveq #0, d6
	move.w 0(a0, d5.w), d6
	addq.w #1, d6
	move.w d6, Span.Parent(a5)
push
	move.w d2, d5
	add.w d5, d5
	lea STACK(a4), a0
	move.w d1, 0(a0, d5.w)
	addq.w #1, d2
	addq.w #1, d1
	bra.w next
closing
	btst #2, d4
	beq.w next
	cmpi.w #4, d3
	bne.w bad
	tst.w d2
	beq.w bad
	subq.w #1, d2
	move.w d2, d5
	add.w d5, d5
	lea STACK(a4), a0
	moveq #0, d6
	move.w 0(a0, d5.w), d6
	move.l d6, d5
	mulu.w #SPAN_BYTES, d5
	lea 0(a4, d5.l), a5
	move.l a3, d5
	sub.l a6, d5
	add.l d3, d5
	move.l d5, Span.End(a5)
next
	adda.w d3, a3
	bra.w line
complete
	tst.w d2
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; index
	.endsection
	.endmodule
