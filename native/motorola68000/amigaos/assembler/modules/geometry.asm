; Numeric placement arithmetic for section fragments, without target semantics.
; @opforge-owner: opforge.assembler.modules.geometry
	.module opforge.assembler.modules.geometry
	.cpu 68020
	.pub
OK = 0
INVALID = 1
	.section code, kind=code
; A section cursor is aligned afresh each time retained fragments are placed.
; D1=cursor,D2=nonzero power-of-two alignment,D3=allocation extent.
; Success D1=aligned start,D2=exclusive allocation end. D0/CCR=status.
; D3 and all other registers preserved; D1/D2 unchanged on failure.
place	.block
	movem.l d4-d6, -(sp)
	move.l d1, d4
	move.l d2, d5
	beq.w bad
	subq.l #1, d5
	move.l d5, d6
	and.l d2, d6
	bne.w bad
	add.l d5, d4
	bcs.w bad
	not.l d5
	and.l d5, d4
	move.l d4, d5
	add.l d3, d5
	bcs.w bad
	move.l d4, d1
	move.l d5, d2
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d4-d6
	tst.l d0
	rts
	.bend  ; place
	.endsection
	.endmodule
