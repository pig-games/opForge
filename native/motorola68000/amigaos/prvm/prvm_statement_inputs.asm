; Shared bounded input and budget checks for prepared statement facets.
; @opforge-owner: prvm.amigaos.statement_inputs
	.module prvm.amigaos.statement_inputs
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.pub
	.section code, kind=code
; A0/D0=readable span. Empty spans may have a null pointer.
; D0=status/CCR; preserves all other registers. Reject address wraparound.
span	.block
	movem.l d1, -(sp)
	tst.l d0
	bmi.s invalid
	beq.s valid
	move.l a0, d1
	beq.s invalid
	add.l d0, d1
	bcs.s invalid
valid
	moveq #abi.OK, d0
	bra.s done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1
	rts
	.bend  ; span

; A0=remaining nonnegative budget cell. D0=status/CCR, others preserved.
tick	.block
	tst.l (a0)
	beq.s exhausted
	subq.l #1, (a0)
	moveq #abi.OK, d0
	rts
exhausted
	moveq #abi.BUDGET_EXCEEDED, d0
	rts
	.bend  ; tick

; A0=budget cell. D0=status/CCR, others preserved.
budget	.block
	movem.l d1, -(sp)
	move.l a0, d1
	btst #0, d1
	bne.s invalid
	moveq #4, d0
	bsr.w span
	bne.s done
	tst.l (a0)
	bmi.s invalid
	moveq #abi.OK, d0
	bra.s done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1
	rts
	.bend  ; budget

; A0/D0=first span, A1/D1=second span. Both spans must be valid beforehand.
; D0=status/CCR, others preserved. Empty spans are disjoint.
separate	.block
	movem.l d2-d3, -(sp)
	tst.l d0
	beq.s valid
	tst.l d1
	beq.s valid
	move.l a0, d2
	add.l d0, d2
	move.l a1, d3
	cmp.l d3, d2
	bls.s valid
	add.l d1, d3
	move.l a0, d2
	cmp.l d2, d3
	bls.s valid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.s done
valid
	moveq #abi.OK, d0
done
	tst.l d0
	movem.l (sp)+, d2-d3
	rts
	.bend  ; separate

; A0/D0=TokenView array/count, A1/D1=literal arena, A2=budget cell.
; D0=status/CCR; other registers preserved. No facet/text interpretation.
tokens	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d6
	bmi.w invalid
	cmpi.l #$03ffffff, d6  ; maximum nonnegative extent for 32-byte rows
	bhi.w invalid
	move.l a0, d2
	btst #0, d2
	bne.w invalid
	lsl.l #5, d0  ; abi.TOKEN_BYTES = 32
	bsr.w span
	bne.w done
	movea.l a0, a3
	movea.l a1, a0
	move.l d1, d0
	bsr.w span
	bne.w done
	move.l d1, d5
loop
	tst.l d6
	beq.w valid
	movea.l a2, a0
	bsr.w tick
	bne.w done
	tst.w abi.TokenView.Reserved(a3)
	bne.w invalid
	cmpi.b #1, abi.TokenView.NumericStatus(a3)
	bhi.w invalid
	move.l abi.TokenView.ColStart(a3), d0
	cmp.l abi.TokenView.ColEnd(a3), d0
	bhi.w invalid
	move.l abi.TokenView.LiteralOffset(a3), d0
	cmp.l d5, d0
	bhi.w invalid
	move.l d5, d1
	sub.l d0, d1
	cmp.l abi.TokenView.LiteralBytes(a3), d1
	blo.w invalid
	adda.w #abi.TOKEN_BYTES, a3
	subq.l #1, d6
	bra.w loop
valid
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; tokens
	.endsection
	.endmodule
