; Shared semantic validation of canonical prepared statement token facets.
; @opforge-owner: prvm.amigaos.statement_facets
	.module prvm.amigaos.statement_facets
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.use prvm.amigaos.statement_text as text
	.pub
	.section code, kind=code
; A0/D0=TokenView array/count,A1/D1=arena/bytes,A2=budget cell.
; D0=status/CCR; others preserved. Mirrors the Rust prepared-facet invariants.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	movea.l a2, a5
	move.l d0, d7
	jsr inputs.tokens
	bne.w done
next
	tst.l d7
	beq.w valid
	moveq #0, d6
	move.b (a3), d6  ; TokenView.Kind (zero offset)
	cmpi.l #abi.K_NUMBER, d6
	beq.w numeric
	tst.b abi.TokenView.NumericStatus(a3)
	bne.w invalid
numeric
	tst.b abi.TokenView.NumericStatus(a3)
	bne.w valueReady
	move.l abi.TokenView.NumericHigh(a3), d0
	or.l abi.TokenView.NumericLow(a3), d0
	bne.w invalid
valueReady
	move.l abi.TokenView.LiteralBytes(a3), d5
	cmpi.l #abi.K_NUMBER, d6
	bhi.w decoded
	tst.l d5
	beq.w invalid
decoded
	move.l abi.TokenView.DecodedBytes(a3), d4
	cmpi.l #abi.K_STRING, d6
	beq.w string
	tst.l d4
	bne.w invalid
	bra.w utf8
string
	cmp.l d5, d4
	bhi.w invalid
	cmpi.l #$2aaaaaaa, d4  ; above this, 3*decoded exceeds any signed arena extent
	bhi.w utf8
	move.l d4, d0
	add.l d0, d0
	add.l d4, d0
	cmp.l d0, d5
	bhi.w invalid
utf8
	movea.l a4, a0
	adda.l abi.TokenView.LiteralOffset(a3), a0
	move.l d5, d0
	movea.l a5, a1
	jsr text.validate
	bne.w done
	adda.w #abi.TOKEN_BYTES, a3
	subq.l #1, d7
	bra.w next
valid
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_TOKEN, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; validate
	.endsection
	.endmodule
