; Shared UTF-8 text primitive used once while preparing statement token facets.
; @opforge-owner: prvm.amigaos.statement_text
	.module prvm.amigaos.statement_text
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.pub
	.section code, kind=code
; A0/D0=input bytes,A1=budget cell. D0=status/CCR; others preserved.
; Validate canonical text facets/program names; no decoding or source parsing.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	move.l d0, d7
	jsr inputs.span
	bne.w done
	movea.l a4, a0
	jsr inputs.budget
	bne.w done
loop
	tst.l d7
	beq.w valid
	movea.l a4, a0
	jsr inputs.tick
	bne.w done
	movea.l a3, a0
	move.l d7, d1
	bsr.w next
	tst.l d0
	beq.w invalid
	adda.l d0, a3
	sub.l d0, d7
	bra.w loop
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

; A0/D0=decoded raw bytes,A1/D1=output/capacity,A2=budget cell.
; D0=status/CCR,D1=UTF8-lossy byte count; others preserved.
; Measure/charge first, then copy without failure. Output untouched on error.
; Invalid subsequences follow Rust Utf8Error.error_len, including truncated tails.
lossy	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	movea.l a2, a5
	move.l d0, d7
	move.l d1, d6
	jsr inputs.span
	bne.w failed
	movea.l a4, a0
	move.l d6, d0
	jsr inputs.span
	bne.w failed
	movea.l a3, a0
	move.l d7, d0
	movea.l a4, a1
	move.l d6, d1
	jsr inputs.separate
	bne.w failed
	movea.l a5, a0
	jsr inputs.budget
	bne.w failed
	movea.l a3, a6
	move.l d7, d4
	moveq #0, d5
measure
	tst.l d4
	beq.w measured
	movea.l a5, a0
	jsr inputs.tick
	bne.w failed
	movea.l a6, a0
	move.l d4, d1
	bsr.w next
	move.l d0, d3
	bne.w validLength
	moveq #3, d3  ; UTF-8 U+FFFD
validLength
	move.l d6, d1
	sub.l d5, d1
	cmp.l d3, d1
	blo.w overflow
	add.l d3, d5
	adda.l d2, a6
	sub.l d2, d4
	bra.w measure
measured
	movea.l a3, a6
	move.l d7, d4
copy
	tst.l d4
	beq.w success
	movea.l a6, a0
	move.l d4, d1
	bsr.w next
	tst.l d0
	beq.w replacement
	move.l d0, d3
validByte
	move.b (a6)+, (a4)+
	subq.l #1, d3
	bne.w validByte
	bra.w copied
replacement
	move.b #$ef, (a4)+
	move.b #$bf, (a4)+
	move.b #$bd, (a4)+
	adda.l d2, a6
copied
	sub.l d2, d4
	bra.w copy
success
	move.l d5, d1
	moveq #abi.OK, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
failed
	moveq #0, d1
done
	tst.l d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; lossy
	.priv
; A0=input,D1=positive remaining bytes. D0=valid sequence length or zero;
; D2=bytes consumed (valid length or Utf8Error invalid prefix). Others preserved.
next	.block
	movem.l d3-d6, -(sp)
	moveq #0, d0
	move.b (a0), d0
	moveq #1, d2
	cmpi.l #$80, d0
	blo.w one
	cmpi.l #$c2, d0
	blo.w invalid
	cmpi.l #$df, d0
	bls.w two
	cmpi.l #$ef, d0
	bls.w three
	cmpi.l #$f4, d0
	bls.w four
	bra.w invalid
two
	moveq #2, d3
	move.l #$80, d4
	move.l #$bf, d5
	bra.w second
three
	moveq #3, d3
	move.l #$80, d4
	move.l #$bf, d5
	cmpi.l #$e0, d0
	bne.w notE0
	move.l #$a0, d4
notE0
	cmpi.l #$ed, d0
	bne.w second
	move.l #$9f, d5
	bra.w second
four
	moveq #4, d3
	move.l #$80, d4
	move.l #$bf, d5
	cmpi.l #$f0, d0
	bne.w notF0
	move.l #$90, d4
notF0
	cmpi.l #$f4, d0
	bne.w second
	move.l #$8f, d5
second
	cmp.l d1, d2
	bhs.w invalid
	moveq #0, d0
	move.b 1(a0), d0
	cmp.l d4, d0
	blo.w invalid
	cmp.l d5, d0
	bhi.w invalid
	addq.l #1, d2
continuation
	cmp.l d3, d2
	beq.w valid
	cmp.l d1, d2
	bhs.w invalid
	moveq #0, d0
	move.b 0(a0, d2.l), d0
	cmpi.l #$80, d0
	blo.w invalid
	cmpi.l #$bf, d0
	bhi.w invalid
	addq.l #1, d2
	bra.w continuation
valid
	move.l d3, d0
	bra.w done
one
	moveq #1, d0
	bra.w done
invalid
	moveq #0, d0
done
	tst.l d0
	movem.l (sp)+, d3-d6
	rts
	.bend  ; next
	.endsection
	.endmodule
