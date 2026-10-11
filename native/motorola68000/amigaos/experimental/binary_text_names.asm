; Encoding-name normalization at the tokenizer boundary only.
; Rust uses UTF-8 lossy decoding, Unicode trim and ASCII lowercase.
; @opforge-owner: experimental.amigaos.binary_text_names
	.module experimental.amigaos.binary_text_names
	.cpu 68020
	.pub
STATUS_INVALID = 1
STATUS_OVERFLOW = 2
REPLACEMENT_FIRST = $ef
REPLACEMENT_SECOND = $bf
REPLACEMENT_LAST = $bd
	.section code, kind=code
; A0/D0=input bytes, A1/D1=output/capacity, nonoverlapping buffers.
; D0=status,D1=normalized bytes; preserves D2-D7/A0-A6.
; No allocation or source-identifier lookup. Empty normalized names reject.
normalize	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a2
	movea.l a0, a3
	adda.l d0, a3
	movea.l a1, a4
	movea.l a1, a5  ; end of the last non-whitespace scalar
	movea.l a1, a6
	adda.l d1, a6
	moveq #0, d7  ; whether a non-whitespace scalar has been emitted
next
	cmpa.l a3, a2
	bhs.w complete
	bsr.w scalar
	move.l d0, d6  ; invalid UTF-8 needs a replacement scalar
	bsr.w whitespace
	tst.l d7
	bne.w emit
	tst.l d0
	bne.w advance
emit
	move.l d0, d5
	move.l a6, d0
	sub.l a4, d0
	tst.l d6
	bne.w replacement
	cmp.l d2, d0
	blo.w overflow
	movea.l a2, a0
	move.l d2, d3
copy
	move.b (a0)+, d4
	cmpi.b #'A', d4
	blo.w copied
	cmpi.b #'Z', d4
	bhi.w copied
	addi.b #'a'-'A', d4
copied
	move.b d4, (a4)+
	subq.l #1, d3
	bne.w copy
	bra.w emitted
replacement
	cmpi.l #3, d0
	blo.w overflow
	move.b #REPLACEMENT_FIRST, (a4)+
	move.b #REPLACEMENT_SECOND, (a4)+
	move.b #REPLACEMENT_LAST, (a4)+
emitted
	tst.l d5
	bne.w advance
	moveq #1, d7
	movea.l a4, a5
advance
	adda.l d2, a2
	bra.w next
complete
	move.l a5, d1
	sub.l a1, d1
	beq.w invalid
	moveq #0, d0
	bra.w done
invalid
	moveq #STATUS_INVALID, d0
	bra.w failed
overflow
	moveq #STATUS_OVERFLOW, d0
failed
	clr.l d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; normalize
	.priv
; A2/A3=input cursor/end. D1=scalar,D2=consumed raw prefix,D0=invalid.
; Invalid sequences consume exactly the valid prefix preceding a bad byte,
; including truncated prefixes, matching String::from_utf8_lossy.
scalar	.block
	moveq #0, d1
	move.b (a2), d1
	moveq #1, d2
	cmpi.b #$80, d1
	blo.w valid
	move.l d1, d4
	cmpi.b #$c2, d1
	blo.w invalid
	cmpi.b #$df, d1
	bls.w two
	cmpi.b #$ef, d1
	bls.w three
	cmpi.b #$f4, d1
	bhi.w invalid
	moveq #4, d3
	andi.l #7, d1
	bra.w continuation
three
	moveq #3, d3
	andi.l #15, d1
	bra.w continuation
two
	moveq #2, d3
	andi.l #31, d1
continuation
	movea.l a2, a0
	adda.l d2, a0
	cmpa.l a3, a0
	bhs.w invalid
	moveq #0, d0
	move.b (a0), d0
	cmpi.b #$80, d0
	blo.w invalid
	cmpi.b #$bf, d0
	bhi.w invalid
	cmpi.l #1, d2
	bne.w accepted
	cmpi.b #$e0, d4
	bne.w notE0
	cmpi.b #$a0, d0
	blo.w invalid
notE0
	cmpi.b #$ed, d4
	bne.w notEd
	cmpi.b #$a0, d0
	bhs.w invalid
notEd
	cmpi.b #$f0, d4
	bne.w notF0
	cmpi.b #$90, d0
	blo.w invalid
notF0
	cmpi.b #$f4, d4
	bne.w accepted
	cmpi.b #$90, d0
	bhs.w invalid
accepted
	lsl.l #6, d1
	andi.l #63, d0
	or.l d0, d1
	addq.l #1, d2
	cmp.l d3, d2
	blo.w continuation
valid
	moveq #0, d0
	rts
invalid
	move.l #$fffd, d1
	moveq #1, d0
	rts
	.bend  ; scalar
; D1=decoded Unicode scalar. D0=1 for Rust char::is_whitespace.
whitespace	.block
	cmpi.l #9, d1
	blo.w other
	cmpi.l #13, d1
	bls.w yes
	cmpi.l #32, d1
	beq.w yes
	cmpi.l #$85, d1
	beq.w yes
	cmpi.l #$a0, d1
	beq.w yes
	cmpi.l #$1680, d1
	beq.w yes
	cmpi.l #$2000, d1
	blo.w other
	cmpi.l #$200a, d1
	bls.w yes
	cmpi.l #$2028, d1
	beq.w yes
	cmpi.l #$2029, d1
	beq.w yes
	cmpi.l #$202f, d1
	beq.w yes
	cmpi.l #$205f, d1
	beq.w yes
	cmpi.l #$3000, d1
	beq.w yes
other
	moveq #0, d0
	rts
yes
	moveq #1, d0
	rts
	.bend  ; whitespace
	.endsection
	.endmodule
