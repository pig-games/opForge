; Conservative structural facts over packed tokens, independent of target semantics.
; @opforge-owner: experimental.amigaos.binary_shapes
	.module experimental.amigaos.binary_shapes
	.cpu 68020
	.use opasm.amigaos.binary_expression as expression

NAME_TAG_MAX = 1
OPEN_PAREN = 14
CLOSE_PAREN = 15
PLUS = 18
MINUS = 19
DIVIDE = 22
	.section code, kind=code
	.pub
; A0/A1=bounded operand. D0=1 for one complete compiled scalar wrapper.
; All other registers are preserved. This proves its root shape only; the
; compiler/evaluator own expression validity, and no payload byte is decoded.
isScalar	.block
	movem.l d1-d2, -(sp)
	move.l a1, d1
	sub.l a0, d1
	bcs.w no
	cmpi.l #3, d1
	blo.w no
	cmpi.b #expression.COMPILED_TAG, (a0)
	bne.w no
	moveq #0, d2
	move.b 1(a0), d2
	beq.w no
	addq.l #2, d2
	cmp.l d1, d2
	bne.w no
	moveq #1, d0
	bra.w done
no
	moveq #0, d0
done
	movem.l (sp)+, d1-d2
	tst.l d0
	rts
	.bend  ; isScalar

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

; A0/A1=bounded operand. D0=1 only for a complete nonempty sequence of
; unqualified numeric names joined by '-' or '/'. This proves a raw list root
; cannot be a parenthesized member; register identity and ranges are not
; interpreted here. Other registers are preserved; CCR reflects D0.
isNameSequence	.block
	movem.l d1/a0, -(sp)
	cmpa.l a1, a0
	bhs.w no
nextName
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w no
	cmpi.b #NAME_TAG_MAX, (a0)
	bhi.w no
	tst.b 3(a0)
	bne.w no
	addq.l #4, a0
	cmpa.l a1, a0
	beq.w yes
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #MINUS, d1
	beq.w nextName
	cmpi.b #DIVIDE, d1
	beq.w nextName
no
	moveq #0, d0
	bra.w done
yes
	moveq #1, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; isNameSequence

; A0/A1=bounded operand. D0=1 only for (name), -(name) or (name)+.
; A name token must be unqualified. All other registers are preserved;
; CCR reflects D0. This proves syntax only, without resolving the name.
isWrappedName	.block
	movem.l d1/a0, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #6, d0
	beq.w plain
	cmpi.l #7, d0
	bne.w no
	cmpi.b #OPEN_PAREN, (a0)
	beq.w tail
	cmpi.b #MINUS, (a0)
	bne.w no
	cmpi.b #OPEN_PAREN, 1(a0)
	bne.w no
	cmpi.b #CLOSE_PAREN, 6(a0)
	bne.w no
	addq.l #2, a0
	bra.w name
tail
	cmpi.b #CLOSE_PAREN, 5(a0)
	bne.w no
	cmpi.b #PLUS, 6(a0)
	bne.w no
	addq.l #1, a0
	bra.w name
plain
	cmpi.b #OPEN_PAREN, (a0)
	bne.w no
	cmpi.b #CLOSE_PAREN, 5(a0)
	bne.w no
	addq.l #1, a0
name
	cmpi.b #NAME_TAG_MAX, (a0)
	bhi.w no
	tst.b 3(a0)
	bne.w no
	moveq #1, d0
	bra.w done
no
	moveq #0, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; isWrappedName
	.endsection
	.endmodule
