; Bounded tuple views over prepared numeric tokens; no target semantics.
; @opforge-owner: experimental.amigaos.binary_tuples
	.module experimental.amigaos.binary_tuples
	.cpu 68020
	.use opasm.amigaos.binary_expression as expression
	.use experimental.amigaos.binary_products as products
	.priv
NAME_MAX = 1
COMMA = 4
OPEN = 14
CLOSE = 15
	.section code, kind=code
	.pub

; A0/A1=complete prepared operand, D0=expected arity (2/3, or 0 for either), D1=item index.
; D0/CCR=status, A0/A1=selected item, D3=actual arity on success.
; Other registers preserved. Scalar payloads are opaque length-delimited leaves.
; Accept (item,item) and scalar(item[,item]) without changing their arity.
select	.block
	moveq #0, d3
	bra.w view
	.bend  ; select

; A0/A1=complete numeric call, D1=argument ordinal. Select a bounded leaf
; without interpreting the callee name. D0/CCR=status, A0/A1=leaf,D3=arity.
; Other registers preserved. Canonical call projections require an existing
; argument, not a particular callee spelling or exact argument count.
callArgument	.block
	moveq #1, d3
	bra.w view
	.bend  ; callArgument
	.priv
view	.block
	movem.l d1-d2/d4-d5/a2-a5, -(sp)
	tst.l d3
	beq.w tuple
	moveq #-1, d2
	bra.w expectedReady
tuple
	move.l d0, d2
	beq.w expectedReady
	cmpi.l #2, d2
	blo.w bad
	cmpi.l #3, d2
	bhi.w bad
expectedReady
	movea.l a0, a2
	movea.l a1, a3
	suba.l a4, a4
	moveq #0, d4
	cmpa.l a3, a2
	bhs.w bad
	tst.l d2
	bmi.w callHead
	cmpi.b #OPEN, (a2)
	beq.w opening
	; Displacement-style syntax retains its scalar before the opening token.
	cmpi.b #expression.COMPILED_TAG, (a2)
	bne.w bad
	bsr.w leaf
	bne.w bad
	bra.w opening
callHead
	cmpi.b #7, (a2)
	bne.w callName
	addq.l #1, a2
callName
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #5, d0
	blo.w bad
	cmpi.b #NAME_MAX, (a2)
	bhi.w bad
	tst.b 3(a2)
	bne.w bad
	addq.l #4, a2
opening
	cmpa.l a3, a2
	bhs.w bad
	cmpi.b #OPEN, (a2)+
	bne.w bad
next
	bsr.w leaf
	bne.w bad
	cmpa.l a3, a2
	bhs.w bad
	move.b (a2)+, d0
	cmpi.b #CLOSE, d0
	beq.w finish
	cmpi.b #COMMA, d0
	bne.w bad
	tst.l d2
	bmi.w next
	cmpi.l #3, d4
	bhs.w bad
	bra.w next
finish
	cmpa.l a3, a2
	bne.w bad
	tst.l d2
	bmi.w arityReady
	cmpi.l #2, d4
	blo.w bad
	cmpi.l #3, d4
	bhi.w bad
	tst.l d2
	beq.w arityReady
	cmp.l d2, d4
	bne.w bad
arityReady
	cmp.l d4, d1
	bhs.w bad
	movea.l a4, a0
	movea.l a5, a1
	move.l d4, d3
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/d4-d5/a2-a5
	tst.l d0
	rts
	.bend  ; view
	.pub

; A0/A1=complete prepared operand. D0/CCR=status, D3=arity (2/3).
; Other registers preserved; selected item bounds are not exposed.
count	.block
	movem.l d1/a0-a1, -(sp)
	moveq #0, d0
	moveq #0, d1
	bsr.w select
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; count
	.priv

; A2/A3=remaining bounded input, D4=ordinal, D1=selected ordinal.
; Advance over one numeric name or compiled scalar; retain selected bounds.
; D0/CCR=status; A2,D4-D5/A4-A5 are scratch owned by select.
leaf	.block
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #4, d5
	cmpi.b #expression.COMPILED_TAG, (a2)
	beq.w sized
	cmpi.b #products.BINARY_TAG, (a2)
	bne.w name
sized
	moveq #0, d5
	move.b 1(a2), d5
	beq.w bad
	addq.l #2, d5
	bra.w length
name
	cmpi.b #NAME_MAX, (a2)
	bhi.w bad
length
	cmp.l d5, d0
	blo.w bad
	cmp.l d1, d4
	bne.w advance
	movea.l a2, a4
	lea 0(a2, d5.l), a5
advance
	adda.l d5, a2
	addq.l #1, d4
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; leaf
	.endsection
	.endmodule
