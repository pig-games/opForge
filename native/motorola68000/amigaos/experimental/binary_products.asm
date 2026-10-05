; Bounded product transport and shared ExprVM identity evaluation; no target rules.
; @opforge-owner: experimental.amigaos.binary_products
	.module experimental.amigaos.binary_products
	.cpu 68020
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as runtime
	.pub
BINARY_TAG = $82
MULTIPLY = 20
STATUS_OK = 0
STATUS_MALFORMED = 1
STATUS_OUTPUT = 2
	.priv
FACTOR_UNRESOLVED = 3
RAW_NAME_MAX = 1
RAW_LITERAL = 2
RAW_STRING = 3
RAW_CURRENT = 6
RAW_OPEN = 14
RAW_CLOSE = 15
RAW_PLUS = 18
RAW_MINUS = 19
RAW_BIT_NOT = 26
NODE_HEADER_BYTES = 4
ProductView	.struct
LeftStart	.long ?
LeftEnd	.long ?
RightEnd	.long ?
RightStart	.long ?
	.endstruct
PRODUCT_VIEW_BYTES = ProductView.RightStart+4
	.section code, kind=code
	.pub

; A0/A1=exact raw leaf span, A3/A4=bounded output.
; D0/CCR=status; A0/A3 advance. Preserves D1-D7/A1-A2/A4-A6.
; Names remain four-byte leaves, including their qualifier. Other scalars use
; expression.compile with complete consumption. One outer multiply becomes
; $82,u8 payload bytes,u8 operator,u8 lhs bytes,left leaf,right leaf.
; Multiple outer products or mixed outer infixes are rejected, never reassociated.
; Children contain names or compiled scalars, never pointers or binary nodes.
; Failed output is uncommitted caller scratch.
prepare	.block
	movem.l d1-d7/a1-a2/a4-a6, -(sp)
	bsr.w scan
	bne.w done
	move.l a6, d0
	beq.w plain
	move.l a4, d0
	sub.l a3, d0
	bcs.w output
	cmpi.l #NODE_HEADER_BYTES, d0
	blo.w output
	movea.l a3, a5
	move.b #BINARY_TAG, (a3)+
	clr.b (a3)+
	move.b #MULTIPLY, (a3)+
	clr.b (a3)+
	movea.l a1, a2
	movea.l a6, a1
	bsr.w scalar
	bne.w done
	move.l a3, d1
	sub.l a5, d1
	subi.l #NODE_HEADER_BYTES, d1
	cmpi.l #255, d1
	bhi.w output
	move.b d1, 3(a5)
	lea 1(a6), a0
	movea.l a2, a1
	bsr.w scalar
	bne.w done
	move.l a3, d1
	sub.l a5, d1
	subq.l #2, d1
	cmpi.l #255, d1
	bhi.w output
	move.b d1, 1(a5)
	moveq #STATUS_OK, d0
	bra.w done
plain
	bsr.w scalar
	bra.w done
output
	moveq #STATUS_OUTPUT, d0
done
	movem.l (sp)+, d1-d7/a1-a2/a4-a6
	tst.l d0
	rts
	.bend  ; prepare

; A0/A1=exact prepared binary node. D0/CCR=status, D1=operator token.
; Success: A0/A1=left leaf bounds, A6/A5=right leaf start/end.
; Preserves D2-D7/A2-A4. Child validation checks framing only; ExprVM owns
; compiled payload validity and values. Nested binary nodes are not accepted.
split	.block
	movem.l d2-d7/a2-a4, -(sp)
	movea.l a0, a2
	movea.l a1, a5
	move.l a1, d2
	sub.l a0, d2
	bcs.w bad
	cmpi.l #NODE_HEADER_BYTES, d2
	blo.w bad
	cmpi.b #BINARY_TAG, (a0)
	bne.w bad
	moveq #0, d0
	move.b 1(a0), d0
	subq.l #2, d2
	cmp.l d2, d0
	bne.w bad
	moveq #0, d1
	move.b 2(a0), d1
	cmpi.b #MULTIPLY, d1
	bne.w bad
	moveq #0, d0
	move.b 3(a0), d0
	beq.w bad
	subq.l #2, d2
	cmp.l d2, d0
	bhs.w bad
	lea NODE_HEADER_BYTES(a0), a0
	lea 0(a0, d0.l), a1
	movea.l a1, a6
	movea.l a0, a2
	movea.l a1, a3
	bsr.w preparedLeaf
	bne.w bad
	movea.l a6, a2
	movea.l a5, a3
	bsr.w preparedLeaf
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d2-d7/a2-a4
	tst.l d0
	rts
	.bend  ; split

; A0/A1=binary node, A2=expression.Frame, D5=u16 expected identity.
; D4=nonzero for right-first successful-value precedence.
; D0/CCR=status; success returns opposite child A0/A1 and identity D3.
; Other registers preserved. Unknown values cannot prove a match. A known
; 64-bit nonmatch, including a nonzero high word, prevents strict left fallback.
; An unresolved right scalar keeps the strict barrier; it is not an evaluation
; error authorizing a left fallback. Scalar evaluation uses the shared ExprVM.
resolveIdentity	.block
	movem.l d1-d2/d4-d5/a2-a6, -(sp)
	andi.l #$ffff, d5
	bsr.w split
	bne.w bad
	movem.l a0-a1/a5-a6, -(sp)
	movea.l ProductView.RightStart(sp), a0
	movea.l ProductView.RightEnd(sp), a1
	bsr.w factor
	beq.w rightKnown
	cmpi.l #FACTOR_UNRESOLVED, d0
	bne.w left
	tst.l d4
	bne.w framedBad
	bra.w left
rightKnown
	tst.l d2
	bne.w nonmatch
	cmp.l d5, d1
	beq.w chooseLeft
nonmatch
	tst.l d4
	bne.w framedBad
left
	movea.l ProductView.LeftStart(sp), a0
	movea.l ProductView.LeftEnd(sp), a1
	bsr.w factor
	bne.w framedBad
	tst.l d2
	bne.w framedBad
	cmp.l d5, d1
	bne.w framedBad
	movea.l ProductView.RightStart(sp), a0
	movea.l ProductView.RightEnd(sp), a1
	bra.w matched
chooseLeft
	movea.l ProductView.LeftStart(sp), a0
	movea.l ProductView.LeftEnd(sp), a1
matched
	move.l d5, d3
	adda.w #PRODUCT_VIEW_BYTES, sp
	moveq #STATUS_OK, d0
	bra.w done
framedBad
	adda.w #PRODUCT_VIEW_BYTES, sp
bad
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d1-d2/d4-d5/a2-a6
	tst.l d0
	rts
	.bend  ; resolveIdentity
	.priv

; A0/A1=scalar/name leaf, A2=expression.Frame.
; D0/CCR=status, D1=low value, D2=high value. Preserve other registers/bounds.
; An unqualified numeric name becomes a bounded PUSH_SYMBOL capsule. Known
; values retain all 64 bits. D0=FACTOR_UNRESOLVED distinguishes an unknown
; scalar from an evaluation error; qualified names retain that opaque state.
factor	.block
	movem.l a0-a1, -(sp)
	move.l a1, d0
	sub.l a0, d0
	bcs.w bad
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #expression.COMPILED_TAG, (a0)
	beq.w compiled
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #RAW_NAME_MAX, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w unknown
	subq.l #6, sp
	move.b #expression.COMPILED_TAG, (sp)
	move.b #4, 1(sp)
	move.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, 2(sp)
	move.b 2(a0), 3(sp)
	move.b 1(a0), 4(sp)
	move.b #runtime.EXPRVM_V2_OPCODE_END, 5(sp)
	movea.l sp, a0
	lea 6(sp), a1
	jsr expression.evaluate
	adda.w #6, sp
	bra.w known
compiled
	jsr expression.evaluate
	bne.w done
	cmpa.l a1, a0
	bne.w bad
known
	tst.l d0
	bne.w done
	tst.l d2
	bne.w unknown
	move.l expression.Frame.High(a2), d2
	moveq #STATUS_OK, d0
	bra.w done
unknown
	moveq #FACTOR_UNRESOLVED, d0
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; factor

; Compile/copy one exact scalar child. Same cursor ABI as expression.compile;
; D0-D1 scratch. Prepared wrappers cannot enter through the raw interface.
scalar	.block
	move.l a1, d0
	sub.l a0, d0
	bcs.w bad
	beq.w bad
	cmpi.b #RAW_NAME_MAX, (a0)
	bhi.w compile
	cmpi.l #4, d0
	bne.w compile
	move.l a4, d0
	sub.l a3, d0
	bcs.w output
	cmpi.l #4, d0
	blo.w output
	moveq #3, d1
copy
	move.b (a0)+, (a3)+
	dbra d1, copy
	moveq #STATUS_OK, d0
	rts
compile
	jsr expression.compile
	bne.w done
	cmpa.l a1, a0
	bne.w bad
	moveq #STATUS_OK, d0
done
	rts
output
	moveq #STATUS_OUTPUT, d0
	rts
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; scalar

; A2/A3=exact prepared child span. D0/CCR=status; D2 scratch.
; The compiled payload remains opaque and must be nonempty.
preparedLeaf	.block
	move.l a3, d0
	sub.l a2, d0
	bcs.w bad
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #RAW_NAME_MAX, (a2)
	bls.w name
	cmpi.b #expression.COMPILED_TAG, (a2)
	bne.w bad
	moveq #0, d2
	move.b 1(a2), d2
	beq.w bad
	addq.l #2, d2
	cmp.l d2, d0
	bne.w bad
	bra.w ok
name
	cmpi.l #4, d0
	bne.w bad
ok
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; preparedLeaf

; Find one outer multiply without reading token payloads as syntax.
; A0/A1 unchanged; A6=operator cursor or zero. D0/CCR=status.
; Scratch D1-D5/A2. D2=parenthesis depth, D3=primary expected,
; D4=other outer infix. Scalar parsing and precedence remain compiler-owned.
scan	.block
	movea.l a0, a2
	suba.l a6, a6
	moveq #0, d2
	moveq #1, d3
	moveq #0, d4
next
	cmpa.l a1, a2
	bhs.w finish
	moveq #0, d1
	move.b (a2), d1
	moveq #4, d5
	cmpi.b #RAW_NAME_MAX, d1
	bls.w primary
	moveq #5, d5
	cmpi.b #RAW_LITERAL, d1
	beq.w primary
	cmpi.b #RAW_STRING, d1
	beq.w stringToken
	moveq #1, d5
	cmpi.b #RAW_CURRENT, d1
	beq.w primary
	cmpi.b #RAW_OPEN, d1
	beq.w opening
	cmpi.b #RAW_CLOSE, d1
	beq.w closing
	cmpi.b #RAW_PLUS, d1
	blo.w bad
	cmpi.b #30, d1
	bhi.w bad
	cmpi.b #27, d1
	beq.w bad
	tst.l d3
	beq.w infix
	cmpi.b #RAW_MINUS, d1
	bls.w advance
	cmpi.b #RAW_BIT_NOT, d1
	beq.w advance
	bra.w bad
infix
	cmpi.b #RAW_BIT_NOT, d1
	beq.w bad
	tst.l d2
	bne.w operator
	cmpi.b #MULTIPLY, d1
	bne.w other
	move.l a6, d0
	bne.w bad
	movea.l a2, a6
	bra.w operator
other
	moveq #1, d4
operator
	moveq #1, d3
	bra.w advance
opening
	tst.l d3
	beq.w bad
	addq.l #1, d2
	cmpi.l #expression.MAX_DEPTH, d2
	bhi.w bad
	bra.w advance
closing
	tst.l d3
	bne.w bad
	tst.l d2
	beq.w bad
	subq.l #1, d2
	bra.w advance
stringToken
	move.l a1, d0
	sub.l a2, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d5
	move.b 1(a2), d5
	addq.l #2, d5
primary
	tst.l d3
	beq.w bad
	moveq #0, d3
advance
	move.l a1, d0
	sub.l a2, d0
	cmp.l d5, d0
	blo.w bad
	adda.l d5, a2
	bra.w next
finish
	cmpa.l a1, a2
	bne.w bad
	tst.l d2
	bne.w bad
	tst.l d3
	bne.w bad
	move.l a6, d0
	beq.w ok
	tst.l d4
	bne.w bad
ok
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; scan
	.endsection
	.endmodule
