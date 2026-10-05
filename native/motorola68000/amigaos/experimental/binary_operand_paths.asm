; Numeric package-owned expression paths over bounded prepared operands.
; @opforge-owner: experimental.amigaos.binary_operand_paths
	.module experimental.amigaos.binary_operand_paths
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_nested_operands as nested
	.use experimental.amigaos.binary_products as products
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as runtime
	.pub
KIND = 26
STATUS_OK = 0
STATUS_MALFORMED = 1
MAX_BYTES = 32
STEP_BYTES = 4
UNWRAP_PAREN = 1
UNWRAP_BRACKET = 2
TUPLE_CHILD = 3
REGISTER_VALUE = 4
QUALIFIED_REGISTER = 5
SCALE_VALUE = 6
MEMBER_VALUE = 7
	.priv
ProductView	.struct
LeftStart	.long ?
LeftEnd	.long ?
RightEnd	.long ?
RightStart	.long ?
	.endstruct
PRODUCT_VIEW_BYTES = ProductView.RightStart+4
	.section code, kind=code
	.pub

; A0/A1=exact prepared operand,A2=package.Context,A4=Projection.
; D0/CCR=status,D3=projected value,D2=unresolved (scalar terminal only).
; Preserves D4-D7/A2-A6. Containers select bounded views; terminal steps
; consume exact leaves. The package owns all class, qualifier and member IDs.
evaluate	.block
	movem.l d4-d7/a2-a6, -(sp)
	cmpi.b #KIND, package.Projection.Kind(a4)
	bne.w bad
	tst.w package.Projection.Reserved(a4)
	bne.w bad
	moveq #0, d4
	move.w package.Projection.Class(a4), d4
	cmpi.l #STEP_BYTES, d4
	blo.w bad
	cmpi.l #MAX_BYTES, d4
	bhi.w bad
	move.l d4, d0
	andi.l #3, d0
	bne.w bad
	movea.l package.Context.Package(a2), a6
	move.l package.Projection.Literal(a4), d0
	cmpi.l #package.HEADER_BYTES, d0
	blo.w bad
	move.l d0, d1
	andi.l #3, d1
	bne.w bad
	move.l d0, d1
	add.l d4, d1
	bcs.w bad
	cmp.l package.Header.Bytes(a6), d1
	bhi.w bad
	movea.l a6, a3
	adda.l d0, a3
	movea.l a6, a5
	adda.l d1, a5
next
	moveq #0, d6
	move.b (a3)+, d6
	moveq #0, d7
	move.b (a3)+, d7
	moveq #0, d5
	move.w (a3)+, d5
	cmpa.l a5, a3
	beq.w terminal
	; Container step arguments are closed, with no ignored payload bits.
	tst.l d5
	bne.w bad
	cmpi.b #TUPLE_CHILD, d6
	beq.w child
	tst.l d7
	bne.w bad
	moveq #nested.RAW_PAREN_OPEN, d1
	cmpi.b #UNWRAP_PAREN, d6
	beq.w unwrap
	cmpi.b #UNWRAP_BRACKET, d6
	bne.w bad
	moveq #nested.RAW_BRACKET_OPEN, d1
unwrap
	jsr nested.unwrap
	bne.w done
	bra.w next
child
	cmpi.l #2, d7
	bhi.w bad
	move.l d7, d1
	jsr nested.select
	bne.w done
	bra.w next
terminal
	cmpi.b #REGISTER_VALUE, d6
	beq.w plainRegister
	cmpi.b #QUALIFIED_REGISTER, d6
	beq.w qualified
	cmpi.b #SCALE_VALUE, d6
	beq.w scale
	cmpi.b #MEMBER_VALUE, d6
	beq.w member
	bra.w bad
plainRegister
	tst.l d7
	bne.w bad
	bsr.w register
	bra.w done
qualified
	tst.l d7
	beq.w bad
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #products.BINARY_TAG, (a0)
	bne.w qualifiedLeaf
	bsr.w productFactor
	bne.w done
qualifiedLeaf
	bsr.w register
	bra.w done
scale
	tst.l d7
	bne.w bad
	tst.l d5
	bne.w bad
	bsr.w productFactor
	bne.w done
	tst.l expression.Frame.High(a2)
	bne.w bad
	moveq #0, d3
	cmpi.l #1, d1
	beq.w known
	moveq #1, d3
	cmpi.l #2, d1
	beq.w known
	moveq #2, d3
	cmpi.l #4, d1
	beq.w known
	moveq #3, d3
	cmpi.l #8, d1
	bne.w bad
known
	moveq #0, d2
	moveq #STATUS_OK, d0
	bra.w done
member
	tst.l d7
	bne.w bad
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #10, d0
	blo.w bad
	lea -5(a1), a6
	cmpi.b #nested.RAW_MEMBER, (a6)
	bne.w bad
	cmpi.b #nested.RAW_NAME_MAX, 1(a6)
	bhi.w bad
	tst.b 4(a6)
	bne.w bad
	cmp.w 2(a6), d5
	bne.w bad
	movea.l a6, a1
	moveq #nested.RAW_PAREN_OPEN, d1
	jsr nested.unwrap
	bne.w done
	bsr.w scalar
	bne.w done
	tst.l d2
	bne.w memberValue
	tst.l expression.Frame.High(a2)
	beq.w memberValue
	cmpi.l #-1, expression.Frame.High(a2)
	bne.w bad
	tst.l d1
	bpl.w bad
memberValue
	move.l d1, d3
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d4-d7/a2-a6
	tst.l d0
	rts
	.bend  ; evaluate
	.priv

; Exact numeric register leaf. D5=class,D7=encoded qualifier (zero plain).
; D0/CCR=status,D3=value,D2=0; scratch D1/D4/A6. Cursor remains bounded.
register	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #nested.RAW_NAME_MAX, (a0)
	bhi.w bad
	cmp.b 3(a0), d7
	bne.w bad
	moveq #0, d1
	move.w 1(a0), d1
	movea.l package.Context.Package(a2), a6
	move.l package.Header.RegisterRows(a6), d0
	move.l package.Header.RegisterCount(a6), d2
	cmpi.l #$ffff, d2
	bhi.w bad
	move.l d2, d4
	mulu.w #6, d4
	add.l d0, d4
	bcs.w bad
	cmp.l package.Header.Bytes(a6), d4
	bhi.w bad
	adda.l d0, a6
next
	tst.l d2
	beq.w bad
	cmp.w (a6), d1
	bne.w advance
	cmp.w 2(a6), d5
	bne.w advance
	moveq #0, d3
	move.w 4(a6), d3
	moveq #0, d2
	moveq #STATUS_OK, d0
	rts
advance
	addq.l #6, a6
	subq.l #1, d2
	bra.w next
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; register

; A0/A1=product. Pick the opposite leaf of the first successful scalar,
; right before left. D1=value, Frame.High retains all 64 bits,D2=0.
; Preserves D4-D7/A2-A5; A6 scratch. Unknowns do not select a factor.
productFactor	.block
	movem.l a5, -(sp)
	jsr products.split
	bne.w done
	movem.l a0-a1/a5-a6, -(sp)
	movea.l ProductView.RightStart(sp), a0
	movea.l ProductView.RightEnd(sp), a1
	bsr.w scalar
	bne.w left
	tst.l d2
	beq.w chooseLeft
left
	movea.l ProductView.LeftStart(sp), a0
	movea.l ProductView.LeftEnd(sp), a1
	bsr.w scalar
	bne.w bad
	tst.l d2
	bne.w bad
	movea.l ProductView.RightStart(sp), a0
	movea.l ProductView.RightEnd(sp), a1
	bra.w chosen
chooseLeft
	movea.l ProductView.LeftStart(sp), a0
	movea.l ProductView.LeftEnd(sp), a1
chosen
	adda.w #PRODUCT_VIEW_BYTES, sp
	moveq #STATUS_OK, d0
	bra.w done
bad
	adda.w #PRODUCT_VIEW_BYTES, sp
	moveq #STATUS_MALFORMED, d0
done
	movea.l (sp)+, a5
	tst.l d0
	rts
	.bend  ; productFactor

; Evaluate one exact compiled scalar or unqualified name. D1=low64,
; D2=unresolved; Frame.High=high64 on success. Preserve A0/A1 and D3-D7.
; Names use a temporary pointer-free PUSH_SYMBOL capsule for shared ExprVM.
scalar	.block
	movem.l a0-a1, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #expression.COMPILED_TAG, (a0)
	beq.w compiled
	cmpi.l #4, d0
	bne.w bad
	cmpi.b #nested.RAW_NAME_MAX, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w bad
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
	bra.w done
compiled
	jsr expression.evaluate
	bne.w done
	cmpa.l a1, a0
	bne.w bad
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; scalar
	.endsection
	.endmodule
