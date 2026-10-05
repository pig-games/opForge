; Bounded nested operand transport; syntax remains package owned.
; @opforge-owner: experimental.amigaos.binary_nested_operands
	.module experimental.amigaos.binary_nested_operands
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_products as products
	.use opasm.amigaos.binary_expression as expression
	.pub
STATUS_OK = 0
STATUS_MALFORMED = 1
STATUS_OUTPUT = 2
MAX_DEPTH = 8
RAW_NAME_MAX = 1
RAW_NUMBER = 2
RAW_STRING = 3
RAW_COMMA = 4
RAW_MEMBER = 7
RAW_BRACKET_OPEN = 10
RAW_BRACKET_CLOSE = 11
RAW_PAREN_OPEN = 14
RAW_PAREN_CLOSE = 15
	.section code, kind=code

; A0/A1=remaining raw line,A2=raw package header.
; A6=first complete operand end (before comma).
; D0=0 ordinary,1 complex,2 malformed; other registers preserved. CCR=D0.
; Brackets and scalar member/displacement syntax select recursive preparation.
needs	.block
	movem.l d1-d7/a0-a5, -(sp)
	cmpa.l a1, a0
	bhs.w bad
	movea.l a2, a5
	moveq #0, d7
	bsr.w span
	bne.w bad
	cmpa.l a1, a6
	beq.w ready
	cmpi.b #RAW_COMMA, (a6)
	bne.w bad
ready
	btst #1, d7
	bne.w complex
	tst.l d7
	beq.w classified
	moveq #0, d7
	movea.l a0, a2
classify
	cmpa.l a6, a2
	bhs.w classified
	cmpi.b #RAW_MEMBER, (a2)
	bne.w advance
	cmpa.l a0, a2
	beq.w advance
	move.l a6, d0
	sub.l a2, d0
	cmpi.l #5, d0
	blo.w bad
	cmpi.b #RAW_NAME_MAX, 1(a2)
	bhi.w advance
	moveq #0, d0
	move.w 2(a2), d0
	move.l a2, -(sp)
	movea.l a5, a2
	bsr.w typedField
	movea.l (sp)+, a2
	cmpi.l #2, d0
	beq.w bad
	tst.l d0
	bne.w advance
	moveq #1, d7
advance
	bsr.w tokenBytes
	bne.w bad
	adda.l d1, a2
	bra.w classify
complex
	moveq #1, d7
classified
	move.l d7, d0
	bra.w done
bad
	moveq #2, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; needs

; A0/A1=exact raw operand,A2=raw package header; A3/A4=destination.
; D0/CCR=status,
; A0/A3 advance. Preserves D1-D7/A1-A2/A4-A6. Partial output is scratch.
; Retains parentheses/brackets and tuple commas; leaves use products.prepare.
prepare	.block
	movem.l d1-d7/a1-a2/a4-a6, -(sp)
	move.l a2, d7
	moveq #0, d5
	bsr.w node
	movem.l (sp)+, d1-d7/a1-a2/a4-a6
	tst.l d0
	rts
	.bend  ; prepare

; A0/A1=exact prepared wrapper,D1=opening token (14 or 10).
; D0/CCR=status; success A0/A1=interior. Other registers preserved.
unwrap	.block
	movem.l d1-d7/a2-a6, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	cmp.b (a0), d1
	bne.w bad
	cmpi.b #RAW_PAREN_OPEN, d1
	beq.w paren
	cmpi.b #RAW_BRACKET_OPEN, d1
	bne.w bad
	moveq #RAW_BRACKET_CLOSE, d2
	bra.w closing
paren
	moveq #RAW_PAREN_CLOSE, d2
closing
	cmp.b -1(a1), d2
	bne.w bad
	moveq #0, d7
	bsr.w span
	bne.w bad
	cmpa.l a1, a6
	bne.w bad
	addq.l #1, a0
	subq.l #1, a1
	movem.l a0-a1, -(sp)
	moveq #0, d1
	moveq #1, d0
	bsr.w sequence
	movem.l (sp)+, a0-a1
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a2-a6
	tst.l d0
	rts
	.bend  ; unwrap

; A0/A1=prepared comma sequence interior,D1=zero-based child ordinal.
; D0/CCR=status; success A0/A1=exact selected child. Other regs preserved.
; Requires at least two children (Tuple/List); every child is framed and
; nonempty, including children after the selection.
select	.block
	moveq #2, d0
	bra.w sequence
	.bend  ; select
	.priv

; D0=minimum child count; otherwise the public select ABI.
sequence	.block
	movem.l d1-d7/a2-a6, -(sp)
	move.l d0, -(sp)
	move.l d1, d6
	moveq #0, d5
	suba.l a3, a3
	suba.l a4, a4
next
	moveq #0, d7
	bsr.w span
	bne.w bad
	cmpa.l a0, a6
	beq.w bad
	cmp.l d6, d5
	bne.w tail
	movea.l a0, a3
	movea.l a6, a4
tail
	addq.l #1, d5
	cmpa.l a1, a6
	beq.w finish
	cmpi.b #RAW_COMMA, (a6)
	bne.w bad
	lea 1(a6), a0
	bra.w next
finish
	cmp.l (sp), d5
	blo.w bad
	move.l a3, d0
	beq.w bad
	movea.l a3, a0
	movea.l a4, a1
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	addq.l #4, sp
	movem.l (sp)+, d1-d7/a2-a6
	tst.l d0
	rts
	.bend  ; sequence
	.priv

; Exact node. Recursion has its own saved bounds and temporary registers.
node	.block
	movem.l d1-d4/d6-d7/a1-a2/a5-a6, -(sp)
	addq.l #1, d5
	cmpi.l #MAX_DEPTH, d5
	bhi.w bad
	cmpa.l a1, a0
	bhs.w bad
	; Find outer suffixes while skipping bounded token payloads.
	movea.l a0, a2
	suba.l a5, a5
	moveq #0, d4
scan
	cmpa.l a1, a2
	bhs.w scanned
	moveq #0, d0
	move.b (a2), d0
	cmpi.b #RAW_PAREN_OPEN, d0
	beq.w open
	cmpi.b #RAW_BRACKET_OPEN, d0
	beq.w open
	cmpi.b #RAW_PAREN_CLOSE, d0
	beq.w close
	cmpi.b #RAW_BRACKET_CLOSE, d0
	beq.w close
	tst.l d4
	bne.w token
	cmpi.b #RAW_MEMBER, d0
	beq.w member
	bra.w token
open
	tst.l d4
	bne.w opened
	cmpa.l a0, a2
	beq.w opened
	movea.l a2, a5
opened
	addq.l #1, d4
	bra.w token
close
	subq.l #1, d4
	bmi.w bad
token
	bsr.w tokenBytes
	bne.w bad
	adda.l d1, a2
	bra.w scan
scanned
	tst.l d4
	bne.w bad
	move.l a5, d0
	bne.w prefixTuple
	move.b (a0), d2
	cmpi.b #RAW_PAREN_OPEN, d2
	beq.w wrapper
	cmpi.b #RAW_BRACKET_OPEN, d2
	beq.w wrapper
	jsr products.prepare
	bra.w done
member
	; The final field is a four-byte unqualified numeric name.
	move.l a1, d0
	sub.l a2, d0
	cmpi.l #5, d0
	bne.w token
	cmpi.b #RAW_NAME_MAX, 1(a2)
	bhi.w bad
	tst.b 4(a2)
	bne.w bad
	moveq #0, d0
	move.w 2(a2), d0
	move.l a2, -(sp)
	movea.l d7, a2
	bsr.w typedField
	movea.l (sp)+, a2
	cmpi.l #2, d0
	beq.w bad
	tst.l d0
	bne.w token
	movea.l a2, a6
	movea.l a1, a5
	movea.l a2, a1
	moveq #RAW_PAREN_OPEN, d2
	bsr.w put
	bne.w done
	jsr products.prepare
	bne.w done
	moveq #RAW_PAREN_CLOSE, d2
	bsr.w put
	bne.w done
	movea.l a5, a1
	movea.l a6, a0
	moveq #5, d6
	bsr.w copy
	bra.w done
prefixTuple
	; scalar(tuple) has the same transport as (scalar,tuple children).
	cmpi.b #RAW_PAREN_OPEN, (a5)
	bne.w bad
	movea.l a1, a6
	movea.l a5, a1
	moveq #RAW_PAREN_OPEN, d2
	bsr.w put
	bne.w done
	bsr.w node
	bne.w done
	moveq #RAW_COMMA, d2
	bsr.w put
	bne.w done
	movea.l a6, a1
	addq.l #1, a0
	bsr.w children
	bra.w done
wrapper
	bsr.w put
	bne.w done
	addq.l #1, a0
	bsr.w children
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	subq.l #1, d5
	movem.l (sp)+, d1-d4/d6-d7/a1-a2/a5-a6
	tst.l d0
	rts
	.bend  ; node

; A0=first child,A1=exact wrapper end. Match the opening's close token D2.
children	.block
	movem.l d2-d4/d7/a1/a5-a6, -(sp)
	moveq #RAW_PAREN_CLOSE, d3
	cmpi.b #RAW_BRACKET_OPEN, d2
	bne.w next
	moveq #RAW_BRACKET_CLOSE, d3
next
	move.l d7, -(sp)
	moveq #0, d7
	bsr.w span
	move.l (sp)+, d7
	tst.l d0
	bne.w bad
	cmpa.l a0, a6
	beq.w bad
	movea.l a1, a5
	movea.l a6, a1
	bsr.w node
	movea.l a5, a1
	bne.w done
	cmpa.l a1, a0
	bhs.w bad
	moveq #0, d2
	move.b (a0)+, d2
	cmpi.b #RAW_COMMA, d2
	beq.w comma
	cmp.b d3, d2
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	bsr.w put
	bra.w done
comma
	bsr.w put
	beq.w next
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d2-d4/d7/a1/a5-a6
	tst.l d0
	rts
	.bend  ; children

; Bounded first child/operand scan. A6=end; D7 complex flag. Scratch D0-D4/A2.
; Balanced nesting is checked using eight expected closing bytes on the stack.
span	.block
	subq.l #8, sp
	movea.l a0, a6
	moveq #0, d4
next
	cmpa.l a1, a6
	bhs.w finish
	moveq #0, d0
	move.b (a6), d0
	cmpi.b #RAW_MEMBER, d0
	bne.w delimiters
	cmpa.l a0, a6
	beq.w delimiters
	bset #0, d7
delimiters
	cmpi.b #RAW_BRACKET_OPEN, d0
	beq.w bracket
	cmpi.b #RAW_PAREN_OPEN, d0
	beq.w paren
	cmpi.b #RAW_BRACKET_CLOSE, d0
	beq.w close
	cmpi.b #RAW_PAREN_CLOSE, d0
	beq.w close
	cmpi.b #RAW_COMMA, d0
	bne.w token
	tst.l d4
	beq.w ok
	bra.w token
bracket
	bset #1, d7
	moveq #RAW_BRACKET_CLOSE, d2
	bra.w push
paren
	moveq #RAW_PAREN_CLOSE, d2
push
	cmpi.l #MAX_DEPTH, d4
	bhs.w bad
	move.b d2, 0(sp, d4.l)
	addq.l #1, d4
	bra.w token
close
	tst.l d4
	beq.w ok
	subq.l #1, d4
	cmp.b 0(sp, d4.l), d0
	bne.w bad
token
	movea.l a6, a2
	bsr.w tokenBytes
	bne.w bad
	adda.l d1, a6
	bra.w next
finish
	tst.l d4
	bne.w bad
ok
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_MALFORMED, d0
done
	addq.l #8, sp
	tst.l d0
	rts
	.bend  ; span

; A2/A1 bounded raw token. D1=byte count, D0/CCR=status.
tokenBytes	.block
	moveq #1, d1
	moveq #0, d0
	move.b (a2), d0
	cmpi.b #RAW_NAME_MAX, d0
	bhi.w literal
	moveq #4, d1
	bra.w bounded
literal
	cmpi.b #RAW_NUMBER, d0
	bne.w string
	moveq #5, d1
	bra.w bounded
string
	cmpi.b #expression.COMPILED_TAG, d0
	beq.w payload
	cmpi.b #products.BINARY_TAG, d0
	beq.w payload
	cmpi.b #RAW_STRING, d0
	bne.w bounded
payload
	move.l a1, d0
	sub.l a2, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d1
	move.b 1(a2), d1
	addq.l #2, d1
bounded
	move.l a1, d0
	sub.l a2, d0
	cmp.l d1, d0
	blo.w bad
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; tokenBytes

; D0=numeric field ID,A2=raw package header. D0=0 typed member,
; 1 ordinary field,2 malformed metadata. All other registers preserved.
typedField	.block
	movem.l d1-d4/a0, -(sp)
	move.l d0, d4
	move.l package.Header.MemberBindings(a2), d0
	move.l package.Header.MemberBindingCount(a2), d1
	cmpi.l #$ffff, d1
	bhi.w bad
	move.l d1, d2
	mulu.w #package.MEMBER_BINDING_BYTES, d2
	add.l d0, d2
	bcs.w bad
	cmp.l package.Header.Bytes(a2), d2
	bhi.w bad
	movea.l a2, a0
	adda.l d0, a0
next
	tst.l d1
	beq.w ordinary
	cmp.w package.MemberBinding.Field(a0), d4
	beq.w member
	adda.w #package.MEMBER_BINDING_BYTES, a0
	subq.l #1, d1
	bra.w next
member
	moveq #0, d0
	bra.w done
ordinary
	moveq #1, d0
	bra.w done
bad
	moveq #2, d0
done
	movem.l (sp)+, d1-d4/a0
	tst.l d0
	rts
	.bend  ; typedField

; Emit one structural byte D2, preserving input cursor.
put	.block
	cmpa.l a4, a3
	bhs.w bad
	move.b d2, (a3)+
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_OUTPUT, d0
	rts
	.bend  ; put

; Copy D6 source bytes, bounded on both sides. Advances A0/A3.
copy	.block
	move.l a1, d0
	sub.l a0, d0
	cmp.l d6, d0
	blo.w bad
	move.l a4, d0
	sub.l a3, d0
	cmp.l d6, d0
	blo.w bad
next
	move.b (a0)+, (a3)+
	subq.l #1, d6
	bne.w next
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_OUTPUT, d0
	rts
	.bend  ; copy
	.endsection
	.endmodule
