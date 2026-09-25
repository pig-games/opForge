; Find section-relative values before an instruction or data value is emitted.
; @opforge-owner: experimental.amigaos.binary_hunk_references
	.module experimental.amigaos.binary_hunk_references
	.cpu 68020
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_package as pkg
	.pub
STATUS_CLEAR = 0
STATUS_SECTION = 1
STATUS_BAD = 2
WRAPPER = $81
	.section code, kind=code

; A0=bounded compiled expression wrapper, A1=end, A2=Context.
; D0/CCR=0 if absolute, 1 if it references a section symbol or current PC,
; 2 if malformed. A0 advances past the wrapper. Other registers are preserved.
expression	.block
	movem.l d1-d7/a1-a6, -(sp)
	move.l a1, d1
	sub.l a0, d1
	bcs.w bad
	cmpi.l #3, d1
	blo.w bad
	cmpi.b #WRAPPER, (a0)+
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	beq.w bad
	movea.l a0, a4
	adda.l d1, a4
	cmpa.l a1, a4
	bhi.w bad
	moveq #STATUS_CLEAR, d5
next
	cmpa.l a4, a0
	bhs.w bad
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #runtime.EXPRVM_V2_OPCODE_END, d1
	beq.w end
	cmpi.b #runtime.COMPACT_I8, d1
	beq.w byte
	cmpi.b #runtime.COMPACT_I16, d1
	beq.w word
	cmpi.b #runtime.COMPACT_I32, d1
	beq.w long
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d1
	beq.w current
	cmpi.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d1
	beq.w symbol
	cmpi.b #runtime.COMPACT_NEGATE, d1
	beq.w next
	cmpi.b #runtime.COMPACT_ADD, d1
	beq.w next
	cmpi.b #runtime.COMPACT_SUBTRACT, d1
	beq.w next
	cmpi.b #runtime.COMPACT_MULTIPLY, d1
	beq.w next
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d1
	beq.w byte
	cmpi.b #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d1
	beq.w byte
	bra.w bad
current
	moveq #STATUS_SECTION, d5
	bra.w next
symbol
	moveq #2, d1
	bsr.w available
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	moveq #0, d2
	move.b (a0)+, d2
	lsl.w #8, d2
	or.w d2, d1
	bsr.w sectionId
	bne.w bad
	bra.w next
byte
	moveq #1, d1
	bra.w skip
word
	moveq #2, d1
	bra.w skip
long
	moveq #4, d1
skip
	bsr.w available
	bne.w bad
	adda.l d1, a0
	bra.w next
end
	cmpa.l a4, a0
	bne.w bad
	move.l d5, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a1-a6
	tst.l d0
	rts
	.bend  ; expression

; A0=prepared operand tokens, A1=end, A2=Context. The same statuses as
; expression are returned; A0 advances to the end. Other regs are preserved.
tokens	.block
	movem.l d1-d7/a1-a6, -(sp)
	movea.l a1, a4
	moveq #STATUS_CLEAR, d5
next
	cmpa.l a1, a0
	beq.w clear
	bhi.w bad
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #WRAPPER, d1
	beq.w wrapped
	cmpi.b #1, d1
	bls.w name
	cmpi.b #2, d1
	beq.w number
	cmpi.b #3, d1
	beq.w string
	cmpi.b #4, d1
	blo.w bad
	cmpi.b #40, d1
	bhi.w bad
	bra.w next
wrapped
	subq.l #1, a0
	jsr expression
	cmpi.l #STATUS_BAD, d0
	beq.w bad
	tst.l d0
	beq.w next
	moveq #STATUS_SECTION, d5
	bra.w next
name
	moveq #3, d1
	bsr.w available
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
	moveq #0, d2
	move.b (a0)+, d2
	lsl.w #8, d2
	or.w d2, d1
	tst.b (a0)+  ; qualifier is not part of the numeric ID
	bsr.w sectionId
	bne.w bad
	bra.w next
number
	moveq #4, d1
	bra.w skipToken
string
	moveq #1, d1
	bsr.w available
	bne.w bad
	moveq #0, d1
	move.b (a0)+, d1
skipToken
	bsr.w available
	bne.w bad
	adda.l d1, a0
	bra.w next
clear
	move.l d5, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a1-a6
	tst.l d0
	rts
	.bend  ; tokens
	.priv

; D1=bytes needed at A0. Uses the current bounded end in A4 for expression
; and A1 for tokens; callers set A4 to their own end before entry.
available	.block
	move.l a4, d0
	sub.l a0, d0
	bcs.w bad
	cmp.l d1, d0
	blo.w bad
	moveq #STATUS_CLEAR, d0
	rts
bad
	moveq #STATUS_BAD, d0
	rts
	.bend  ; available

; D1=numeric symbol ID, A2=Context, D5=accumulated reference status.
sectionId	.block
	cmp.l pkg.Context.Count(a2), d1
	bhs.w bad
	movea.l pkg.Context.SectionIds(a2), a3
	move.l a3, d0
	beq.w bad
	tst.b 0(a3, d1.l)
	beq.w clear
	moveq #STATUS_SECTION, d5
clear
	moveq #STATUS_CLEAR, d0
	rts
bad
	moveq #STATUS_BAD, d0
	rts
	.bend  ; sectionId
	.endsection
	.endmodule
