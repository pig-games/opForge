; Compile bounded numeric tokens once; evaluate through the shared ExprVM.
; @opforge-owner: opasm.amigaos.binary_expression
	.module opasm.amigaos.binary_expression
	.cpu 68020
	.use exprvm.amigaos.runtime as runtime
	.include "telemetry_macros.i"
	.include "memory_telemetry.i"
	.pub
COMPILED_TAG = $80
MAX_DEPTH = 16
STATUS_OK = 0
STATUS_MALFORMED = 1
STATUS_OUTPUT = 2
STATUS_DEPTH = 3
Frame	.struct
Values	.long ?
Defined	.long ?
Count	.long ?
Pc	.long ?
	.endstruct
	.section code, kind=code
; A0=input tokens, A1=bounded end, A3=output, A4=bounded output end.
; Returns D0/CCR=status, A0=first delimiter/end, A3=after compiled wrapper.
; Preserves D1-D7/A1-A2/A4-A6. Failed output is uncommitted scratch.
; Wrapper: $80,u8 payload length, current ExprVM v2 program (LE payloads).
compile	.block
	movem.l d1-d7/a1-a2/a4-a6, -(sp)
	move.l a4, d0
	sub.l a3, d0
	bcs.w output
	cmpi.l #3, d0
	blo.w output
	move.b #COMPILED_TAG, (a3)+
	movea.l a3, a5
	clr.b (a3)+
	moveq #0, d6
	moveq #0, d7
	bsr.w sum
	tst.l d0
	bne.w done
	cmpi.w #1, d7
	bne.w malformed
	moveq #runtime.EXPRVM_V2_OPCODE_END, d0
	bsr.w emit
	bne.w done
	move.l a3, d1
	sub.l a5, d1
	subq.l #1, d1
	cmpi.l #255, d1
	bhi.w output
	move.b d1, (a5)
	.MEMORY_WORK #0, #1
	.MEMORY_WORK #2, d1
	moveq #STATUS_OK, d0
	bra.w done
output
	moveq #STATUS_OUTPUT, d0
	bra.w done
malformed
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a1-a2/a4-a6
	tst.l d0
	rts
	.bend  ; compile

; A0=compiled wrapper, A1=bounded end, A2=Frame.
; Returns D0/CCR=status, D1=i32 result, D2=unresolved, A0=after wrapper.
; Preserves D3-D7/A1-A6. No parser, lexical storage or source fallback.
evaluate	.block
	movem.l d3-d7/a1-a6, -(sp)
	.MEMORY_WORK #1, #1
	move.l a1, d0
	sub.l a0, d0
	bcs.w malformed
	cmpi.l #3, d0
	blo.w malformed
	cmpi.b #COMPILED_TAG, (a0)+
	bne.w malformed
	moveq #0, d3
	move.b (a0)+, d3
	beq.w malformed
	subq.l #2, d0
	cmp.l d3, d0
	blo.w malformed
	movea.l a0, a4
	adda.l d3, a4
	cmpi.b #runtime.EXPRVM_V2_OPCODE_END, -1(a4)
	bne.w malformed
	movea.l a2, a5
	move.l d3, d0
	move.l Frame.Count(a5), d1
	move.l Frame.Pc(a5), d2
	movea.l Frame.Values(a5), a2
	movea.l Frame.Defined(a5), a6
	jsr runtime.evalNumeric32
	movea.l a4, a0
	move.l d3, d1
	move.l d5, d2
	bra.w done
malformed
	moveq #STATUS_MALFORMED, d0
done
	movem.l (sp)+, d3-d7/a1-a6
	tst.l d0
	rts
	.bend  ; evaluate
	.priv

; Compiler helpers share bounded cursors A0/A1 and A3/A4. D6=syntax nesting,
; D7=postfix stack depth; D0-D3 scratch. An operator is saved across recursion.
sum	.block
	bsr.w product
	tst.l d0
	bne.w done
loop
	cmpa.l a1, a0
	bhs.w ok
	moveq #runtime.EXPRVM_BINARY_ADD, d3
	cmpi.b #18, (a0)
	beq.w operator
	moveq #runtime.EXPRVM_BINARY_SUBTRACT, d3
	cmpi.b #19, (a0)
	bne.w ok
operator
	addq.l #1, a0
	move.l d3, -(sp)
	bsr.w product
	move.l (sp)+, d1
	tst.l d0
	bne.w done
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d0
	bsr.w pair
	bne.w done
	subq.w #1, d7
	bra.w loop
ok
	moveq #STATUS_OK, d0
done
	rts
	.bend  ; sum

product	.block
	bsr.w unary
	tst.l d0
	bne.w done
loop
	cmpa.l a1, a0
	bhs.w ok
	cmpi.b #20, (a0)
	bne.w ok
	addq.l #1, a0
	bsr.w unary
	tst.l d0
	bne.w done
	moveq #runtime.EXPRVM_BINARY_MULTIPLY, d1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d0
	bsr.w pair
	bne.w done
	subq.w #1, d7
	bra.w loop
ok
	moveq #STATUS_OK, d0
done
	rts
	.bend  ; product

unary	.block
	cmpa.l a1, a0
	bhs.w malformed
	moveq #0, d3
	move.b (a0), d3
	cmpi.b #18, d3
	beq.w signed
	cmpi.b #19, d3
	bne.w primary
signed
	addq.l #1, a0
	addq.w #1, d6
	cmpi.w #MAX_DEPTH, d6
	bhi.w depth
	move.l d3, -(sp)
	bsr.w unary
	move.l (sp)+, d3
	subq.w #1, d6
	tst.l d0
	bne.w done
	cmpi.b #19, d3
	bne.w ok
	moveq #runtime.EXPRVM_UNARY_MINUS, d1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d0
	bra.w pair
ok
	moveq #STATUS_OK, d0
done
	rts
depth
	subq.w #1, d6
	moveq #STATUS_DEPTH, d0
	rts
malformed
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; unary

primary	.block
	cmpa.l a1, a0
	bhs.w malformed
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #2, d3
	beq.w literal
	cmpi.b #1, d3
	bls.w symbol
	cmpi.b #6, d3
	beq.w current
	cmpi.b #14, d3
	bne.w malformed
	addq.w #1, d6
	cmpi.w #MAX_DEPTH, d6
	bhi.w depth
	bsr.w sum
	subq.w #1, d6
	tst.l d0
	bne.w done
	cmpa.l a1, a0
	bhs.w malformed
	cmpi.b #15, (a0)+
	bne.w malformed
	moveq #STATUS_OK, d0
done
	rts
current
	moveq #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d0
	bsr.w emit
	bne.w done
	bra.w push
literal
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w malformed
	tst.b (a0)
	bmi.w malformed
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #9, d0
	blo.w output
	move.b #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, (a3)+
	move.b 3(a0), (a3)+
	move.b 2(a0), (a3)+
	move.b 1(a0), (a3)+
	move.b (a0), (a3)+
	clr.b (a3)+
	clr.b (a3)+
	clr.b (a3)+
	clr.b (a3)+
	addq.l #4, a0
	bra.w push
symbol
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w malformed
	tst.b 2(a0)
	bne.w malformed
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #3, d0
	blo.w output
	move.b #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, (a3)+
	move.b 1(a0), (a3)+
	move.b (a0), (a3)+
	addq.l #3, a0
	bra.w push
depth
	subq.w #1, d6
	moveq #STATUS_DEPTH, d0
	rts
output
	moveq #STATUS_OUTPUT, d0
	rts
malformed
	moveq #STATUS_MALFORMED, d0
	rts
	.bend  ; primary

push	.block
	addq.w #1, d7
	cmpi.w #runtime.EXPRVM_STACK_CAPACITY, d7
	bhi.w bad
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_DEPTH, d0
	rts
	.bend  ; push

; D0=opcode,D1=operator. D2 scratch; preserves D1. CCR=status.
pair	.block
	move.l a4, d2
	sub.l a3, d2
	cmpi.l #2, d2
	blo.w bad
	move.b d0, (a3)+
	move.b d1, (a3)+
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_OUTPUT, d0
	rts
	.bend  ; pair

; D0=byte to append; returns D0/CCR=status.
emit	.block
	cmpa.l a4, a3
	bhs.w bad
	move.b d0, (a3)+
	moveq #STATUS_OK, d0
	rts
bad
	moveq #STATUS_OUTPUT, d0
	rts
	.bend  ; emit
	.endsection
	.endmodule
