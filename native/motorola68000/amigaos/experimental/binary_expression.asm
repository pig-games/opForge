; Numeric expression evaluator for experimental binary source records.
; @opforge-owner: opasm.amigaos.binary_expression

	.module opasm.amigaos.binary_expression
	.cpu 68020
	.include "telemetry_macros.i"

MAX_DEPTH = 16

TOKEN_SYMBOL_0 = 0
TOKEN_SYMBOL_1 = 1
TOKEN_LITERAL = 2
TOKEN_COMMA = 4
TOKEN_CURRENT_PC = 6
TOKEN_OPEN_PAREN = 14
TOKEN_CLOSE_PAREN = 15
TOKEN_PLUS = 18
TOKEN_MINUS = 19

STATUS_OK = 0
STATUS_MALFORMED = 1
STATUS_OVERFLOW = 2

Frame	.struct
Values	.long ?
Defined	.long ?
Count	.long ?
Pc	.long ?
.endstruct

	.section code, kind=code
	.pub

; Evaluate one numeric expression without consuming a comma, closing
; parenthesis, or the bounded end cursor.
; Inputs: A0=token cursor, A1=bounded end, A2=Frame.
; Outputs: D0=status, D1=i32 value, D2=nonzero when any symbol is unresolved;
;          A0 advanced to the first terminator.
; Preserves: D3-D7/A1-A6. CCR reflects D0.
evaluate	.block
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_EXPRESSION
	movem.l d3-d7/a1-a6, -(sp)
	moveq #0, d6
	bsr.w expression
	movem.l (sp)+, d3-d7/a1-a6
	.TELEMETRY_SERVICE_LEAVE
	tst.l d0
	rts
	.bend  ; evaluate

	.priv

; Parse a non-empty left-associative addition/subtraction expression.
expression	.block
	bsr.w unary
	tst.l d0
	bne.w return
	move.l d1, d4
	move.l d2, d5
loop
	cmpa.l a1, a0
	bhs.w ok
	moveq #0, d7
	move.b (a0), d7
	cmpi.b #TOKEN_PLUS, d7
	beq.w operator
	cmpi.b #TOKEN_MINUS, d7
	bne.w ok
operator
	addq.l #1, a0
	movem.l d4-d5/d7, -(sp)
	bsr.w unary
	movem.l (sp)+, d4-d5/d7
	tst.l d0
	bne.w return
	or.l d2, d5
	cmpi.b #TOKEN_PLUS, d7
	bne.w subtract
	add.l d1, d4
	bvs.w overflow
	bra.w loop
subtract
	sub.l d1, d4
	bvs.w overflow
	bra.w loop
ok
	move.l d4, d1
	move.l d5, d2
	moveq #STATUS_OK, d0
return
	rts
overflow
	moveq #STATUS_OVERFLOW, d0
	rts
	.bend  ; expression

; Parse unary plus/minus followed by a primary.
unary	.block
	cmpa.l a1, a0
	bhs.w malformed
	moveq #0, d3
	move.b (a0), d3
	cmpi.b #TOKEN_PLUS, d3
	beq.w signed
	cmpi.b #TOKEN_MINUS, d3
	bne.w primary
signed
	addq.l #1, a0
	move.w d3, -(sp)
	bsr.w unary
	move.w (sp)+, d3
	tst.l d0
	bne.w return
	cmpi.b #TOKEN_MINUS, d3
	bne.w return
	neg.l d1
	bvs.w overflow
	moveq #STATUS_OK, d0
return
	rts
malformed
	moveq #STATUS_MALFORMED, d0
	rts
overflow
	moveq #STATUS_OVERFLOW, d0
	rts
	.bend  ; unary

; Parse literal, symbol, current-PC, or one parenthesized expression.
primary	.block
	cmpa.l a1, a0
	bhs.w malformed
	moveq #0, d3
	move.b (a0)+, d3
	cmpi.b #TOKEN_LITERAL, d3
	beq.w literal
	cmpi.b #TOKEN_SYMBOL_0, d3
	beq.w symbol
	cmpi.b #TOKEN_SYMBOL_1, d3
	beq.w symbol
	cmpi.b #TOKEN_CURRENT_PC, d3
	beq.w currentPc
	cmpi.b #TOKEN_OPEN_PAREN, d3
	beq.w group
	bra.w unknown

currentPc
	; The token kind byte was consumed by the common primary dispatch.
	move.l Frame.Pc(a2), d1
	bmi.w malformed
	moveq #0, d2
	moveq #STATUS_OK, d0
	rts

group
	addq.w #1, d6
	cmpi.w #MAX_DEPTH, d6
	bhi.w groupMalformed
	bsr.w expression
	subq.w #1, d6
	tst.l d0
	bne.w return
	cmpa.l a1, a0
	bhs.w malformed
	cmpi.b #TOKEN_CLOSE_PAREN, (a0)
	bne.w malformed
	addq.l #1, a0
	rts
groupMalformed
	subq.w #1, d6
malformed
	moveq #STATUS_MALFORMED, d0
return
	rts
unknown
	; A delimiter or unsupported token is not part of this expression.
	subq.l #1, a0
	bra.w malformed

literal
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w malformed
	; The writer transports u32 literals; this evaluator supports signed i32.
	; Reject high-bit literals instead of silently reinterpreting them as negative.
	tst.b (a0)
	bmi.w malformed
	moveq #0, d1
	move.b (a0)+, d1
	lsl.l #8, d1
	move.b (a0)+, d1
	lsl.l #8, d1
	move.b (a0)+, d1
	lsl.l #8, d1
	move.b (a0)+, d1
	moveq #0, d2
	moveq #STATUS_OK, d0
	rts

symbol
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w malformed
	moveq #0, d3
	move.b (a0)+, d3
	lsl.w #8, d3
	move.b (a0)+, d3
	tst.b (a0)+
	bne.w malformed
	moveq #0, d0
	move.w d3, d0
	cmp.l Frame.Count(a2), d0
	bhs.w malformed
	movea.l Frame.Defined(a2), a3
	tst.b 0(a3, d0.l)
	beq.w unresolved
	movea.l Frame.Values(a2), a3
	lsl.l #2, d0
	move.l 0(a3, d0.l), d1
	bmi.w malformed  ; symbol values in this subset are nonnegative addresses
	moveq #0, d2
	moveq #STATUS_OK, d0
	rts
unresolved
	moveq #0, d1
	moveq #1, d2
	moveq #STATUS_OK, d0
	rts
	.bend  ; primary

	.endsection
	.endmodule
