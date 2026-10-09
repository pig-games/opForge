; Shared scalar compound-assignment arithmetic, matching asmline_eval.rs.
; @opforge-owner: exprvm.amigaos.assignment
	.module exprvm.amigaos.assignment
	.cpu 68020
	.use exprvm.amigaos.runtime as runtime
	.pub

ADD = 0
SUB = 1
MUL = 2
DIV = 3
MOD = 4
POWER = 5
BIT_OR = 6
BIT_XOR = 7
BIT_AND = 8
LOGIC_OR = 9
LOGIC_AND = 10
SHL = 11
SHR = 12
CONCAT = 13
MIN = 14
MAX = 15
MEMBER = 16
OK = 0
ERROR = 1

	.priv
Literal	.struct
Opcode	.byte ?
Low	.long ?
High	.long ?
	.endstruct
LITERAL_BYTES = Literal.High+4
Program	.struct
Left	.res LITERAL_BYTES
Right	.res LITERAL_BYTES
Apply	.byte ?
Operator	.byte ?
End	.byte ?
	.endstruct
PROGRAM_BYTES = Program.End+1
FRAME_BYTES = (PROGRAM_BYTES+1)&$fffffffe

	.section code, kind=code
	.pub

; Apply one scalar assignment action. Inputs: D0=action, D2:D1=left,
; D4:D3=right. Outputs: D0=OK/ERROR, D2:D1=result on success.
; Clobbers: D0-D2/CCR. CCR reflects D0. Uses a temporary stack evalProgram
; and the shared runtime's last-result state; arithmetic is signed i64,
; bit/logic/shift/append operands and results are zero-extended u32.
apply	.block
	.priv
	movem.l d3-d7/a0-a6, -(sp)
	move.l d0, d7
	cmpi.l #MEMBER, d7
	bhi.w failure
	beq.w right
	cmpi.l #MIN, d7
	bhs.w minMax
	cmpi.l #CONCAT, d7
	beq.w append
	cmpi.l #POWER, d7
	bne.w domain
	tst.l d4
	bne.w failure
	cmpi.l #63, d3
	bhi.w failure
domain
	cmpi.l #BIT_OR, d7
	blo.w evalProgram
	moveq #0, d2
	moveq #0, d4
evalProgram
	suba.l #FRAME_BYTES, sp
	movea.l sp, a3
	lea Program.Left(a3), a0
	move.l d1, d5
	move.l d2, d6
	bsr.w emitLiteral
	lea Program.Right(a3), a0
	move.l d3, d5
	move.l d4, d6
	bsr.w emitLiteral
	move.b #runtime.EXPRVM_OPCODE_APPLY_BINARY, Program.Apply(a3)
	lea Operators, a0
	move.b 0(a0, d7.w), Program.Operator(a3)
	move.b #runtime.EXPRVM_OPCODE_END, Program.End(a3)
	movea.l a3, a0
	moveq #PROGRAM_BYTES, d0
	moveq #0, d1
	moveq #0, d2
	suba.l a2, a2
	suba.l a6, a6
	jsr runtime.evalNumeric64
	adda.l #FRAME_BYTES, sp
	tst.l d0
	bne.w failure
	move.l d3, d6
	jsr runtime.exprvmGetLastResultHighV1
	bne.w failure
	move.l d1, d2
	move.l d6, d1
	cmpi.l #BIT_OR, d7
	blo.w success
	moveq #0, d2
	bra.w success
minMax
	cmp.l d4, d2
	blt.w less
	bgt.w greater
	cmp.l d3, d1
	blo.w less
	bhi.w greater
	bra.w success
less
	cmpi.l #MIN, d7
	beq.w success
	bra.w right
greater
	cmpi.l #MAX, d7
	beq.w success
right
	move.l d3, d1
	move.l d4, d2
	bra.w success
append
	; Four-byte RHS discards the LHS completely; hardware shift-by-32
	; must not become shift-by-zero as it does for assignment shifts.
	cmpi.l #$ffffff, d3
	bhi.w appendRight
	moveq #24, d5
	cmpi.l #$ffff, d3
	bhi.w appendShift
	moveq #16, d5
	cmpi.l #$ff, d3
	bhi.w appendShift
	moveq #8, d5
appendShift
	lsl.l d5, d1
	or.l d3, d1
	bra.w appendDone
appendRight
	move.l d3, d1
appendDone
	moveq #0, d2
success
	moveq #OK, d0
	bra.w done
failure
	moveq #ERROR, d0
done
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend  ; apply

; Emit one canonical literal. Inputs: A0=Literal slot, D6:D5=value.
; Clobbers: D0/D5-D6/A0/CCR. Payload is little-endian regardless of host.
emitLiteral	.block
	move.b #runtime.EXPRVM_OPCODE_PUSH_LITERAL, Literal.Opcode(a0)
	lea Literal.Low(a0), a0
	moveq #3, d0
low
	move.b d5, (a0)+
	lsr.l #8, d5
	dbra d0, low
	moveq #3, d0
high
	move.b d6, (a0)+
	lsr.l #8, d6
	dbra d0, high
	rts
	.bend  ; emitLiteral

Operators
	.byte runtime.EXPRVM_BINARY_ADD, runtime.EXPRVM_BINARY_SUBTRACT
	.byte runtime.EXPRVM_BINARY_MULTIPLY, runtime.EXPRVM_BINARY_DIVIDE
	.byte runtime.EXPRVM_BINARY_MOD, runtime.EXPRVM_BINARY_POWER
	.byte runtime.EXPRVM_BINARY_BIT_OR, runtime.EXPRVM_BINARY_BIT_XOR
	.byte runtime.EXPRVM_BINARY_BIT_AND, runtime.EXPRVM_BINARY_LOGIC_OR
	.byte runtime.EXPRVM_BINARY_LOGIC_AND, runtime.EXPRVM_BINARY_SHIFT_LEFT
	.byte runtime.EXPRVM_BINARY_SHIFT_RIGHT
	.align 2
	.endsection
	.endmodule
