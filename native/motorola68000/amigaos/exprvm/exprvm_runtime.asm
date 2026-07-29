; Native ExprVM bytecode runtime for AmigaOS.
;
; Owns portable ExprVM bytecode execution and the small runtime state used by
; opcore expression bridge callers.

	.module exprvm.amigaos.runtime
	.cpu 68020
	.pub

EXPRVM_OPCODE_END               = $00
EXPRVM_OPCODE_PUSH_LITERAL      = $01
EXPRVM_OPCODE_PUSH_CURRENT_ADDR = $02
EXPRVM_OPCODE_PUSH_SYMBOL       = $03
EXPRVM_OPCODE_APPLY_UNARY       = $04
EXPRVM_OPCODE_APPLY_BINARY      = $05
EXPRVM_V2_OPCODE_END            = $00
EXPRVM_V2_OPCODE_PUSH_LITERAL   = $10
EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR = $11
EXPRVM_V2_OPCODE_PUSH_SYMBOL    = $12
EXPRVM_V2_OPCODE_APPLY_UNARY    = $20
EXPRVM_V2_OPCODE_APPLY_BINARY   = $21
EXPRVM_V2_OPCODE_REQUIRE_SCALAR = $70
EXPRVM_UNARY_PLUS               = 0
EXPRVM_UNARY_MINUS              = 1
EXPRVM_UNARY_BIT_NOT            = 2
EXPRVM_UNARY_LOGIC_NOT          = 3
EXPRVM_UNARY_HIGH               = 4
EXPRVM_UNARY_LOW                = 5
EXPRVM_BINARY_ADD               = 6
EXPRVM_BINARY_SUBTRACT          = 7
EXPRVM_BINARY_LOGIC_OR          = 8
EXPRVM_TERNARY_SELECT           = 9
EXPRVM_BINARY_MULTIPLY          = 10
EXPRVM_BINARY_DIVIDE            = 11
EXPRVM_BINARY_MOD               = 12
EXPRVM_BINARY_SHIFT_LEFT        = 13
EXPRVM_BINARY_SHIFT_RIGHT       = 14
EXPRVM_STACK_CAPACITY           = 8

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Evaluate one portable ExprVM bytecode program for the native 6502 scalar
; first-run subset.
;
; Inputs:
; - A0/D0: ExprVM bytecode pointer and byte length.
; - A1: fixed-width symbol-name table pointer.
; - A2: symbol-value table pointer parallel to A1.
; - D1: number of symbol entries.
; - D2: current assembly PC for PushCurrentAddress.
;
; Outputs:
; - D0: 0 on success, 1 on invalid program/evaluation failure.
; - D3: resolved scalar value on success.
; - D4: nonzero when the program referenced at least one symbol.
; - D5: nonzero when the program referenced a symbol that is unstable for the
;   current pass.
;
; Clobbers:
; - D0/D3-D5/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
exprvmEvalProgramV1	.block
	movem.l d1-d2/d6-d7/a0-a6, -(sp)
	movea.l a1, a3
	movea.l a2, a4
	movea.l d2, a5
	clr.l d3
	clr.l d4
	clr.l d5
	clr.l d7

evalLoop
	tst.l d0
	beq.w missingEnd
	moveq #0, d6
	move.b (a0)+, d6
	subq.l #1, d0
	moveq #0, d2
	move.w ExprvmSelectedOpcodeVersion, d2
	cmpi.w #2, d2
	beq.s evalLoopV2
	cmpi.b #EXPRVM_OPCODE_END, d6
	beq.w opcodeEnd
	cmpi.b #EXPRVM_OPCODE_PUSH_LITERAL, d6
	beq.w opcodePushLiteral
	cmpi.b #EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d6
	beq.w opcodePushCurrent
	cmpi.b #EXPRVM_OPCODE_PUSH_SYMBOL, d6
	beq.w opcodePushSymbol
	cmpi.b #EXPRVM_OPCODE_APPLY_UNARY, d6
	beq.w opcodeApplyUnary
	cmpi.b #EXPRVM_OPCODE_APPLY_BINARY, d6
	beq.w opcodeApplyBinary
	bra.w unknownOpcode

evalLoopV2
	cmpi.b #EXPRVM_V2_OPCODE_END, d6
	beq.w opcodeEnd
	cmpi.b #EXPRVM_V2_OPCODE_PUSH_LITERAL, d6
	beq.w opcodePushLiteral
	cmpi.b #EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d6
	beq.w opcodePushCurrent
	cmpi.b #EXPRVM_V2_OPCODE_PUSH_SYMBOL, d6
	beq.w opcodePushSymbol
	cmpi.b #EXPRVM_V2_OPCODE_APPLY_UNARY, d6
	beq.w opcodeApplyUnary
	cmpi.b #EXPRVM_V2_OPCODE_APPLY_BINARY, d6
	beq.w opcodeApplyBinary
	cmpi.b #EXPRVM_V2_OPCODE_REQUIRE_SCALAR, d6
	beq.w opcodeRequireScalar
	bra.w unknownOpcode

opcodePushLiteral
	bsr.w readI64Low32
	bmi.w literalReadFail
	move.l d0, ExprvmEvalRemaining
	bsr.w pushD3
	bmi.w literalPushFail
	move.l ExprvmEvalRemaining, d0
	bra.w evalLoop

opcodePushCurrent
	move.l a5, d3
	move.l d0, ExprvmEvalRemaining
	bsr.w pushD3
	bmi.w fail
	move.l ExprvmEvalRemaining, d0
	bra.w evalLoop

opcodePushSymbol
	bsr.w readU16
	bmi.w fail
	cmp.w d1, d3
	bhs.w fail
	moveq #1, d4
	moveq #0, d6
	move.w d3, d6
	tst.b 0(a6, d6.l)
	bne.s pushSymbolStable

pushSymbolUnstable
	moveq #1, d5

pushSymbolStable
	move.l d0, ExprvmEvalRemaining
	moveq #0, d6
	move.w d3, d6
	lsl.l #2, d6
	movea.l a4, a2
	move.l 0(a2, d6.l), d3
	bsr.w pushD3
	bmi.w fail
	move.l ExprvmEvalRemaining, d0
	bra.w evalLoop

opcodeApplyUnary
	bsr.w readU8
	bmi.w fail
	move.l d0, ExprvmEvalRemaining
	move.l d3, d6
	bsr.w popD3
	bmi.w fail
	cmpi.b #EXPRVM_UNARY_PLUS, d6
	beq.s applyUnaryDone
	cmpi.b #EXPRVM_UNARY_MINUS, d6
	beq.s applyUnaryMinus
	cmpi.b #EXPRVM_UNARY_BIT_NOT, d6
	beq.s applyUnaryBitNot
	cmpi.b #EXPRVM_UNARY_LOGIC_NOT, d6
	beq.s applyUnaryLogicNot
	cmpi.b #EXPRVM_UNARY_HIGH, d6
	beq.s applyUnaryHigh
	cmpi.b #EXPRVM_UNARY_LOW, d6
	beq.s applyUnaryLow
	bra.w fail

applyUnaryMinus
	neg.l d3
	bra.s applyUnaryDone

applyUnaryBitNot
	not.l d3
	bra.s applyUnaryDone

applyUnaryLogicNot
	tst.l d3
	beq.s applyUnaryLogicNotTrue
	clr.l d3
	bra.s applyUnaryDone

applyUnaryLogicNotTrue
	moveq #1, d3
	bra.s applyUnaryDone

applyUnaryHigh
	lsr.l #8, d3
	andi.l #$000000FF, d3
	bra.s applyUnaryDone

applyUnaryLow
	andi.l #$000000FF, d3

applyUnaryDone
	bsr.w pushD3
	bmi.w fail
	move.l ExprvmEvalRemaining, d0
	bra.w evalLoop

opcodeApplyBinary
	bsr.w readU8
	bmi.w fail
	move.l d0, ExprvmEvalRemaining
	move.l d3, d6
	bsr.w popD3
	bmi.w fail
	move.l d3, -(sp)
	bsr.w popD3
	bmi.s applyBinaryRestoreFail
	move.l (sp)+, d2
	cmpi.b #EXPRVM_BINARY_ADD, d6
	beq.s applyBinaryAdd
	cmpi.b #EXPRVM_BINARY_SUBTRACT, d6
	beq.s applyBinarySubtract
	cmpi.b #EXPRVM_BINARY_LOGIC_OR, d6
	beq.s applyBinaryLogicOr
	cmpi.b #EXPRVM_TERNARY_SELECT, d6
	beq.s applyTernarySelect
	cmpi.b #EXPRVM_BINARY_MULTIPLY, d6
	beq.s applyBinaryMultiply
	cmpi.b #EXPRVM_BINARY_DIVIDE, d6
	beq.s applyBinaryDivide
	cmpi.b #EXPRVM_BINARY_MOD, d6
	beq.s applyBinaryMod
	cmpi.b #EXPRVM_BINARY_SHIFT_LEFT, d6
	beq.s applyBinaryShiftLeft
	cmpi.b #EXPRVM_BINARY_SHIFT_RIGHT, d6
	beq.s applyBinaryShiftRight
	bra.w fail

applyBinaryRestoreFail
	addq.l #4, sp
	bra.w fail

applyBinaryAdd
	add.l d2, d3
	bra.s applyBinaryDone

applyBinarySubtract
	sub.l d2, d3
	bra.s applyBinaryDone

applyBinaryLogicOr
	or.l d2, d3
	beq.s applyBinaryLogicOrDone
	moveq #1, d3

applyBinaryLogicOrDone
	bra.s applyBinaryDone

applyBinaryMultiply
	muls.l d2, d3
	bra.s applyBinaryDone

applyBinaryDivide
	tst.l d2
	beq.w fail
	move.l d3, d1
	divs.l d2, d1
	move.l d1, d3
	bra.s applyBinaryDone

applyBinaryMod
	tst.l d2
	beq.w fail
	move.l d2, d6
	move.l d3, d1
	swap d3
	ext.l d3
	divs.l d6, d3:d1
	bra.s applyBinaryDone

applyBinaryShiftLeft
	andi.l #31, d2
	lsl.l d2, d3
	bra.s applyBinaryDone

applyBinaryShiftRight
	andi.l #31, d2
	lsr.l d2, d3
	bra.s applyBinaryDone

applyTernarySelect
	move.l d3, -(sp)
	bsr.w popD3
	bmi.s applyTernaryRestoreFail
	tst.l d3
	move.l (sp)+, d3
	bne.s applyBinaryDone
	move.l d2, d3
	bra.s applyBinaryDone

applyTernaryRestoreFail
	addq.l #4, sp
	bra.w fail

applyBinaryDone
	bsr.w pushD3
	bmi.w fail
	move.l ExprvmEvalRemaining, d0
	bra.w evalLoop

opcodeRequireScalar
	cmpi.l #1, d7
	bne.w requireScalarFail
	bra.w evalLoop

opcodeEnd
	cmpi.l #1, d7
	bne.w endStackFail
	bsr.w popD3
	bmi.w popFail
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0
	bra.s return

missingEnd
	moveq #51, d0
	bra.s return

unknownOpcode
	moveq #52, d0
	bra.s return

literalReadFail
	moveq #53, d0
	bra.s return

literalPushFail
	moveq #54, d0
	bra.s return

requireScalarFail
	moveq #55, d0
	bra.s return

endStackFail
	moveq #56, d0
	bra.s return

popFail
	moveq #57, d0

return
	movem.l (sp)+, d1-d2/d6-d7/a0-a6
	rts
	.bend  ; exprvmEvalProgramV1

; Push D3 onto the private ExprVM value stack.
; Inputs: D3 = value to push; D7 = current stack depth.
; Outputs: D0 = 0 on success or -1 on overflow; D7 incremented on success.
; Clobbers: D2/A2.
; CCR: reflects D0 on return.
pushD3	.block
	cmpi.l #EXPRVM_STACK_CAPACITY, d7
	bhs.s fail
	move.l d7, d2
	lsl.l #2, d2
	lea ExprvmStack, a2
	move.l d3, 0(a2, d2.l)
	addq.l #1, d7
	moveq #0, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; pushD3

; Pop the private ExprVM value stack into D3.
; Inputs: D7 = current stack depth.
; Outputs: D0 = 0 on success or -1 on underflow; D3 = popped value on success;
; D7 decremented on success.
; Clobbers: D2/A2.
; CCR: reflects D0 on return.
popD3	.block
	tst.l d7
	beq.s fail
	subq.l #1, d7
	move.l d7, d2
	lsl.l #2, d2
	lea ExprvmStack, a2
	move.l 0(a2, d2.l), d3
	moveq #0, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; popD3

; Read one unsigned byte from the bytecode stream.
; Inputs: A0 = bytecode cursor; D0 = remaining byte count.
; Outputs: D0 = remaining byte count after consume or -1 on underflow; D3 =
; zero-extended byte value on success; A0 advanced by 1 on success.
; Clobbers: CCR.
; CCR: reflects D0 on return.
readU8	.block
	tst.l d0
	beq.s fail
	moveq #0, d3
	move.b (a0)+, d3
	subq.l #1, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; readU8

; Read one big-endian unsigned 16-bit value from the bytecode stream.
; Inputs: A0 = bytecode cursor; D0 = remaining byte count.
; Outputs: D0 = remaining byte count after consume or -1 on underflow; D3 =
; zero-extended 16-bit value on success; A0 advanced by 2 on success.
; Clobbers: D2/CCR.
; CCR: reflects D0 on return.
readU16	.block
	cmpi.l #2, d0
	bcs.s fail
	moveq #0, d3
	move.b (a0)+, d3
	moveq #0, d2
	move.b (a0)+, d2
	lsl.w #8, d2
	or.w d2, d3
	subq.l #2, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; readU16

; Read the low 32 bits of one big-endian 64-bit literal from the bytecode stream.
; Inputs: A0 = bytecode cursor; D0 = remaining byte count.
; Outputs: D0 = remaining byte count after consume or -1 on underflow; D3 =
; low 32 literal bits on success; A0 advanced by 8 on success.
; Clobbers: D2/CCR.
; CCR: reflects D0 on return.
readI64Low32	.block
	cmpi.l #8, d0
	bcs.s fail
	moveq #0, d3
	move.b (a0)+, d3
	moveq #0, d2
	move.b (a0)+, d2
	lsl.l #8, d2
	or.l d2, d3
	moveq #0, d2
	move.b (a0)+, d2
	lsl.l #8, d2
	lsl.l #8, d2
	or.l d2, d3
	moveq #0, d2
	move.b (a0)+, d2
	lsl.l #8, d2
	lsl.l #8, d2
	lsl.l #8, d2
	or.l d2, d3
	addq.l #4, a0
	subq.l #8, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; readI64Low32

	.priv

	.endsection

	.section bss, kind=bss

	.pub

ExprvmStack
	.res long, EXPRVM_STACK_CAPACITY
ExprvmSelectedOpcodeVersion
	.res word, 1
ExprvmCurrentPass
	.res word, 1
ExprvmEvalRemaining
	.res long, 1

	.endsection
	.endmodule
