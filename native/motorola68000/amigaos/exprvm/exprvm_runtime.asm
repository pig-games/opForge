; Native ExprVM bytecode runtime for AmigaOS.
;
; Owns portable ExprVM bytecode execution and the small runtime state used by
; opcore expression bridge callers.

	.module exprvm.amigaos.runtime
	.cpu 68020
	.use exprvm.amigaos.i64_math as i64_math
	.pub
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	.use debug.amigaos.runtime_profile as runtime_profile
.endif

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
EXPRVM_BINARY_POWER             = 3
EXPRVM_BINARY_ADD               = 6
EXPRVM_BINARY_SUBTRACT          = 7
EXPRVM_BINARY_LOGIC_OR          = 8
EXPRVM_TERNARY_SELECT           = 9
EXPRVM_BINARY_MULTIPLY          = 10
EXPRVM_BINARY_DIVIDE            = 11
EXPRVM_BINARY_MOD               = 12
EXPRVM_BINARY_SHIFT_LEFT        = 13
EXPRVM_BINARY_SHIFT_RIGHT       = 14
EXPRVM_BINARY_EQ                = 15
EXPRVM_BINARY_NE                = 16
EXPRVM_BINARY_GE                = 17
EXPRVM_BINARY_GT                = 18
EXPRVM_BINARY_LE                = 19
EXPRVM_BINARY_LT                = 20
EXPRVM_BINARY_BIT_AND           = 21
EXPRVM_BINARY_BIT_OR            = 22
EXPRVM_BINARY_BIT_XOR           = 23
EXPRVM_BINARY_LOGIC_AND         = 24
EXPRVM_BINARY_LOGIC_XOR         = 25
EXPRVM_STACK_CAPACITY           = 8

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Evaluate one portable ExprVM bytecode program with signed i64 scalars.
;
; Inputs:
; - A0/D0: ExprVM bytecode pointer and byte length.
; - A1: fixed-width symbol-name table pointer.
; - A2: unsigned 32-bit symbol-value table pointer parallel to A1.
; - A6: byte-per-symbol stability table; nonzero means finalized.
; - D1: number of symbol entries.
; - D2: current assembly PC for PushCurrentAddress.
;
; Outputs:
; - D0: 0 on success, 1 on invalid program/evaluation failure.
; - D3: low 32 bits of the resolved scalar on success.
; - exprvmGetLastResultHighV1 exposes its high word after successful evaluation.
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
	.priv
	movem.l d1-d2/d6-d7/a0-a6, -(sp)
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_VM_EXPRVM, d0
	moveq #runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR, d1
	jsr runtime_profile.opforgeRuntimeProfileEnterVmV1
	movem.l (sp)+, d0-d1
.endif
	; Keep the symbol count independent of arithmetic scratch register D1.
	movea.l d1, a3
	clr.w ExprvmLastResultPresent
	clr.l ExprvmLastResultHigh
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	movem.l d0-d1, -(sp)
	moveq #runtime_profile.OPFORGE_RUNTIME_VM_EXPRVM, d0
	moveq #runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR, d1
	jsr runtime_profile.opforgeRuntimeProfileRecordOpcodeV1
	movem.l (sp)+, d0-d1
.endif
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
	bsr.w readI64
	bmi.w literalReadFail
	move.l d0, ExprvmEvalRemaining
	bsr.w pushD3
	bmi.w literalPushFail
	move.l ExprvmEvalRemaining, d0
	bra.w evalLoop

opcodePushCurrent
	moveq #0, d2
	move.l a5, d3
	move.l d0, ExprvmEvalRemaining
	bsr.w pushD3
	bmi.w fail
	move.l ExprvmEvalRemaining, d0
	bra.w evalLoop

opcodePushSymbol
	bsr.w readU16
	bmi.w fail
	cmpa.l d3, a3
	bls.w fail
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
	moveq #0, d2
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
	beq.w applyUnaryDone
	cmpi.b #EXPRVM_UNARY_MINUS, d6
	beq.w applyUnaryMinus
	cmpi.b #EXPRVM_UNARY_BIT_NOT, d6
	beq.w applyUnaryBitNot
	cmpi.b #EXPRVM_UNARY_LOGIC_NOT, d6
	beq.w applyUnaryLogicNot
	cmpi.b #EXPRVM_UNARY_HIGH, d6
	beq.w applyUnaryHigh
	cmpi.b #EXPRVM_UNARY_LOW, d6
	beq.w applyUnaryLow
	bra.w fail
applyUnaryMinus
	cmpi.l #$80000000, d2
	bne.s negate
	tst.l d3
	beq.w fail
negate
	neg.l d3
	negx.l d2
	bra.w applyUnaryDone
applyUnaryBitNot
	not.l d2
	not.l d3
	bra.w applyUnaryDone
applyUnaryLogicNot
	move.l d2, d0
	or.l d3, d0
	seq d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyUnaryDone
applyUnaryHigh
	lsr.l #8, d3
applyUnaryLow
	andi.l #$ff, d3
	moveq #0, d2
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
	movem.l d2-d3, -(sp)
	bsr.w popD3
	bmi.w applyBinaryRestoreFail
	movem.l (sp)+, d0-d1
	cmpi.b #EXPRVM_BINARY_ADD, d6
	beq.w applyBinaryAdd
	cmpi.b #EXPRVM_BINARY_SUBTRACT, d6
	beq.w applyBinarySubtract
	cmpi.b #EXPRVM_BINARY_LOGIC_OR, d6
	beq.w applyBinaryLogicOr
	cmpi.b #EXPRVM_BINARY_POWER, d6
	beq.w applyBinaryPower
	cmpi.b #EXPRVM_BINARY_MULTIPLY, d6
	beq.w applyBinaryMultiply
	cmpi.b #EXPRVM_BINARY_DIVIDE, d6
	beq.w applyBinaryDivide
	cmpi.b #EXPRVM_BINARY_MOD, d6
	beq.w applyBinaryMod
	cmpi.b #EXPRVM_BINARY_SHIFT_LEFT, d6
	beq.w applyBinaryShiftLeft
	cmpi.b #EXPRVM_BINARY_SHIFT_RIGHT, d6
	beq.w applyBinaryShiftRight
	cmpi.b #EXPRVM_BINARY_EQ, d6
	beq.w applyBinaryEq
	cmpi.b #EXPRVM_BINARY_NE, d6
	beq.w applyBinaryNe
	cmpi.b #EXPRVM_BINARY_GE, d6
	beq.w applyBinaryGe
	cmpi.b #EXPRVM_BINARY_GT, d6
	beq.w applyBinaryGt
	cmpi.b #EXPRVM_BINARY_LE, d6
	beq.w applyBinaryLe
	cmpi.b #EXPRVM_BINARY_LT, d6
	beq.w applyBinaryLt
	cmpi.b #EXPRVM_BINARY_BIT_AND, d6
	beq.w applyBinaryBitAnd
	cmpi.b #EXPRVM_BINARY_BIT_OR, d6
	beq.w applyBinaryBitOr
	cmpi.b #EXPRVM_BINARY_BIT_XOR, d6
	beq.w applyBinaryBitXor
	cmpi.b #EXPRVM_BINARY_LOGIC_AND, d6
	beq.w applyBinaryLogicAnd
	cmpi.b #EXPRVM_BINARY_LOGIC_XOR, d6
	beq.w applyBinaryLogicXor
	cmpi.b #EXPRVM_TERNARY_SELECT, d6
	beq.w applyTernarySelect
	bra.w fail
applyBinaryRestoreFail
	addq.l #8, sp
	bra.w fail
applyBinaryAdd
	add.l d1, d3
	addx.l d0, d2
	bra.w applyBinaryDone
applyBinarySubtract
	sub.l d1, d3
	subx.l d0, d2
	bra.w applyBinaryDone
applyBinaryMultiply
	jsr i64_math.multiplyV1
	bra.w applyBinaryDone
applyBinaryPower
	jsr i64_math.powerV1
	bne.w fail
	bra.w applyBinaryDone
applyBinaryDivide
	moveq #0, d6
	bra.s divideOrMod
applyBinaryMod
	moveq #1, d6
divideOrMod
	jsr i64_math.divideModuloV1
	bne.w fail
	bra.w applyBinaryDone
applyBinaryShiftLeft
	andi.l #31, d1
	beq.w applyBinaryDone
	moveq #32, d6
	sub.l d1, d6
	move.l d3, d0
	lsr.l d6, d0
	lsl.l d1, d2
	or.l d0, d2
	lsl.l d1, d3
	bra.w applyBinaryDone
applyBinaryShiftRight
	andi.l #31, d1
	beq.w applyBinaryDone
	moveq #32, d6
	sub.l d1, d6
	move.l d2, d0
	lsl.l d6, d0
	lsr.l d1, d3
	or.l d0, d3
	lsr.l d1, d2
	bra.w applyBinaryDone
applyBinaryEq
	bsr.w compareI64
	seq d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryNe
	bsr.w compareI64
	sne d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryGe
	bsr.w compareI64
	sge d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryGt
	bsr.w compareI64
	sgt d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryLe
	bsr.w compareI64
	sle d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryLt
	bsr.w compareI64
	slt d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryBitAnd
	and.l d0, d2
	and.l d1, d3
	bra.w applyBinaryDone
applyBinaryBitOr
	or.l d0, d2
	or.l d1, d3
	bra.w applyBinaryDone
applyBinaryBitXor
	eor.l d0, d2
	eor.l d1, d3
	bra.w applyBinaryDone
applyBinaryLogicOr
	or.l d0, d2
	or.l d1, d3
	or.l d2, d3
	sne d3
	andi.l #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryLogicAnd
	or.l d2, d3
	beq.s binaryFalse
	or.l d0, d1
	beq.s binaryFalse
	moveq #1, d3
	moveq #0, d2
	bra.w applyBinaryDone
binaryFalse
	moveq #0, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyBinaryLogicXor
	or.l d2, d3
	sne d3
	andi.l #1, d3
	or.l d0, d1
	sne d1
	andi.l #1, d1
	eor.l d1, d3
	moveq #0, d2
	bra.w applyBinaryDone
applyTernarySelect
	movem.l d0-d3, -(sp)
	bsr.w popD3
	bmi.s ternaryFail
	or.l d2, d3
	beq.s ternaryFalse
	movem.l 8(sp), d2-d3
	bra.s ternaryDone
ternaryFalse
	movem.l (sp), d2-d3
ternaryDone
	adda.l #16, sp
	bra.w applyBinaryDone
ternaryFail
	adda.l #16, sp
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
	move.l d2, ExprvmLastResultHigh
	move.w #1, ExprvmLastResultPresent
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileLeaveVmV1
.endif
	movem.l (sp)+, d1-d2/d6-d7/a0-a6
	rts
	.bend  ; exprvmEvalProgramV1

	.pub

; Read the high word of the most recent successful evaluation.
; Inputs: none. Outputs: D0=0/D1=high word, or D0=1/D1=0 if unavailable.
; Clobbers: D0-D1/CCR. CCR: reflects D0 on return.
exprvmGetLastResultHighV1	.block
	.priv
	moveq #0, d1
	tst.w ExprvmLastResultPresent
	beq.s missing
	move.l ExprvmLastResultHigh, d1
	moveq #0, d0
	rts
missing
	moveq #1, d0
	rts
	.bend  ; exprvmGetLastResultHighV1

; Compare signed i64 left D2:D3 with right D0:D1.
; Outputs: D0=-1/0/1 for less/equal/greater. Clobbers: D0/CCR.
; CCR: reflects the signed ordering in D0; all other input registers unchanged.
compareI64	.block
	cmp.l d0, d2
	blt.s less
	bgt.s greater
	cmp.l d1, d3
	blo.s less
	bhi.s greater
	moveq #0, d0
	rts
less
	moveq #-1, d0
	rts
greater
	moveq #1, d0
	rts
	.bend  ; compareI64

; Push signed pair D2:D3 into one of eight logical stack slots.
; Inputs: D2:D3=value, D7=depth. Outputs: D0=0/-1, D7 increments on success.
; Clobbers: D0/A2/CCR. CCR: reflects D0. Pair and operator register D6 survive.
pushD3	.block
	cmpi.l #EXPRVM_STACK_CAPACITY, d7
	bhs.s fail
	move.l d7, d0
	lsl.l #3, d0
	lea ExprvmStack, a2
	move.l d2, 0(a2, d0.l)
	move.l d3, 4(a2, d0.l)
	addq.l #1, d7
	moveq #0, d0
	rts
fail
	moveq #-1, d0
	rts
	.bend  ; pushD3

; Pop one signed pair into D2:D3.
; Inputs: D7=depth. Outputs: D0=0/-1, D2:D3=value and D7 decrements on success.
; Clobbers: D0/D2-D3/A2/CCR. CCR: reflects D0. Operator register D6 survives.
popD3	.block
	tst.l d7
	beq.s fail
	subq.l #1, d7
	move.l d7, d0
	lsl.l #3, d0
	lea ExprvmStack, a2
	move.l 0(a2, d0.l), d2
	move.l 4(a2, d0.l), d3
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

; Read one little-endian unsigned 16-bit value from the bytecode stream.
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

; Read both little-endian words of an i64 literal.
; Inputs: A0=cursor,D0=remaining. Outputs: D2:D3=high:low,D0=remaining or -1.
; Clobbers: D2-D3/A0/CCR. CCR: reflects D0. Failure consumes no bytes.
readI64	.block
	cmpi.l #8, d0
	bcs.s fail
	bsr.s readLiteralLong
	move.l d3, -(sp)
	bsr.s readLiteralLong
	move.l d3, d2
	move.l (sp)+, d3
	tst.l d0
	rts
fail
	moveq #-1, d0
	rts
	.bend  ; readI64

; Read a little-endian long after readI64 proves eight available bytes.
; Inputs: A0=cursor,D0>=4. Outputs: D3=value,D0 reduced by four,A0 advanced.
; Clobbers: D2-D3/A0/CCR. CCR: reflects D0.
readLiteralLong	.block
	moveq #0, d3
	move.b 3(a0), d3
	lsl.l #8, d3
	move.b 2(a0), d3
	lsl.l #8, d3
	move.b 1(a0), d3
	lsl.l #8, d3
	move.b (a0), d3
	addq.l #4, a0
	subq.l #4, d0
	rts
	.bend  ; readLiteralLong

	.priv

	.endsection

	.section bss, kind=bss

	.pub

ExprvmStack
	.res long, EXPRVM_STACK_CAPACITY*2
ExprvmSelectedOpcodeVersion
	.res word, 1
ExprvmCurrentPass
	.res word, 1
ExprvmEvalRemaining
	.res long, 1
	.priv
ExprvmLastResultHigh
	.res long, 1
ExprvmLastResultPresent
	.res word, 1

	.endsection
	.endmodule
