; Native opcore/EXVM-style scalar operand expression bridge.

	.module opcore.amigaos.expr_bridge
	.cpu 68020
	.pub
	.use exprvm.amigaos.runtime

TOKEN_BUFFER_CAPACITY           = 64
OPCORE_EXPRVM_PROGRAM_CAPACITY  = 128
EXVM_OPCODE_END                 = $00
EXVM_OPCODE_PARSE_EXPRESSION    = $01
EXVM_OPCODE_EMIT_DIAG           = $02
EXVM_OPCODE_FAIL                = $03

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Evaluate one scalar operand expression through the native EXVM default path.
;
; opcoreExprEvalOperandV1 is the compatibility entry used by the current
; native selector/pass code. It now runs the same EXVM default program shape as
; the Rust path (`ParseExpression`, `End`) by compiling the current native
; scalar subset into ExprVM bytecode and then executing the ExprVM evaluator.
;
; Supported input forms:
; - optional immediate prefix '#'
; - '$' and '0x' prefixed hexadecimal numbers
; - '%' prefixed binary numbers
; - decimal numbers
; - unary '+' and '-'
; - additive/subtractive expressions over scalar terms
; - '$' current-address terms matching the portable VM syntax
; - '*' current-address terms accepted as compatibility syntax
; - labels resolved through the caller-supplied label tables
;
; Inputs:
; - A0/D0: operand text pointer and byte length.
; - A1: fixed-width label-name table pointer.
; - A2: label-value table pointer parallel to A1.
; - D1: number of label entries.
; - D2: current assembly PC for '$' current-address terms.
; - D4: EXVM parser opcode version.
; - D5: expression evaluator opcode version (1 or 2).
; - D6: current assembler pass number.
;
; Outputs:
; - D0: 0 on success, 1 on parse/lookup failure.
; - D3: resolved scalar value on success.
; - D4: nonzero when the evaluated program referenced at least one symbol.
; - D5: nonzero when the evaluated program referenced a symbol that is
;   unstable for the current pass.
;
; Clobbers:
; - D0/D3-D5/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
opcoreExprEvalOperandV1	.block
	moveq #1, d4
	bra.w opcoreExvmEvalOperandV1
	.bend  ; opcoreExprEvalOperandV1

opcoreExvmEvalOperandV1	.block
	move.w d4, OpcoreExvmSelectedOpcodeVersion
	movem.l d1-d2/d6-d7/a0-a5, -(sp)
	cmpi.w #2, d5
	beq.s selectedVersionReady
	moveq #1, d5

selectedVersionReady
	move.w d5, runtime.ExprvmSelectedOpcodeVersion
	clr.l d5
	move.w d6, runtime.ExprvmCurrentPass
	movea.l a1, a3  ; label-name table base kept stable across parse helpers
	movea.l a2, a4  ; label-value table base kept stable across parse helpers
	movea.l d2, a5  ; A5 carries the current PC for '*' terms
	move.w d1, d7  ; D7 is the label count consumed by the resolver loop
	clr.l d3
	bsr.w skipWhitespace
	tst.l d0
	beq.w fail
	cmpi.b #'#', (a0)
	bne.s noImmediatePrefix
	addq.l #1, a0  ; strip immediate marker; addressing mode was selected elsewhere
	subq.l #1, d0  ; keep remaining text length aligned with A0
	bsr.w skipWhitespace

noImmediatePrefix
	tst.l d0
	beq.w fail
	move.l d0, -(sp)
	bsr.w selectProgram
	move.l d0, d2
	move.l (sp)+, d0
	tst.l d2
	bne.w fail
	bsr.w runEvalProgram
	bra.w return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d6-d7/a0-a5
	rts
	.bend  ; opcoreExvmEvalOperandV1

selectProgram	.block
	moveq #0, d0
	move.w OpcoreExvmSelectedOpcodeVersion, d6
	cmpi.w #1, d6
	beq.s version1
	moveq #1, d0
	rts

version1
	lea OpcoreExvmDefaultProgram(PC), a1
	moveq #OPCORE_EXVM_DEFAULT_PROGRAM_LEN, d1
	rts
	.bend  ; selectProgram

runEvalProgram	.block
	moveq #0, d4  ; output expression count, matching Rust's stack-depth 1 contract

loop
	tst.l d1
	beq.w missingEnd
	moveq #0, d6
	move.b (a1)+, d6
	subq.l #1, d1
	cmpi.b #EXVM_OPCODE_END, d6
	beq.w opcodeEnd
	cmpi.b #EXVM_OPCODE_PARSE_EXPRESSION, d6
	beq.w opcodeParseExpression
	cmpi.b #EXVM_OPCODE_EMIT_DIAG, d6
	beq.w programFail
	cmpi.b #EXVM_OPCODE_FAIL, d6
	beq.w programFail
	bra.w programFail

opcodeParseExpression
	tst.l d4
	bne.w programFail
	movem.l d1/d4/a1, -(sp)
	bsr.w compileAdditive
	move.l d0, d2
	beq.s compileOk
	cmpi.l #2, d2
	bhs.s restore
	moveq #3, d2
	bra.s restore

compileOk
	bsr.w finalizeProgram
	move.l d0, d2
	beq.s finalizeOk
	moveq #4, d2
	bra.s restore

finalizeOk
	moveq #runtime.EXPRVM_OPCODE_END, d6
	bsr.w emitU8D6
	move.l d0, d2
	beq.s ensureEndOk
	moveq #4, d2
	bra.s restore

ensureEndOk
	lea OpcoreExprVmProgramBuffer, a0
	moveq #0, d0
	move.w OpcoreExprVmProgramLen, d0
	movea.l a3, a1
	movea.l a4, a2
	move.l d7, d1
	move.l a5, d2
	moveq #0, d6
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	jsr runtime.exprvmEvalProgramV1
	move.l d0, d2
	beq.s restore
	cmpi.l #51, d2
	bhs.s restore
	moveq #5, d2

restore
	movem.l (sp)+, d1/d4/a1
	tst.l d2
	bne.w failWithCode
	addq.l #1, d4
	bra.w loop

opcodeEnd
	cmpi.l #1, d4
	bne.s programFail
	moveq #0, d0
	rts

missingEnd
programFail
	moveq #1, d0
	rts

failWithCode
	move.l d2, d0
	rts
	.bend  ; runEvalProgram

compileAdditive	.block
	bsr.w resetProgram
	bsr.w compileSingleTerm
	tst.l d5
	bne.s fail

loop
	bsr.w skipWhitespace
	tst.l d0
	beq.s ok
	moveq #0, d6
	move.b (a0), d6
	cmpi.b #'+', d6
	beq.s operator
	cmpi.b #'-', d6
	bne.s trailingFail

operator
	move.l d6, -(sp)
	addq.l #1, a0
	subq.l #1, d0
	bsr.w compileSingleTerm
	move.l (sp)+, d6
	tst.l d5
	bne.s fail
	cmpi.b #'+', d6
	beq.s add
	moveq #runtime.EXPRVM_BINARY_SUBTRACT, d6
	bsr.w emitApplyBinaryD6
	bne.s fail
	bra.s loop

add
	moveq #runtime.EXPRVM_BINARY_ADD, d6
	bsr.w emitApplyBinaryD6
	bne.s fail
	bra.s loop

trailingFail
	moveq #33, d5
	bra.s fail

ok
	moveq #0, d0
	rts

fail
	tst.l d5
	beq.s genericFail
	move.l d5, d0
	rts

genericFail
	moveq #1, d0
	rts
	.bend  ; compileAdditive

compileSingleTerm	.block
	movem.l d4, -(sp)
	moveq #0, d4
	moveq #0, d5
	bsr.w skipWhitespace
	tst.l d0
	beq.w fail
	cmpi.b #'+', (a0)
	beq.s unaryPlus
	cmpi.b #'-', (a0)
	beq.s unaryMinus

body
	tst.l d0
	beq.w fail
	cmpi.b #'*', (a0)
	beq.s currentPc
	cmpi.b #'$', (a0)
	beq.w dollar
	cmpi.b #'%', (a0)
	beq.w binaryLiteral
	cmpi.b #'0', (a0)
	bne.s numberOrLabel
	cmpi.l #2, d0
	bcs.s numberOrLabel
	cmpi.b #'x', 1(a0)
	beq.w hex0x
	cmpi.b #'X', 1(a0)
	beq.w hex0x

numberOrLabel
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #'0', d1
	bcs.w label
	cmpi.b #'9', d1
	bhi.w label
	bra.w decimal

unaryPlus
	addq.l #1, a0
	subq.l #1, d0
	bra.s body

unaryMinus
	addq.l #1, a0
	subq.l #1, d0
	moveq #1, d4
	bra.s body

currentPc
	addq.l #1, a0
	subq.l #1, d0
	bsr.w emitPushCurrent
	bra.w maybeApplyUnary

dollar
	cmpi.l #1, d0
	beq.w currentPc
	moveq #0, d1
	move.b 1(a0), d1
	cmpi.b #'0', d1
	blo.s dollarUpperHex
	cmpi.b #'9', d1
	bls.w hex

dollarUpperHex
	cmpi.b #'A', d1
	blo.s dollarLowerHex
	cmpi.b #'F', d1
	bls.w hex

dollarLowerHex
	cmpi.b #'a', d1
	blo.w currentPc
	cmpi.b #'f', d1
	bls.w hex
	bra.w currentPc

hex
	addq.l #1, a0
	subq.l #1, d0
	bsr.w parseHex
	tst.l d5
	beq.s hexParsed
	moveq #31, d5
	bra.w maybeApplyUnary

hexParsed
	bsr.w emitPushLiteralD3
	beq.s hexEmitOk
	moveq #32, d5
	bra.w maybeApplyUnary

hexEmitOk
	moveq #0, d5
	bra.w maybeApplyUnary

hex0x
	addq.l #2, a0
	subq.l #2, d0
	bsr.w parseHex
	tst.l d5
	bne.w maybeApplyUnary
	bsr.w emitPushLiteralD3
	move.l d0, d5
	bra.w maybeApplyUnary

binaryLiteral
	addq.l #1, a0
	subq.l #1, d0
	bsr.w parseBinary
	tst.l d5
	bne.s maybeApplyUnary
	bsr.w emitPushLiteralD3
	move.l d0, d5
	bra.s maybeApplyUnary

decimal
	bsr.w parseDecimal
	tst.l d5
	bne.w maybeApplyUnary
	bsr.w emitPushLiteralD3
	move.l d0, d5
	bra.w maybeApplyUnary

label
	move.l d0, d6
	bsr.w termLength
	move.l d0, d2
	bsr.w resolveLabelIndex
	move.l d0, d5
	beq.s labelResolved
	moveq #0, d3
	move.w runtime.ExprvmCurrentPass, d3
	cmpi.w #1, d3
	bne.s maybeApplyUnary
	clr.l d3
	bsr.w emitPushLiteralD3
	move.l d0, d5
	bne.s maybeApplyUnary
	adda.l d2, a0
	move.l d6, d0
	sub.l d2, d0
	bra.s maybeApplyUnary

labelResolved
	bsr.w emitPushSymbolD3
	move.l d0, d5
	bne.s maybeApplyUnary
	adda.l d2, a0
	move.l d6, d0
	sub.l d2, d0

maybeApplyUnary
	tst.l d5
	bne.w return
	tst.l d4
	beq.s ok
	moveq #runtime.EXPRVM_UNARY_MINUS, d6
	bsr.w emitApplyUnaryD6
	move.l d0, d5
	bne.w return

ok
	moveq #0, d5
	bra.s return

fail
	moveq #34, d5

return
	movem.l (sp)+, d4
	rts
	.bend  ; compileSingleTerm

resetProgram	.block
	clr.w OpcoreExprVmProgramLen
	rts
	.bend  ; resetProgram

; Append the final ExprVM end/require-scalar sequence for the selected opcode version.
; Inputs: runtime.ExprvmSelectedOpcodeVersion = evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/D6/CCR.
; CCR: reflects D0 on return.
finalizeProgram	.block
	move.w runtime.ExprvmSelectedOpcodeVersion, d3
	cmpi.w #2, d3
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_REQUIRE_SCALAR, d6
	bsr.w emitU8D6
	bne.s return
	moveq #runtime.EXPRVM_V2_OPCODE_END, d6
	bra.s return

version1
	moveq #runtime.EXPRVM_OPCODE_END, d6

return
	bsr.w emitU8D6
	rts
	.bend  ; finalizeProgram

; Append the selected-version PushCurrentAddress opcode.
; Inputs: runtime.ExprvmSelectedOpcodeVersion = evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/D6/CCR.
; CCR: reflects D0 on return.
emitPushCurrent	.block
	move.w runtime.ExprvmSelectedOpcodeVersion, d3
	cmpi.w #2, d3
	bne.w version1
	moveq #runtime.EXPRVM_V2_OPCODE_PUSH_CURRENT_ADDR, d6
	bra.w ready

version1
	moveq #runtime.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d6

ready
	bra.w emitU8D6
	.bend  ; emitPushCurrent

; Append the selected-version unary opcode plus operand kind from D6.
; Inputs: D6 = ExprVM unary operator id; runtime.ExprvmSelectedOpcodeVersion =
; evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/CCR.
; CCR: reflects D0 on return.
emitApplyUnaryD6	.block
	movem.l d6, -(sp)
	move.l d6, d3
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_UNARY, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_APPLY_UNARY, d6

ready
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	bsr.w emitU8D6

return
	movem.l (sp)+, d6
	rts
	.bend  ; emitApplyUnaryD6

; Append the selected-version binary opcode plus operand kind from D6.
; Inputs: D6 = ExprVM binary operator id; runtime.ExprvmSelectedOpcodeVersion =
; evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D3/CCR.
; CCR: reflects D0 on return.
emitApplyBinaryD6	.block
	movem.l d6, -(sp)
	move.l d6, d3
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_APPLY_BINARY, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_APPLY_BINARY, d6

ready
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	bsr.w emitU8D6

return
	movem.l (sp)+, d6
	rts
	.bend  ; emitApplyBinaryD6

; Append the selected-version PushSymbol opcode plus the symbol index in D3.
; Inputs: D3 = symbol index; runtime.ExprvmSelectedOpcodeVersion = evaluator
; opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D2/D6/CCR.
; CCR: reflects D0 on return.
emitPushSymbolD3	.block
	movem.l d2-d3/d6, -(sp)
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_PUSH_SYMBOL, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, d6

ready
	bsr.w emitU8D6
	bne.s return
	movem.l (sp), d2-d3/d6
	bsr.w emitU16D3

return
	movem.l (sp)+, d2-d3/d6
	rts
	.bend  ; emitPushSymbolD3

; Append the selected-version PushLiteral opcode plus the 64-bit literal with
; the low 32 bits from D3 and a zero high word.
; Inputs: D3 = low 32 literal bits; runtime.ExprvmSelectedOpcodeVersion =
; evaluator opcode version.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D2/D6/CCR.
; CCR: reflects D0 on return.
emitPushLiteralD3	.block
	movem.l d2-d3/d6, -(sp)
	move.w runtime.ExprvmSelectedOpcodeVersion, d6
	cmpi.w #2, d6
	bne.s version1
	moveq #runtime.EXPRVM_V2_OPCODE_PUSH_LITERAL, d6
	bra.s ready

version1
	moveq #runtime.EXPRVM_OPCODE_PUSH_LITERAL, d6

ready
	bsr.w emitU8D6
	bne.s return
	movem.l (sp), d2-d3/d6
	bsr.w emitU32D3
	bne.s return
	clr.l d3
	bsr.w emitU32D3

return
	movem.l (sp)+, d2-d3/d6
	rts
	.bend  ; emitPushLiteralD3

; Append D3 as four little-endian bytes to the private ExprVM program buffer.
; Inputs: D3 = value to encode.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D6/CCR.
; CCR: reflects D0 on return.
emitU32D3	.block
	move.l d3, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	lsr.l #8, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	lsr.l #8, d6
	lsr.l #8, d6
	bsr.w emitU8D6

return
	rts
	.bend  ; emitU32D3

; Append D3 as two little-endian bytes to the private ExprVM program buffer.
; Inputs: D3 = value to encode.
; Outputs: D0 = 0 on success or 1 on program-buffer overflow.
; Clobbers: D6/CCR.
; CCR: reflects D0 on return.
emitU16D3	.block
	move.l d3, d6
	bsr.w emitU8D6
	bne.s return
	move.l d3, d6
	lsr.l #8, d6
	bsr.w emitU8D6

return
	rts
	.bend  ; emitU16D3

; Append the low byte of D6 to the private ExprVM program buffer.
; Inputs: D6 = byte value to append; OpcoreExprVmProgramLen = current length.
; Outputs: D0 = 0 on success or 1 on buffer overflow; OpcoreExprVmProgramLen
; incremented on success.
; Clobbers: D1/A2/CCR.
; CCR: reflects D0 on return.
emitU8D6	.block
	movem.l d1/a2, -(sp)
	moveq #0, d0
	move.w OpcoreExprVmProgramLen, d0
	cmpi.l #OPCORE_EXPRVM_PROGRAM_CAPACITY, d0
	bhs.s fail
	lea OpcoreExprVmProgramBuffer, a2
	move.b d6, 0(a2, d0.l)
	addi.w #1, OpcoreExprVmProgramLen
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1/a2
	rts
	.bend  ; emitU8D6

skipWhitespace	.block
	tst.l d0
	beq.s done
	moveq #0, d1
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s one
	cmpi.b #9, d1
	bne.s done

one
	addq.l #1, a0
	subq.l #1, d0
	bra.s skipWhitespace

done
	rts
	.bend  ; skipWhitespace

termLength	.block
	movem.l d1-d2/a0, -(sp)
	clr.l d2

loop
	cmp.l d0, d2
	bhs.s done
	moveq #0, d1
	move.b 0(a0, d2.l), d1
	cmpi.b #' ', d1
	beq.s done
	cmpi.b #9, d1
	beq.s done
	cmpi.b #'+', d1
	beq.s done
	cmpi.b #'-', d1
	beq.s done
	addq.l #1, d2
	bra.s loop

done
	move.l d2, d0
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; termLength

parseHex	.block
	movem.l d1-d2, -(sp)
	clr.l d3

loop
	tst.l d0
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'+', d1
	beq.s endBeforeOperator
	cmpi.b #'-', d1
	beq.s endBeforeOperator
	cmpi.b #' ', d1
	beq.s ok
	cmpi.b #9, d1
	beq.s ok
	cmpi.b #'0', d1
	bcs.s fail
	cmpi.b #'9', d1
	bls.s digit
	cmpi.b #'A', d1
	bcs.s lower
	cmpi.b #'F', d1
	bhi.s lower
	subi.b #'A' - 10, d1
	bra.s haveDigit

lower
	cmpi.b #'a', d1
	bcs.s fail
	cmpi.b #'f', d1
	bhi.s fail
	subi.b #'a' - 10, d1
	bra.s haveDigit

digit
	subi.b #'0', d1

haveDigit
	lsl.l #4, d3
	or.b d1, d3
	bra.s loop

ok
	moveq #0, d5
	bra.s return

endBeforeOperator
	subq.l #1, a0
	addq.l #1, d0
	moveq #0, d5
	bra.s return

fail
	moveq #1, d5

return
	movem.l (sp)+, d1-d2
	rts
	.bend  ; parseHex

parseBinary	.block
	movem.l d1, -(sp)
	clr.l d3

loop
	tst.l d0
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'+', d1
	beq.s endBeforeOperator
	cmpi.b #'-', d1
	beq.s endBeforeOperator
	cmpi.b #' ', d1
	beq.s ok
	cmpi.b #9, d1
	beq.s ok
	cmpi.b #'0', d1
	beq.s digit
	cmpi.b #'1', d1
	bne.s fail

digit
	subi.b #'0', d1
	lsl.l #1, d3
	or.b d1, d3
	bra.s loop

ok
	moveq #0, d5
	bra.s return

endBeforeOperator
	subq.l #1, a0
	addq.l #1, d0
	moveq #0, d5
	bra.s return

fail
	moveq #1, d5

return
	movem.l (sp)+, d1
	rts
	.bend  ; parseBinary

parseDecimal	.block
	movem.l d1-d2, -(sp)
	clr.l d3

loop
	tst.l d0
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #'+', d1
	beq.s endBeforeOperator
	cmpi.b #'-', d1
	beq.s endBeforeOperator
	cmpi.b #' ', d1
	beq.s ok
	cmpi.b #9, d1
	beq.s ok
	cmpi.b #'0', d1
	bcs.s fail
	cmpi.b #'9', d1
	bhi.s fail
	subi.b #'0', d1
	move.l d3, d2
	lsl.l #3, d3
	add.l d2, d3
	add.l d2, d3
	add.l d1, d3
	bra.s loop

ok
	moveq #0, d5
	bra.s return

endBeforeOperator
	subq.l #1, a0
	addq.l #1, d0
	moveq #0, d5
	bra.s return

fail
	moveq #1, d5

return
	movem.l (sp)+, d1-d2
	rts
	.bend  ; parseDecimal

resolveLabelIndex	.block
	movem.l d1-d2/d4-d6/a0-a2, -(sp)
	movea.l a0, a2
	move.l d0, d6
	clr.w d4

loop
	cmp.w d7, d4
	bhs.s fail
	moveq #0, d5
	move.w d4, d5
	lsl.l #6, d5
	movea.l a3, a0
	adda.l d5, a0
	movea.l a2, a1
	move.l d6, d0
	bsr.w labelEquals
	bne.s found
	addq.w #1, d4
	bra.s loop

found
	moveq #0, d3
	move.w d4, d3
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	rts
	.bend  ; resolveLabelIndex

labelEquals	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l d0, d3
	beq.s no

loop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d2, d1
	bne.s no
	subq.l #1, d3
	bne.s loop
	tst.b (a0)
	bne.s no
	moveq #1, d0
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; labelEquals

OpcoreExvmDefaultProgram
	.byte EXVM_OPCODE_PARSE_EXPRESSION, EXVM_OPCODE_END
OPCORE_EXVM_DEFAULT_PROGRAM_END

OPCORE_EXVM_DEFAULT_PROGRAM_LEN = OPCORE_EXVM_DEFAULT_PROGRAM_END - OpcoreExvmDefaultProgram
	.priv

	.endsection

	.section bss, kind=bss

OpcoreExvmSelectedOpcodeVersion
	.res word, 1
OpcoreExprVmProgramLen
	.res word, 1
OpcoreExprVmProgramBuffer
	.res byte, OPCORE_EXPRVM_PROGRAM_CAPACITY
	.align 4

	.endsection
	.endmodule
