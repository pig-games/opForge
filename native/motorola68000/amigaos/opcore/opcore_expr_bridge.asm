; Native opcore/EXVM-style scalar operand expression bridge.

        .module opcore.amigaos.expr_bridge
        .cpu 68020
        .pub

TOKEN_BUFFER_CAPACITY           = 64
EXVM_OPCODE_END                 = $00
EXVM_OPCODE_PARSE_EXPRESSION    = $01
EXVM_OPCODE_EMIT_DIAG           = $02
EXVM_OPCODE_FAIL                = $03
EXPRVM_OPCODE_END               = $00
EXPRVM_OPCODE_PUSH_LITERAL      = $01
EXPRVM_OPCODE_PUSH_CURRENT_ADDR = $02
EXPRVM_OPCODE_PUSH_SYMBOL       = $03
EXPRVM_OPCODE_APPLY_UNARY       = $04
EXPRVM_OPCODE_APPLY_BINARY      = $05
EXPRVM_UNARY_PLUS               = 0
EXPRVM_UNARY_MINUS              = 1
EXPRVM_UNARY_BIT_NOT            = 2
EXPRVM_UNARY_LOGIC_NOT          = 3
EXPRVM_UNARY_HIGH               = 4
EXPRVM_UNARY_LOW                = 5
EXPRVM_BINARY_ADD               = 6
EXPRVM_BINARY_SUBTRACT          = 7
OPCORE_EXPRVM_STACK_CAPACITY    = 8

        .section code, kind=code

; ---------------------------------------------------------------------------
; Evaluate one scalar operand expression through the native EXVM default path.
;
; opcore_expr_eval_operand_v1 is the compatibility entry used by the current
; native selector/pass code. It now runs the same EXVM default program shape as
; the Rust path (`ParseExpression`, `End`) before evaluating the 6502 first-run
; scalar subset. The ParseExpression opcode still delegates to the temporary
; native text parser below until the full token/bytecode expression contract is
; implemented natively.
;
; Supported input forms:
; - optional immediate prefix '#'
; - '$' and '0x' prefixed hexadecimal numbers
; - '%' prefixed binary numbers
; - decimal numbers
; - unary '+' and '-'
; - additive/subtractive expressions over scalar terms
; - '*' current address terms supplied by the caller
; - labels resolved through the caller-supplied label tables
;
; Inputs:
; - A0/D0: operand text pointer and byte length.
; - A1: fixed-width label-name table pointer.
; - A2: label-value table pointer parallel to A1.
; - D1: number of label entries.
; - D2: current assembly PC for '*' current-address terms.
;
; Outputs:
; - D0: 0 on success, 1 on parse/lookup failure.
; - D3: resolved scalar value on success.
; ---------------------------------------------------------------------------

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
; - D6: assembler pass number.
;
; Outputs:
; - D0: 0 on success, 1 on invalid program/evaluation failure.
; - D3: resolved scalar value on success.
; - D4: nonzero when the program referenced at least one symbol.
; - D5: nonzero when the program referenced a symbol during pass 1.
; ---------------------------------------------------------------------------

opcore_exprvm_eval_program_v1:
        MOVEM.L D1-D2/D6-D7/A0-A6, -(SP)
        MOVEA.L A1, A3
        MOVEA.L A2, A4
        MOVEA.L D2, A5
        MOVEA.L D6, A6
        CLR.L D3
        CLR.L D4
        CLR.L D5
        CLR.L D7

opcoreExprVmEvalLoop:
        TST.L D0
        BEQ.W opcoreExprVmEvalFail
        MOVEQ #0, D6
        MOVE.B (A0)+, D6
        SUBQ.L #1, D0
        CMPI.B #EXPRVM_OPCODE_END, D6
        BEQ.W opcoreExprVmOpcodeEnd
        CMPI.B #EXPRVM_OPCODE_PUSH_LITERAL, D6
        BEQ.W opcoreExprVmOpcodePushLiteral
        CMPI.B #EXPRVM_OPCODE_PUSH_CURRENT_ADDR, D6
        BEQ.W opcoreExprVmOpcodePushCurrent
        CMPI.B #EXPRVM_OPCODE_PUSH_SYMBOL, D6
        BEQ.W opcoreExprVmOpcodePushSymbol
        CMPI.B #EXPRVM_OPCODE_APPLY_UNARY, D6
        BEQ.W opcoreExprVmOpcodeApplyUnary
        CMPI.B #EXPRVM_OPCODE_APPLY_BINARY, D6
        BEQ.W opcoreExprVmOpcodeApplyBinary
        BRA.W opcoreExprVmEvalFail

opcoreExprVmOpcodePushLiteral:
        BSR.W opcoreExprVmReadI64Low32
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        BSR.W opcoreExprVmPushD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        BRA.W opcoreExprVmEvalLoop

opcoreExprVmOpcodePushCurrent:
        MOVE.L A5, D3
        BSR.W opcoreExprVmPushD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        BRA.W opcoreExprVmEvalLoop

opcoreExprVmOpcodePushSymbol:
        BSR.W opcoreExprVmReadU16
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        CMP.W D1, D3
        BHS.W opcoreExprVmEvalFail
        MOVEQ #1, D4
        MOVE.L A6, D6
        CMPI.L #1, D6
        BNE.S opcoreExprVmPushSymbolStable
        MOVEQ #1, D5

opcoreExprVmPushSymbolStable:
        MOVEQ #0, D6
        MOVE.W D3, D6
        LSL.L #2, D6
        MOVEA.L A4, A2
        MOVE.L 0(A2,D6.L), D3
        BSR.W opcoreExprVmPushD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        BRA.W opcoreExprVmEvalLoop

opcoreExprVmOpcodeApplyUnary:
        BSR.W opcoreExprVmReadU8
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        MOVE.L D3, D6
        BSR.W opcoreExprVmPopD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        CMPI.B #EXPRVM_UNARY_PLUS, D6
        BEQ.S opcoreExprVmApplyUnaryDone
        CMPI.B #EXPRVM_UNARY_MINUS, D6
        BEQ.S opcoreExprVmApplyUnaryMinus
        CMPI.B #EXPRVM_UNARY_BIT_NOT, D6
        BEQ.S opcoreExprVmApplyUnaryBitNot
        CMPI.B #EXPRVM_UNARY_LOGIC_NOT, D6
        BEQ.S opcoreExprVmApplyUnaryLogicNot
        CMPI.B #EXPRVM_UNARY_HIGH, D6
        BEQ.S opcoreExprVmApplyUnaryHigh
        CMPI.B #EXPRVM_UNARY_LOW, D6
        BEQ.S opcoreExprVmApplyUnaryLow
        BRA.W opcoreExprVmEvalFail

opcoreExprVmApplyUnaryMinus:
        NEG.L D3
        BRA.S opcoreExprVmApplyUnaryDone

opcoreExprVmApplyUnaryBitNot:
        NOT.L D3
        BRA.S opcoreExprVmApplyUnaryDone

opcoreExprVmApplyUnaryLogicNot:
        TST.L D3
        BEQ.S opcoreExprVmApplyUnaryLogicNotTrue
        CLR.L D3
        BRA.S opcoreExprVmApplyUnaryDone

opcoreExprVmApplyUnaryLogicNotTrue:
        MOVEQ #1, D3
        BRA.S opcoreExprVmApplyUnaryDone

opcoreExprVmApplyUnaryHigh:
        LSR.L #8, D3
        ANDI.L #$000000FF, D3
        BRA.S opcoreExprVmApplyUnaryDone

opcoreExprVmApplyUnaryLow:
        ANDI.L #$000000FF, D3

opcoreExprVmApplyUnaryDone:
        BSR.W opcoreExprVmPushD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        BRA.W opcoreExprVmEvalLoop

opcoreExprVmOpcodeApplyBinary:
        BSR.W opcoreExprVmReadU8
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        MOVE.L D3, D6
        BSR.W opcoreExprVmPopD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        MOVE.L D3, -(SP)
        BSR.W opcoreExprVmPopD3
        TST.L D0
        BMI.S opcoreExprVmApplyBinaryRestoreFail
        MOVE.L (SP)+, D2
        CMPI.B #EXPRVM_BINARY_ADD, D6
        BEQ.S opcoreExprVmApplyBinaryAdd
        CMPI.B #EXPRVM_BINARY_SUBTRACT, D6
        BEQ.S opcoreExprVmApplyBinarySubtract
        BRA.W opcoreExprVmEvalFail

opcoreExprVmApplyBinaryRestoreFail:
        ADDQ.L #4, SP
        BRA.W opcoreExprVmEvalFail

opcoreExprVmApplyBinaryAdd:
        ADD.L D2, D3
        BRA.S opcoreExprVmApplyBinaryDone

opcoreExprVmApplyBinarySubtract:
        SUB.L D2, D3

opcoreExprVmApplyBinaryDone:
        BSR.W opcoreExprVmPushD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        BRA.W opcoreExprVmEvalLoop

opcoreExprVmOpcodeEnd:
        CMPI.L #1, D7
        BNE.W opcoreExprVmEvalFail
        BSR.W opcoreExprVmPopD3
        TST.L D0
        BMI.W opcoreExprVmEvalFail
        MOVEQ #0, D0
        BRA.S opcoreExprVmEvalReturn

opcoreExprVmEvalFail:
        MOVEQ #1, D0

opcoreExprVmEvalReturn:
        MOVEM.L (SP)+, D1-D2/D6-D7/A0-A6
        RTS

opcoreExprVmPushD3:
        CMPI.L #OPCORE_EXPRVM_STACK_CAPACITY, D7
        BHS.S opcoreExprVmPushFail
        MOVE.L D7, D2
        LSL.L #2, D2
        LEA opcoreExprVmStack, A2
        MOVE.L D3, 0(A2,D2.L)
        ADDQ.L #1, D7
        MOVEQ #0, D0
        RTS

opcoreExprVmPushFail:
        MOVEQ #-1, D0
        RTS

opcoreExprVmPopD3:
        TST.L D7
        BEQ.S opcoreExprVmPopFail
        SUBQ.L #1, D7
        MOVE.L D7, D2
        LSL.L #2, D2
        LEA opcoreExprVmStack, A2
        MOVE.L 0(A2,D2.L), D3
        MOVEQ #0, D0
        RTS

opcoreExprVmPopFail:
        MOVEQ #-1, D0
        RTS

opcoreExprVmReadU8:
        TST.L D0
        BEQ.S opcoreExprVmReadU8Fail
        MOVEQ #0, D3
        MOVE.B (A0)+, D3
        SUBQ.L #1, D0
        RTS

opcoreExprVmReadU8Fail:
        MOVEQ #-1, D0
        RTS

opcoreExprVmReadU16:
        CMPI.L #2, D0
        BCS.S opcoreExprVmReadU16Fail
        MOVEQ #0, D3
        MOVE.B (A0)+, D3
        MOVEQ #0, D2
        MOVE.B (A0)+, D2
        LSL.W #8, D2
        OR.W D2, D3
        SUBQ.L #2, D0
        RTS

opcoreExprVmReadU16Fail:
        MOVEQ #-1, D0
        RTS

opcoreExprVmReadI64Low32:
        CMPI.L #8, D0
        BCS.S opcoreExprVmReadI64Fail
        MOVEQ #0, D3
        MOVE.B (A0)+, D3
        MOVEQ #0, D2
        MOVE.B (A0)+, D2
        LSL.L #8, D2
        OR.L D2, D3
        MOVEQ #0, D2
        MOVE.B (A0)+, D2
        LSL.L #8, D2
        LSL.L #8, D2
        OR.L D2, D3
        MOVEQ #0, D2
        MOVE.B (A0)+, D2
        LSL.L #8, D2
        LSL.L #8, D2
        LSL.L #8, D2
        OR.L D2, D3
        ADDQ.L #4, A0
        SUBQ.L #8, D0
        RTS

opcoreExprVmReadI64Fail:
        MOVEQ #-1, D0
        RTS

opcore_expr_eval_operand_v1:
opcore_exvm_eval_operand_v1:
        MOVEM.L D1-D2/D4-D7/A0-A5, -(SP)
        MOVEA.L A1, A3                  ; label-name table base kept stable across parse helpers
        MOVEA.L A2, A4                  ; label-value table base kept stable across parse helpers
        MOVEA.L D2, A5                  ; A5 carries the current PC for '*' terms
        MOVE.W D1, D7                   ; D7 is the label count consumed by the resolver loop
        CLR.L D3
        BSR.W opcoreExprBridgeSkipWhitespace
        TST.L D0
        BEQ.W opcoreExprBridgeFail
        CMPI.B #'#', (A0)
        BNE.S opcoreExprBridgeNoImmediatePrefix
        ADDQ.L #1, A0                   ; strip immediate marker; addressing mode was selected elsewhere
        SUBQ.L #1, D0                   ; keep remaining text length aligned with A0
        BSR.W opcoreExprBridgeSkipWhitespace

opcoreExprBridgeNoImmediatePrefix:
        TST.L D0
        BEQ.W opcoreExprBridgeFail
        LEA opcoreExvmDefaultProgram(PC), A1
        MOVEQ #OPCORE_EXVM_DEFAULT_PROGRAM_LEN, D1
        BSR.W opcoreExvmRunEvalProgram
        BRA.W opcoreExprBridgeReturn

opcoreExvmRunEvalProgram:
        MOVEQ #0, D4                    ; output expression count, matching Rust's stack-depth 1 contract

opcoreExvmRunLoop:
        TST.L D1
        BEQ.S opcoreExvmMissingEnd
        MOVEQ #0, D6
        MOVE.B (A1)+, D6
        SUBQ.L #1, D1
        CMPI.B #EXVM_OPCODE_END, D6
        BEQ.S opcoreExvmOpcodeEnd
        CMPI.B #EXVM_OPCODE_PARSE_EXPRESSION, D6
        BEQ.S opcoreExvmOpcodeParseExpression
        CMPI.B #EXVM_OPCODE_EMIT_DIAG, D6
        BEQ.S opcoreExvmProgramFail
        CMPI.B #EXVM_OPCODE_FAIL, D6
        BEQ.S opcoreExvmProgramFail
        BRA.S opcoreExvmProgramFail

opcoreExvmOpcodeParseExpression:
        TST.L D4
        BNE.S opcoreExvmProgramFail
        MOVEM.L D1/D4/A1, -(SP)
        BSR.W opcoreExprBridgeEvalAdditive
        MOVE.L D0, D2
        MOVEM.L (SP)+, D1/D4/A1
        TST.L D2
        BNE.S opcoreExvmProgramFail
        ADDQ.L #1, D4
        BRA.S opcoreExvmRunLoop

opcoreExvmOpcodeEnd:
        CMPI.L #1, D4
        BNE.S opcoreExvmProgramFail
        MOVEQ #0, D0
        RTS

opcoreExvmMissingEnd:
opcoreExvmProgramFail:
        MOVEQ #1, D0
        RTS

opcoreExprBridgeEvalAdditive:
        BSR.W opcoreExprBridgeEvalSingleTerm
        TST.L D5
        BNE.S opcoreExprBridgeEvalAdditiveReturn

opcoreExprBridgeEvalAdditiveLoop:
        BSR.W opcoreExprBridgeSkipWhitespace
        TST.L D0
        BEQ.S opcoreExprBridgeEvalAdditiveOk
        MOVEQ #0, D6
        MOVE.B (A0), D6
        CMPI.B #'+', D6
        BEQ.S opcoreExprBridgeEvalAdditiveOperator
        CMPI.B #'-', D6
        BNE.S opcoreExprBridgeEvalAdditiveFail

opcoreExprBridgeEvalAdditiveOperator:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        MOVE.L D3, D4
        BSR.W opcoreExprBridgeEvalSingleTerm
        TST.L D5
        BNE.S opcoreExprBridgeEvalAdditiveReturn
        CMPI.B #'+', D6
        BEQ.S opcoreExprBridgeEvalAdditiveAdd
        MOVE.L D4, D2
        SUB.L D3, D2
        MOVE.L D2, D3
        BRA.S opcoreExprBridgeEvalAdditiveLoop

opcoreExprBridgeEvalAdditiveAdd:
        ADD.L D4, D3
        BRA.S opcoreExprBridgeEvalAdditiveLoop

opcoreExprBridgeEvalAdditiveOk:
        MOVEQ #0, D0
        RTS

opcoreExprBridgeEvalAdditiveFail:
        MOVEQ #1, D0

opcoreExprBridgeEvalAdditiveReturn:
        RTS

opcoreExprBridgeEvalSingleTerm:
        MOVEM.L D4, -(SP)
        MOVEQ #0, D4
        MOVEQ #0, D5
        BSR.W opcoreExprBridgeSkipWhitespace
        TST.L D0
        BEQ.W opcoreExprBridgeEvalSingleFail
        CMPI.B #'+', (A0)
        BEQ.S opcoreExprBridgeEvalSingleUnaryPlus
        CMPI.B #'-', (A0)
        BEQ.S opcoreExprBridgeEvalSingleUnaryMinus

opcoreExprBridgeEvalSingleBody:
        TST.L D0
        BEQ.W opcoreExprBridgeEvalSingleFail
        CMPI.B #'*', (A0)
        BEQ.S opcoreExprBridgeCurrentPc
        CMPI.B #'$', (A0)
        BEQ.W opcoreExprBridgeHex
        CMPI.B #'%', (A0)
        BEQ.W opcoreExprBridgeBinaryLiteral
        CMPI.B #'0', (A0)
        BNE.S opcoreExprBridgeEvalSingleNumberOrLabel
        CMPI.L #2, D0
        BCS.S opcoreExprBridgeEvalSingleNumberOrLabel
        CMPI.B #'x', 1(A0)
        BEQ.S opcoreExprBridgeHex0x
        CMPI.B #'X', 1(A0)
        BEQ.S opcoreExprBridgeHex0x

opcoreExprBridgeEvalSingleNumberOrLabel:
        MOVEQ #0, D1
        MOVE.B (A0), D1                 ; first non-space byte decides decimal versus label lookup
        CMPI.B #'0', D1
        BCS.S opcoreExprBridgeLabel
        CMPI.B #'9', D1
        BHI.S opcoreExprBridgeLabel
        BRA.S opcoreExprBridgeDecimal

opcoreExprBridgeEvalSingleUnaryPlus:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BRA.S opcoreExprBridgeEvalSingleBody

opcoreExprBridgeEvalSingleUnaryMinus:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        MOVEQ #1, D4
        BRA.S opcoreExprBridgeEvalSingleBody

opcoreExprBridgeCurrentPc:
        MOVE.L A5, D3
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BRA.S opcoreExprBridgeEvalSingleApplyUnary

opcoreExprBridgeHex:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opcoreExprBridgeParseHex
        BRA.S opcoreExprBridgeEvalSingleMaybeApplyUnary

opcoreExprBridgeHex0x:
        ADDQ.L #2, A0
        SUBQ.L #2, D0
        BSR.W opcoreExprBridgeParseHex
        BRA.S opcoreExprBridgeEvalSingleMaybeApplyUnary

opcoreExprBridgeBinaryLiteral:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opcoreExprBridgeParseBinary
        BRA.S opcoreExprBridgeEvalSingleMaybeApplyUnary

opcoreExprBridgeDecimal:
        BSR.W opcoreExprBridgeParseDecimal
        BRA.S opcoreExprBridgeEvalSingleMaybeApplyUnary

opcoreExprBridgeLabel:
        MOVE.L D0, D6
        BSR.W opcoreExprBridgeTermLength
        MOVE.L D0, D2
        BSR.W opcoreExprBridgeResolveLabel
        MOVE.L D0, D5
        TST.L D5
        BNE.S opcoreExprBridgeEvalSingleMaybeApplyUnary
        ADDA.L D2, A0
        MOVE.L D6, D0
        SUB.L D2, D0

opcoreExprBridgeEvalSingleMaybeApplyUnary:
        TST.L D5
        BNE.S opcoreExprBridgeEvalSingleReturn

opcoreExprBridgeEvalSingleApplyUnary:
        TST.L D4
        BEQ.S opcoreExprBridgeEvalSingleOk
        NEG.L D3

opcoreExprBridgeEvalSingleOk:
        MOVEQ #0, D5
        BRA.S opcoreExprBridgeEvalSingleReturn

opcoreExprBridgeEvalSingleFail:
        MOVEQ #1, D5

opcoreExprBridgeEvalSingleReturn:
        MOVEM.L (SP)+, D4
        RTS

opcoreExprBridgeFail:
        MOVEQ #1, D0

opcoreExprBridgeReturn:
        MOVEM.L (SP)+, D1-D2/D4-D7/A0-A5
        RTS

opcoreExprBridgeSkipWhitespace:
        TST.L D0
        BEQ.S opcoreExprBridgeSkipWhitespaceDone
        MOVEQ #0, D1
        MOVE.B (A0), D1
        CMPI.B #' ', D1
        BEQ.S opcoreExprBridgeSkipWhitespaceOne
        CMPI.B #9, D1
        BNE.S opcoreExprBridgeSkipWhitespaceDone

opcoreExprBridgeSkipWhitespaceOne:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BRA.S opcoreExprBridgeSkipWhitespace

opcoreExprBridgeSkipWhitespaceDone:
        RTS

opcoreExprBridgeTermLength:
        MOVEM.L D1-D2/A0, -(SP)
        CLR.L D2

opcoreExprBridgeTermLengthLoop:
        CMP.L D0, D2
        BHS.S opcoreExprBridgeTermLengthDone
        MOVEQ #0, D1
        MOVE.B 0(A0,D2.L), D1
        CMPI.B #' ', D1
        BEQ.S opcoreExprBridgeTermLengthDone
        CMPI.B #9, D1
        BEQ.S opcoreExprBridgeTermLengthDone
        CMPI.B #'+', D1
        BEQ.S opcoreExprBridgeTermLengthDone
        CMPI.B #'-', D1
        BEQ.S opcoreExprBridgeTermLengthDone
        ADDQ.L #1, D2
        BRA.S opcoreExprBridgeTermLengthLoop

opcoreExprBridgeTermLengthDone:
        MOVE.L D2, D0
        MOVEM.L (SP)+, D1-D2/A0
        RTS

opcoreExprBridgeParseHex:
        MOVEM.L D1-D2, -(SP)
        CLR.L D3

opcoreExprBridgeParseHexLoop:
        TST.L D0
        BEQ.S opcoreExprBridgeParseHexOk
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        SUBQ.L #1, D0
        CMPI.B #'+', D1
        BEQ.S opcoreExprBridgeParseHexEndBeforeOperator
        CMPI.B #'-', D1
        BEQ.S opcoreExprBridgeParseHexEndBeforeOperator
        CMPI.B #' ', D1
        BEQ.S opcoreExprBridgeParseHexOk
        CMPI.B #9, D1
        BEQ.S opcoreExprBridgeParseHexOk
        CMPI.B #'0', D1
        BCS.S opcoreExprBridgeParseHexFail
        CMPI.B #'9', D1
        BLS.S opcoreExprBridgeParseHexDigit
        CMPI.B #'A', D1
        BCS.S opcoreExprBridgeParseHexLower
        CMPI.B #'F', D1
        BHI.S opcoreExprBridgeParseHexLower
        SUBI.B #'A' - 10, D1
        BRA.S opcoreExprBridgeParseHexHaveDigit

opcoreExprBridgeParseHexLower:
        CMPI.B #'a', D1
        BCS.S opcoreExprBridgeParseHexFail
        CMPI.B #'f', D1
        BHI.S opcoreExprBridgeParseHexFail
        SUBI.B #'a' - 10, D1
        BRA.S opcoreExprBridgeParseHexHaveDigit

opcoreExprBridgeParseHexDigit:
        SUBI.B #'0', D1

opcoreExprBridgeParseHexHaveDigit:
        LSL.L #4, D3
        OR.B D1, D3
        BRA.S opcoreExprBridgeParseHexLoop

opcoreExprBridgeParseHexOk:
        MOVEQ #0, D5
        BRA.S opcoreExprBridgeParseHexReturn

opcoreExprBridgeParseHexEndBeforeOperator:
        SUBQ.L #1, A0
        ADDQ.L #1, D0
        MOVEQ #0, D5
        BRA.S opcoreExprBridgeParseHexReturn

opcoreExprBridgeParseHexFail:
        MOVEQ #1, D5

opcoreExprBridgeParseHexReturn:
        MOVEM.L (SP)+, D1-D2
        RTS

opcoreExprBridgeParseBinary:
        MOVEM.L D1, -(SP)
        CLR.L D3

opcoreExprBridgeParseBinaryLoop:
        TST.L D0
        BEQ.S opcoreExprBridgeParseBinaryOk
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        SUBQ.L #1, D0
        CMPI.B #'+', D1
        BEQ.S opcoreExprBridgeParseBinaryEndBeforeOperator
        CMPI.B #'-', D1
        BEQ.S opcoreExprBridgeParseBinaryEndBeforeOperator
        CMPI.B #' ', D1
        BEQ.S opcoreExprBridgeParseBinaryOk
        CMPI.B #9, D1
        BEQ.S opcoreExprBridgeParseBinaryOk
        CMPI.B #'0', D1
        BEQ.S opcoreExprBridgeParseBinaryDigit
        CMPI.B #'1', D1
        BNE.S opcoreExprBridgeParseBinaryFail

opcoreExprBridgeParseBinaryDigit:
        SUBI.B #'0', D1
        LSL.L #1, D3
        OR.B D1, D3
        BRA.S opcoreExprBridgeParseBinaryLoop

opcoreExprBridgeParseBinaryOk:
        MOVEQ #0, D5
        BRA.S opcoreExprBridgeParseBinaryReturn

opcoreExprBridgeParseBinaryEndBeforeOperator:
        SUBQ.L #1, A0
        ADDQ.L #1, D0
        MOVEQ #0, D5
        BRA.S opcoreExprBridgeParseBinaryReturn

opcoreExprBridgeParseBinaryFail:
        MOVEQ #1, D5

opcoreExprBridgeParseBinaryReturn:
        MOVEM.L (SP)+, D1
        RTS

opcoreExprBridgeParseDecimal:
        MOVEM.L D1-D2, -(SP)
        CLR.L D3

opcoreExprBridgeParseDecimalLoop:
        TST.L D0
        BEQ.S opcoreExprBridgeParseDecimalOk
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        SUBQ.L #1, D0
        CMPI.B #'+', D1
        BEQ.S opcoreExprBridgeParseDecimalEndBeforeOperator
        CMPI.B #'-', D1
        BEQ.S opcoreExprBridgeParseDecimalEndBeforeOperator
        CMPI.B #' ', D1
        BEQ.S opcoreExprBridgeParseDecimalOk
        CMPI.B #9, D1
        BEQ.S opcoreExprBridgeParseDecimalOk
        CMPI.B #'0', D1
        BCS.S opcoreExprBridgeParseDecimalFail
        CMPI.B #'9', D1
        BHI.S opcoreExprBridgeParseDecimalFail
        SUBI.B #'0', D1
        MOVE.L D3, D2
        LSL.L #3, D3
        ADD.L D2, D3
        ADD.L D2, D3
        ADD.L D1, D3
        BRA.S opcoreExprBridgeParseDecimalLoop

opcoreExprBridgeParseDecimalOk:
        MOVEQ #0, D5
        BRA.S opcoreExprBridgeParseDecimalReturn

opcoreExprBridgeParseDecimalEndBeforeOperator:
        SUBQ.L #1, A0
        ADDQ.L #1, D0
        MOVEQ #0, D5
        BRA.S opcoreExprBridgeParseDecimalReturn

opcoreExprBridgeParseDecimalFail:
        MOVEQ #1, D5

opcoreExprBridgeParseDecimalReturn:
        MOVEM.L (SP)+, D1-D2
        RTS

opcoreExprBridgeResolveLabel:
        MOVEM.L D1-D2/D4-D6/A0-A2, -(SP)
        MOVEA.L A0, A2
        MOVE.L D0, D6
        CLR.W D4

opcoreExprBridgeResolveLabelLoop:
        CMP.W D7, D4
        BHS.S opcoreExprBridgeResolveLabelFail
        MOVEQ #0, D5
        MOVE.W D4, D5
        LSL.L #6, D5
        MOVEA.L A3, A0
        ADDA.L D5, A0
        MOVEA.L A2, A1
        MOVE.L D6, D0
        BSR.W opcoreExprBridgeLabelEquals
        TST.L D0
        BNE.S opcoreExprBridgeResolveLabelFound
        ADDQ.W #1, D4
        BRA.S opcoreExprBridgeResolveLabelLoop

opcoreExprBridgeResolveLabelFound:
        MOVEQ #0, D5
        MOVE.W D4, D5
        LSL.L #2, D5
        MOVEA.L A4, A0
        MOVE.L 0(A0,D5.L), D3
        MOVEQ #0, D0
        BRA.S opcoreExprBridgeResolveLabelReturn

opcoreExprBridgeResolveLabelFail:
        MOVEQ #1, D0

opcoreExprBridgeResolveLabelReturn:
        MOVEM.L (SP)+, D1-D2/D4-D6/A0-A2
        RTS

opcoreExprBridgeLabelEquals:
        MOVEM.L D1-D3/A0-A1, -(SP)
        MOVE.L D0, D3
        BEQ.S opcoreExprBridgeLabelEqualsNo

opcoreExprBridgeLabelEqualsLoop:
        MOVE.B (A0)+, D1
        MOVE.B (A1)+, D2
        CMP.B D2, D1
        BNE.S opcoreExprBridgeLabelEqualsNo
        SUBQ.L #1, D3
        BNE.S opcoreExprBridgeLabelEqualsLoop
        TST.B (A0)
        BNE.S opcoreExprBridgeLabelEqualsNo
        MOVEQ #1, D0
        BRA.S opcoreExprBridgeLabelEqualsReturn

opcoreExprBridgeLabelEqualsNo:
        MOVEQ #0, D0

opcoreExprBridgeLabelEqualsReturn:
        MOVEM.L (SP)+, D1-D3/A0-A1
        RTS

opcoreExvmDefaultProgram:
        .byte EXVM_OPCODE_PARSE_EXPRESSION, EXVM_OPCODE_END
opcoreExvmDefaultProgramEnd:

OPCORE_EXVM_DEFAULT_PROGRAM_LEN = opcoreExvmDefaultProgramEnd - opcoreExvmDefaultProgram

        .endsection

        .section bss, kind=bss

opcoreExprVmStack:
        .res long,OPCORE_EXPRVM_STACK_CAPACITY

        .endsection
        .endmodule
