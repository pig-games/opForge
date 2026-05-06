; Native opasm selector/request staging for the initial AmigaOS CLI slice.

        .module opasm.amigaos.selector_stage
        .cpu 68020
        .pub
        .use opcore.amigaos.expr_bridge (opcore_expr_eval_operand_v1)

OPASM_SELECTOR_STATUS_OK                  = 0
OPASM_SELECTOR_STATUS_NO_OUTPUT           = 1
OPASM_SELECTOR_STATUS_UNKNOWN_MNEMONIC    = 2
OPASM_SELECTOR_STATUS_UNSUPPORTED_ADDRESS = 3
OPASM_SELECTOR_STATUS_OPERAND_ERROR       = 4

        .section code, kind=code

opasm_selector_stage_build_encode_request_v1:
        MOVEM.L D3-D7/A2-A6, -(SP)
        MOVEA.L A0, A5
        MOVEQ #0, D6
        MOVE.W D0, D6
        MOVEA.L A1, A6
        MOVEQ #0, D7
        MOVE.W D1, D7
        MOVEA.L A4, A0
        MOVEA.L (A0)+, A4
        MOVEA.L (A0)+, A2
        MOVEA.L (A0)+, A3
        MOVE.L (A0), D2
        MOVEA.L A4, A0
        TST.W D6
        BEQ.W opasmSelectorNoOutput
        MOVEA.L A5, A0
        MOVE.W D6, D0
        LEA opasmSelectorLdaText, A1
        MOVEQ #3, D1
        BSR.W opasmSelectorTextEquals
        TST.L D0
        BNE.W opasmSelectorBuildImmediate
        MOVEA.L A5, A0
        MOVE.W D6, D0
        LEA opasmSelectorStaText, A1
        MOVEQ #3, D1
        BSR.W opasmSelectorTextEquals
        TST.L D0
        BNE.W opasmSelectorBuildAbsolute
        MOVEA.L A5, A0
        MOVE.W D6, D0
        LEA opasmSelectorJmpText, A1
        MOVEQ #3, D1
        BSR.W opasmSelectorTextEquals
        TST.L D0
        BNE.W opasmSelectorBuildAbsolute
        MOVEQ #OPASM_SELECTOR_STATUS_UNKNOWN_MNEMONIC, D0
        BRA.W opasmSelectorReturn

opasmSelectorBuildImmediate:
        MOVEA.L A6, A0
        MOVE.L D7, D0
        BSR.W opasmSelectorOperandHasImmediatePrefix
        TST.L D0
        BNE.S opasmSelectorImmediateOk
        MOVEQ #OPASM_SELECTOR_STATUS_UNSUPPORTED_ADDRESS, D0
        BRA.W opasmSelectorReturn

opasmSelectorImmediateOk:
        MOVEQ #1, D5
        MOVEQ #9, D4
        BSR.W opasmSelectorReadOperandValue
        TST.L D0
        BNE.W opasmSelectorOperandError
        LEA opasmSelectorImmediateText, A6
        BRA.S opasmSelectorBuildPayload

opasmSelectorBuildAbsolute:
        MOVEQ #2, D5
        MOVEQ #8, D4
        BSR.W opasmSelectorReadOperandValue
        TST.L D0
        BNE.W opasmSelectorOperandError
        LEA opasmSelectorAbsoluteText, A6

opasmSelectorBuildPayload:
        MOVEA.L A4, A2
        MOVE.B D6, (A2)+
        MOVEA.L A5, A0
        MOVEA.L A2, A1
        MOVE.W D6, D0
        BSR.W opasmSelectorCopyFixedString
        MOVEA.L A1, A2
        MOVE.B #1, (A2)+
        MOVE.B D4, (A2)+
        MOVEA.L A6, A0
        MOVEA.L A2, A1
        MOVE.W D4, D0
        BSR.W opasmSelectorCopyFixedString
        MOVEA.L A1, A2
        MOVE.B #1, (A2)+
        MOVE.B D5, (A2)+
        MOVE.B D3, (A2)+
        CMPI.B #2, D5
        BNE.S opasmSelectorBuildPayloadLenDone
        MOVE.L D3, D0
        LSR.L #8, D0
        MOVE.B D0, (A2)+

opasmSelectorBuildPayloadLenDone:
        MOVE.L A2, D1
        SUB.L A4, D1
        MOVEQ #OPASM_SELECTOR_STATUS_OK, D0
        BRA.S opasmSelectorReturn

opasmSelectorNoOutput:
        MOVEQ #0, D1
        MOVEQ #OPASM_SELECTOR_STATUS_NO_OUTPUT, D0
        BRA.S opasmSelectorReturn

opasmSelectorOperandError:
        MOVEQ #OPASM_SELECTOR_STATUS_OPERAND_ERROR, D0

opasmSelectorReturn:
        MOVEM.L (SP)+, D3-D7/A2-A6
        RTS

opasm_selector_stage_instruction_size_v1:
        MOVEM.L D2/A0-A2, -(SP)
        MOVEQ #0, D2
        MOVE.W D0, D2
        MOVEA.L A0, A2
        MOVEQ #0, D1
        TST.W D2
        BEQ.S opasmSelectorInstructionSizeDone
        MOVEA.L A2, A0
        MOVE.W D2, D0
        LEA opasmSelectorLdaText, A1
        MOVEQ #3, D1
        BSR.W opasmSelectorTextEquals
        TST.L D0
        BNE.S opasmSelectorInstructionSizeTwo
        MOVEA.L A2, A0
        MOVE.W D2, D0
        LEA opasmSelectorNopText, A1
        MOVEQ #3, D1
        BSR.W opasmSelectorTextEquals
        TST.L D0
        BNE.S opasmSelectorInstructionSizeOne
        MOVEA.L A2, A0
        MOVE.W D2, D0
        LEA opasmSelectorStaText, A1
        MOVEQ #3, D1
        BSR.W opasmSelectorTextEquals
        TST.L D0
        BNE.S opasmSelectorInstructionSizeThree
        MOVEA.L A2, A0
        MOVE.W D2, D0
        LEA opasmSelectorJmpText, A1
        MOVEQ #3, D1
        BSR.W opasmSelectorTextEquals
        TST.L D0
        BNE.S opasmSelectorInstructionSizeThree
        BRA.S opasmSelectorInstructionSizeDone

opasmSelectorInstructionSizeOne:
        MOVEQ #1, D1
        BRA.S opasmSelectorInstructionSizeDone

opasmSelectorInstructionSizeTwo:
        MOVEQ #2, D1
        BRA.S opasmSelectorInstructionSizeDone

opasmSelectorInstructionSizeThree:
        MOVEQ #3, D1

opasmSelectorInstructionSizeDone:
        MOVEQ #OPASM_SELECTOR_STATUS_OK, D0
        MOVEM.L (SP)+, D2/A0-A2
        RTS

opasmSelectorReadOperandValue:
        MOVEM.L D1-D2/D4-D7/A0-A2, -(SP)
        MOVEA.L A6, A0
        MOVE.L D7, D0
        BSR.W opasmSelectorResolveLabelOperand
        TST.L D0
        BEQ.S opasmSelectorReadOperandHaveValue
        BRA.S opasmSelectorReadOperandFail

opasmSelectorReadOperandHaveValue:
        CMPI.B #1, D5
        BNE.S opasmSelectorReadOperandOk
        CMPI.L #$000000FF, D3
        BHI.S opasmSelectorReadOperandFail

opasmSelectorReadOperandOk:
        MOVEQ #0, D0
        BRA.S opasmSelectorReadOperandReturn

opasmSelectorReadOperandFail:
        MOVEQ #1, D0

opasmSelectorReadOperandReturn:
        MOVEM.L (SP)+, D1-D2/D4-D7/A0-A2
        RTS

opasmSelectorResolveLabelOperand:
        MOVEM.L D1/A0, -(SP)
        MOVEA.L A6, A0
        MOVE.L D7, D0
        BSR.W opasmSelectorSkipWhitespace
        TST.L D0
        BEQ.S opasmSelectorResolveLabelFail
        CMPI.B #'#', (A0)
        BNE.S opasmSelectorResolveNoImmediatePrefix
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opasmSelectorSkipWhitespace
        TST.L D0
        BEQ.S opasmSelectorResolveLabelFail

opasmSelectorResolveNoImmediatePrefix:
        CMPI.B #'$', (A0)
        BEQ.S opasmSelectorResolveHex
        MOVEQ #0, D1
        MOVE.B (A0), D1
        CMPI.B #'0', D1
        BCS.S opasmSelectorResolveLabelText
        CMPI.B #'9', D1
        BHI.S opasmSelectorResolveLabelText
        BSR.W opasmSelectorParseDecimal
        BRA.S opasmSelectorResolveLabelReturn

opasmSelectorResolveHex:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opasmSelectorParseHex
        BRA.S opasmSelectorResolveLabelReturn

opasmSelectorResolveLabelText:
        MOVEM.L D1/A1-A3, -(SP)
        MOVEA.L A2, A1
        MOVEA.L A3, A2
        MOVE.L D2, D1
        JSR opcore_expr_eval_operand_v1
        MOVEM.L (SP)+, D1/A1-A3
        BRA.S opasmSelectorResolveLabelReturn

opasmSelectorResolveLabelFail:
        MOVEQ #1, D0

opasmSelectorResolveLabelReturn:
        MOVEM.L (SP)+, D1/A0
        RTS

opasmSelectorParseHex:
        MOVEM.L D1-D2/A0, -(SP)
        CLR.L D3

opasmSelectorParseHexLoop:
        TST.L D0
        BEQ.S opasmSelectorParseHexOk
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        SUBQ.L #1, D0
        TST.B D1
        BEQ.S opasmSelectorParseHexOk
        CMPI.B #' ', D1
        BEQ.S opasmSelectorParseHexOk
        CMPI.B #9, D1
        BEQ.S opasmSelectorParseHexOk
        CMPI.B #'0', D1
        BCS.S opasmSelectorParseHexFail
        CMPI.B #'9', D1
        BLS.S opasmSelectorParseHexDigit
        CMPI.B #'A', D1
        BCS.S opasmSelectorParseHexLower
        CMPI.B #'F', D1
        BHI.S opasmSelectorParseHexLower
        SUBI.B #'A' - 10, D1
        BRA.S opasmSelectorParseHexHaveDigit

opasmSelectorParseHexLower:
        CMPI.B #'a', D1
        BCS.S opasmSelectorParseHexFail
        CMPI.B #'f', D1
        BHI.S opasmSelectorParseHexFail
        SUBI.B #'a' - 10, D1
        BRA.S opasmSelectorParseHexHaveDigit

opasmSelectorParseHexDigit:
        SUBI.B #'0', D1

opasmSelectorParseHexHaveDigit:
        MOVE.L D3, D2
        LSL.L #4, D3
        OR.L D1, D3
        BRA.S opasmSelectorParseHexLoop

opasmSelectorParseHexOk:
        MOVEQ #0, D0
        BRA.S opasmSelectorParseHexReturn

opasmSelectorParseHexFail:
        MOVEQ #1, D0

opasmSelectorParseHexReturn:
        MOVEM.L (SP)+, D1-D2/A0
        RTS

opasmSelectorParseDecimal:
        MOVEM.L D1-D2/A0, -(SP)
        CLR.L D3

opasmSelectorParseDecimalLoop:
        TST.L D0
        BEQ.S opasmSelectorParseDecimalOk
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        SUBQ.L #1, D0
        TST.B D1
        BEQ.S opasmSelectorParseDecimalOk
        CMPI.B #' ', D1
        BEQ.S opasmSelectorParseDecimalOk
        CMPI.B #9, D1
        BEQ.S opasmSelectorParseDecimalOk
        CMPI.B #'0', D1
        BCS.S opasmSelectorParseDecimalFail
        CMPI.B #'9', D1
        BHI.S opasmSelectorParseDecimalFail
        SUBI.B #'0', D1
        MOVE.L D3, D2
        LSL.L #3, D3
        ADD.L D2, D3
        ADD.L D2, D3
        ADD.L D1, D3
        BRA.S opasmSelectorParseDecimalLoop

opasmSelectorParseDecimalOk:
        MOVEQ #0, D0
        BRA.S opasmSelectorParseDecimalReturn

opasmSelectorParseDecimalFail:
        MOVEQ #1, D0

opasmSelectorParseDecimalReturn:
        MOVEM.L (SP)+, D1-D2/A0
        RTS

opasmSelectorOperandHasImmediatePrefix:
        BSR.W opasmSelectorSkipWhitespace
        TST.L D0
        BEQ.S opasmSelectorOperandImmediateNo
        CMPI.B #'#', (A0)
        BNE.S opasmSelectorOperandImmediateNo
        MOVEQ #1, D0
        RTS

opasmSelectorOperandImmediateNo:
        MOVEQ #0, D0
        RTS

opasmSelectorTextEquals:
        MOVEM.L D2-D3, -(SP)
        CMP.L D1, D0
        BNE.S opasmSelectorTextNotEqual
        TST.L D1
        BEQ.S opasmSelectorTextEqual

opasmSelectorTextLoop:
        MOVE.B (A0)+, D2
        MOVE.B (A1)+, D3
        CMPI.B #'A', D2
        BCS.S opasmSelectorTextSourceCaseOk
        CMPI.B #'Z', D2
        BHI.S opasmSelectorTextSourceCaseOk
        ADDI.B #'a' - 'A', D2

opasmSelectorTextSourceCaseOk:
        CMPI.B #'A', D3
        BCS.S opasmSelectorTextNeedleCaseOk
        CMPI.B #'Z', D3
        BHI.S opasmSelectorTextNeedleCaseOk
        ADDI.B #'a' - 'A', D3

opasmSelectorTextNeedleCaseOk:
        CMP.B D3, D2
        BNE.S opasmSelectorTextNotEqual
        SUBQ.L #1, D1
        BNE.S opasmSelectorTextLoop

opasmSelectorTextEqual:
        MOVEQ #1, D0
        BRA.S opasmSelectorTextReturn

opasmSelectorTextNotEqual:
        MOVEQ #0, D0

opasmSelectorTextReturn:
        MOVEM.L (SP)+, D2-D3
        RTS

opasmSelectorSkipWhitespace:
        TST.L D0
        BEQ.S opasmSelectorSkipWhitespaceDone
        MOVEQ #0, D1
        MOVE.B (A0), D1
        CMPI.B #' ', D1
        BEQ.S opasmSelectorSkipWhitespaceOne
        CMPI.B #9, D1
        BNE.S opasmSelectorSkipWhitespaceDone

opasmSelectorSkipWhitespaceOne:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BRA.S opasmSelectorSkipWhitespace

opasmSelectorSkipWhitespaceDone:
        RTS

opasmSelectorCopyFixedString:
        TST.W D0
        BEQ.S opasmSelectorCopyFixedDone

opasmSelectorCopyFixedLoop:
        MOVE.B (A0)+, (A1)+
        SUBQ.W #1, D0
        BNE.S opasmSelectorCopyFixedLoop

opasmSelectorCopyFixedDone:
        RTS

        .endsection

        .section data, kind=data

opasmSelectorStageMarker:
        .byte "OPASM-SELECTOR-STAGE-V1", 0

opasmSelectorLdaText:
        .byte "lda"
opasmSelectorStaText:
        .byte "sta"
opasmSelectorJmpText:
        .byte "jmp"
opasmSelectorNopText:
        .byte "nop"
opasmSelectorImmediateText:
        .byte "immediate"
opasmSelectorAbsoluteText:
        .byte "absolute"

        .endsection
        .endmodule
