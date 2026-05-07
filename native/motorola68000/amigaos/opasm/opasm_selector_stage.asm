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

; ---------------------------------------------------------------------------
; Build the compact tkpkg encode-request payload for one parsed statement.
;
; This is a transitional native opasm entry: it stands where the full selector
; will eventually own CPU/addressing candidate selection. For the current smoke
; path it accepts the small 6502 subset and emits the package-service request
; envelope consumed by ENTRY_ORD_ENCODE_INSTRUCTION.
;
; Inputs:
; - A0/D0: mnemonic pointer and byte length.
; - A1/D1: operand text pointer and byte length.
; - A4: selector context pointer containing:
;   - long 0: output request buffer pointer.
;   - long 1: label-name table pointer.
;   - long 2: label-value table pointer.
;   - long 3: label count.
;   - long 4: current assembly PC for opcore expression evaluation.
;
; Outputs:
; - D0: OPASM_SELECTOR_STATUS_*.
; - D1: request byte length when D0 is OPASM_SELECTOR_STATUS_OK.
; - request buffer contains the compact package encode envelope.
; ---------------------------------------------------------------------------

opasm_selector_stage_build_encode_request_v1:
        MOVEM.L D3-D7/A2-A6, -(SP)
        MOVEA.L A0, A5                  ; mnemonic text base survives helper calls in A5
        MOVEQ #0, D6
        MOVE.W D0, D6                   ; D6 is the mnemonic length used in string compare/copy
        MOVEA.L A1, A6                  ; operand text base; later reused by operand evaluators
        MOVEQ #0, D7
        MOVE.W D1, D7                   ; D7 is the operand text length
        MOVEA.L A4, A0                  ; unpack the selector context supplied by the CLI/session
        MOVEA.L (A0)+, A4               ; A4 becomes the output request buffer
        MOVEA.L (A0)+, A2               ; A2 points at fixed-width label names
        MOVEA.L (A0)+, A3               ; A3 points at label values parallel to A2
        MOVE.L (A0)+, D2                ; D2 is the number of live label entries
        MOVE.L (A0), D3                 ; D3 carries current PC until opcore returns an operand value
        MOVEA.L A4, A0                  ; restore output-buffer base for later payload construction
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

; ---------------------------------------------------------------------------
; Return the byte length for one currently supported statement mnemonic.
;
; This is pass-1 sizing support for the same smoke subset as
; opasm_selector_stage_build_encode_request_v1. Unknown or non-output
; statements return size 0 so the caller can leave PC unchanged.
;
; Inputs:
; - A0/D0: mnemonic pointer and byte length.
;
; Outputs:
; - D0: OPASM_SELECTOR_STATUS_OK.
; - D1: instruction size in bytes, or 0 for no output/unknown.
; ---------------------------------------------------------------------------

opasm_selector_stage_instruction_size_v1:
        MOVEM.L D2/A0-A2, -(SP)
        MOVEQ #0, D2
        MOVE.W D0, D2                   ; preserve caller's mnemonic length for repeated compares
        MOVEA.L A0, A2                  ; preserve caller's mnemonic pointer for repeated compares
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
        MOVEM.L D1/A1-A3, -(SP)
        MOVEA.L A2, A1
        MOVEA.L A3, A2
        MOVE.L D2, D1
        MOVE.L D3, D2
        JSR opcore_expr_eval_operand_v1
        MOVEM.L (SP)+, D1/A1-A3
        BRA.S opasmSelectorResolveLabelReturn

opasmSelectorResolveLabelFail:
        MOVEQ #1, D0

opasmSelectorResolveLabelReturn:
        MOVEM.L (SP)+, D1/A0
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
