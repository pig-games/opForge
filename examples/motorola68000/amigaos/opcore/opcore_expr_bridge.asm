; Native opcore/EXVM-style scalar operand expression bridge.

        .module opcore.amigaos.expr_bridge
        .cpu 68020
        .pub

TOKEN_BUFFER_CAPACITY           = 64

        .section code, kind=code

opcore_expr_eval_operand_v1:
        MOVEM.L D1-D2/D4-D7/A0-A4, -(SP)
        MOVEA.L A1, A3
        MOVEA.L A2, A4
        MOVE.W D1, D7
        CLR.L D3
        BSR.W opcoreExprBridgeSkipWhitespace
        TST.L D0
        BEQ.W opcoreExprBridgeFail
        CMPI.B #'#', (A0)
        BNE.S opcoreExprBridgeNoImmediatePrefix
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opcoreExprBridgeSkipWhitespace

opcoreExprBridgeNoImmediatePrefix:
        TST.L D0
        BEQ.W opcoreExprBridgeFail
        CMPI.B #'$', (A0)
        BEQ.S opcoreExprBridgeHex
        MOVEQ #0, D1
        MOVE.B (A0), D1
        CMPI.B #'0', D1
        BCS.S opcoreExprBridgeLabel
        CMPI.B #'9', D1
        BHI.S opcoreExprBridgeLabel
        BRA.S opcoreExprBridgeDecimal

opcoreExprBridgeHex:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opcoreExprBridgeParseHex
        BRA.S opcoreExprBridgeReturn

opcoreExprBridgeDecimal:
        BSR.W opcoreExprBridgeParseDecimal
        BRA.S opcoreExprBridgeReturn

opcoreExprBridgeLabel:
        BSR.W opcoreExprBridgeResolveLabel
        BRA.S opcoreExprBridgeReturn

opcoreExprBridgeFail:
        MOVEQ #1, D0

opcoreExprBridgeReturn:
        MOVEM.L (SP)+, D1-D2/D4-D7/A0-A4
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

opcoreExprBridgeParseHex:
        MOVEM.L D1-D2/A0, -(SP)
        CLR.L D3

opcoreExprBridgeParseHexLoop:
        TST.L D0
        BEQ.S opcoreExprBridgeParseHexOk
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        SUBQ.L #1, D0
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
        MOVEQ #0, D0
        BRA.S opcoreExprBridgeParseHexReturn

opcoreExprBridgeParseHexFail:
        MOVEQ #1, D0

opcoreExprBridgeParseHexReturn:
        MOVEM.L (SP)+, D1-D2/A0
        RTS

opcoreExprBridgeParseDecimal:
        MOVEM.L D1-D2/A0, -(SP)
        CLR.L D3

opcoreExprBridgeParseDecimalLoop:
        TST.L D0
        BEQ.S opcoreExprBridgeParseDecimalOk
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        SUBQ.L #1, D0
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
        MOVEQ #0, D0
        BRA.S opcoreExprBridgeParseDecimalReturn

opcoreExprBridgeParseDecimalFail:
        MOVEQ #1, D0

opcoreExprBridgeParseDecimalReturn:
        MOVEM.L (SP)+, D1-D2/A0
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

        .endsection
        .endmodule