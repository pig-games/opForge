; Native opcore-style one-line router for PRVM statement delegation.

        .module prvm.amigaos.line_router
        .cpu 68020
        .pub
        .use prvm.amigaos.interpreter (prvm_run_68000)

PRVM_REQUEST_FRAME_SIZE             = 112
PRVM_MAGIC_OPRP                     = $4F505250
PRVM_ABI_VERSION_V1                 = 1
PRVM_CALL_MODE_START                = 0
PRVM_ENTRY_KIND_OPASM_STATEMENT     = 1

PRVM_STATUS_OK                      = 0
PRVM_STATUS_NEWLINE_UNSUPPORTED     = 2
PRVM_STATUS_INVALID_ARGUMENT        = 4
PRVM_STATUS_UNSUPPORTED_ROUTE       = 100

PRVM_ROUTE_MAGIC_OPLR               = $4F504C52
PRVM_ROUTE_FRAME_SIZE               = 116
PRVM_ROUTE_ABI_VERSION_V1           = 1

ROUTE_FRAME_MAGIC                   = 0
ROUTE_FRAME_ABI_VERSION             = 4
ROUTE_FRAME_FRAME_SIZE              = 6
ROUTE_FRAME_PROCESSOR_PTR           = 8
ROUTE_FRAME_PROCESSOR_LEN           = 12
ROUTE_FRAME_KIND_PTR                = 16
ROUTE_FRAME_KIND_LEN                = 20
ROUTE_FRAME_LINE_NUM                = 24
ROUTE_FRAME_SOURCE_PTR              = 28
ROUTE_FRAME_SOURCE_LEN              = 32
ROUTE_FRAME_TOKEN_PTR               = 36
ROUTE_FRAME_TOKEN_COUNT             = 40
ROUTE_FRAME_TOKEN_RECORD_SIZE       = 44
ROUTE_FRAME_LEXEME_PTR              = 48
ROUTE_FRAME_LEXEME_LEN              = 52
ROUTE_FRAME_PROGRAM_PTR             = 56
ROUTE_FRAME_PROGRAM_LEN             = 60
ROUTE_FRAME_RESULT_PTR              = 64
ROUTE_FRAME_RESULT_CAPACITY         = 68
ROUTE_FRAME_DIAGNOSTIC_PTR          = 72
ROUTE_FRAME_DIAGNOSTIC_CAPACITY     = 76
ROUTE_FRAME_RESUME_PTR              = 80
ROUTE_FRAME_RESUME_CAPACITY         = 84
ROUTE_FRAME_EXPR_REQUEST_PTR        = 88
ROUTE_FRAME_EXPR_REQUEST_SIZE       = 92
ROUTE_FRAME_EXPR_RESULT_PTR         = 96
ROUTE_FRAME_EXPR_RESULT_COUNT       = 100
ROUTE_FRAME_PARSER_CONTRACT_VERSION = 104
ROUTE_FRAME_STEP_BUDGET             = 108
ROUTE_FRAME_FLAGS                   = 112

        .section code, kind=code

; ---------------------------------------------------------------------------
; Native opcore-style one-line router.
;
; Call ABI:
; - A0: PRVM_ROUTE_FRAME_V1 pointer
; - D0: route frame size in bytes
;
; Return ABI:
; - forwards D0-D3 from prvm_run_68000 on success
; - returns deterministic nonzero status with D1-D3 cleared on route failure
; ---------------------------------------------------------------------------

prvm_route_line_68000:
        MOVEM.L D4-D7/A2-A4, -(SP)
        MOVEA.L A0, A4

        CMPI.L #PRVM_ROUTE_FRAME_SIZE, D0
        BNE.W prvmRouteInvalidArgument
        CMPI.L #PRVM_ROUTE_MAGIC_OPLR, ROUTE_FRAME_MAGIC(A4)
        BNE.W prvmRouteInvalidArgument
        CMPI.W #PRVM_ROUTE_ABI_VERSION_V1, ROUTE_FRAME_ABI_VERSION(A4)
        BNE.W prvmRouteInvalidArgument
        CMPI.W #PRVM_ROUTE_FRAME_SIZE, ROUTE_FRAME_FRAME_SIZE(A4)
        BNE.W prvmRouteInvalidArgument

        MOVEA.L ROUTE_FRAME_PROCESSOR_PTR(A4), A0
        MOVE.L ROUTE_FRAME_PROCESSOR_LEN(A4), D0
        LEA processorAsmText(PC), A1
        MOVEQ #3, D1
        BSR.W prvmRouteCompareText
        TST.L D0
        BNE.W prvmRouteUnsupported

        MOVEA.L ROUTE_FRAME_KIND_PTR(A4), A0
        MOVE.L ROUTE_FRAME_KIND_LEN(A4), D0
        LEA kindStatementText(PC), A1
        MOVEQ #9, D1
        BSR.W prvmRouteCompareText
        TST.L D0
        BNE.W prvmRouteUnsupported

        MOVEA.L ROUTE_FRAME_SOURCE_PTR(A4), A0
        MOVE.L ROUTE_FRAME_SOURCE_LEN(A4), D0
        BSR.W prvmRouteRejectNewline
        TST.L D0
        BNE.W prvmRouteNewlineUnsupported

        BSR.W prvmRouteBuildRequestFrame
        LEA prvmRouteRequestFrame(PC), A0
        MOVE.L #PRVM_REQUEST_FRAME_SIZE, D0
        MOVEA.L prvmRouteInterpreterEntryPtr(PC), A1
        JSR (A1)
        BRA.S prvmRouteDone

prvmRouteInvalidArgument:
        MOVE.L #PRVM_STATUS_INVALID_ARGUMENT, D0
        BRA.S prvmRouteClearTail

prvmRouteUnsupported:
        MOVE.L #PRVM_STATUS_UNSUPPORTED_ROUTE, D0
        BRA.S prvmRouteClearTail

prvmRouteNewlineUnsupported:
        MOVE.L #PRVM_STATUS_NEWLINE_UNSUPPORTED, D0

prvmRouteClearTail:
        CLR.L D1
        CLR.L D2
        CLR.L D3

prvmRouteDone:
        MOVEM.L (SP)+, D4-D7/A2-A4
        RTS

prvmRouteCompareText:
        CMP.L D1, D0
        BNE.S prvmRouteCompareMismatch
        SUBQ.L #1, D1
        BMI.S prvmRouteCompareMatch

prvmRouteCompareLoop:
        MOVE.B (A0)+, D2
        CMP.B (A1)+, D2
        BNE.S prvmRouteCompareMismatch
        DBRA D1, prvmRouteCompareLoop

prvmRouteCompareMatch:
        CLR.L D0
        RTS

prvmRouteCompareMismatch:
        MOVEQ #1, D0
        RTS

prvmRouteRejectNewline:
        TST.L D0
        BEQ.S prvmRouteNoNewline
        SUBQ.L #1, D0

prvmRouteNewlineLoop:
        MOVE.B (A0)+, D1
        CMPI.B #10, D1
        BEQ.S prvmRouteFoundNewline
        CMPI.B #13, D1
        BEQ.S prvmRouteFoundNewline
        DBRA D0, prvmRouteNewlineLoop

prvmRouteNoNewline:
        CLR.L D0
        RTS

prvmRouteFoundNewline:
        MOVEQ #1, D0
        RTS

prvmRouteBuildRequestFrame:
        LEA prvmRouteRequestFrame(PC), A0
        MOVE.L #PRVM_MAGIC_OPRP, 0(A0)
        MOVE.W #PRVM_ABI_VERSION_V1, 4(A0)
        MOVE.W #PRVM_REQUEST_FRAME_SIZE, 6(A0)
        MOVE.W #PRVM_CALL_MODE_START, 8(A0)
        MOVE.W #PRVM_ENTRY_KIND_OPASM_STATEMENT, 10(A0)
        MOVE.L ROUTE_FRAME_LINE_NUM(A4), 12(A0)
        MOVE.L ROUTE_FRAME_SOURCE_PTR(A4), 16(A0)
        MOVE.L ROUTE_FRAME_SOURCE_LEN(A4), 20(A0)
        MOVE.L ROUTE_FRAME_TOKEN_PTR(A4), 24(A0)
        MOVE.L ROUTE_FRAME_TOKEN_COUNT(A4), 28(A0)
        MOVE.W ROUTE_FRAME_TOKEN_RECORD_SIZE(A4), 32(A0)
        CLR.W 34(A0)
        MOVE.L ROUTE_FRAME_LEXEME_PTR(A4), 36(A0)
        MOVE.L ROUTE_FRAME_LEXEME_LEN(A4), 40(A0)
        MOVE.L ROUTE_FRAME_PROGRAM_PTR(A4), 44(A0)
        MOVE.L ROUTE_FRAME_PROGRAM_LEN(A4), 48(A0)
        MOVE.L ROUTE_FRAME_RESULT_PTR(A4), 52(A0)
        MOVE.L ROUTE_FRAME_RESULT_CAPACITY(A4), 56(A0)
        MOVE.L ROUTE_FRAME_DIAGNOSTIC_PTR(A4), 60(A0)
        MOVE.L ROUTE_FRAME_DIAGNOSTIC_CAPACITY(A4), 64(A0)
        MOVE.L ROUTE_FRAME_RESUME_PTR(A4), 68(A0)
        MOVE.L ROUTE_FRAME_RESUME_CAPACITY(A4), 72(A0)
        MOVE.L ROUTE_FRAME_EXPR_REQUEST_PTR(A4), 76(A0)
        MOVE.L ROUTE_FRAME_EXPR_REQUEST_SIZE(A4), 80(A0)
        MOVE.L ROUTE_FRAME_EXPR_RESULT_PTR(A4), 84(A0)
        MOVE.L ROUTE_FRAME_EXPR_RESULT_COUNT(A4), 88(A0)
        MOVE.L ROUTE_FRAME_PARSER_CONTRACT_VERSION(A4), 92(A0)
        MOVE.L ROUTE_FRAME_STEP_BUDGET(A4), 96(A0)
        MOVE.L ROUTE_FRAME_FLAGS(A4), 100(A0)
        CLR.L 104(A0)
        CLR.L 108(A0)
        RTS

processorAsmText:
        .byte "asm"
kindStatementText:
        .byte "statement"

prvmRouteRequestFrame:
        .fill byte, 112, 0
prvmRouteInterpreterEntryPtr:
        .long prvm_run_68000

        .endsection
        .endmodule
