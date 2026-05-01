; Native whole-file iterator over newline-free PRVM line routes.

        .module prvm.amigaos.line_iterator
        .cpu 68020
        .pub
        .use prvm.amigaos.line_router (prvm_route_line_68000)

PRVM_ROUTE_MAGIC_OPLR               = $4F504C52
PRVM_ROUTE_FRAME_SIZE               = 116
PRVM_ROUTE_ABI_VERSION_V1           = 1

PRVM_ITER_MAGIC_OPLI                = $4F504C49
PRVM_ITER_FRAME_SIZE                = 116
PRVM_ITER_ABI_VERSION_V1            = 1

PRVM_ITER_STATUS_OK                 = 0
PRVM_ITER_STATUS_INVALID_ARGUMENT   = 4

ITER_FRAME_MAGIC                    = 0
ITER_FRAME_ABI_VERSION              = 4
ITER_FRAME_FRAME_SIZE               = 6
ITER_FRAME_PROCESSOR_PTR            = 8
ITER_FRAME_PROCESSOR_LEN            = 12
ITER_FRAME_KIND_PTR                 = 16
ITER_FRAME_KIND_LEN                 = 20
ITER_FRAME_START_LINE_NUM           = 24
ITER_FRAME_SOURCE_PTR               = 28
ITER_FRAME_SOURCE_LEN               = 32
ITER_FRAME_TOKEN_PTR                = 36
ITER_FRAME_TOKEN_COUNT              = 40
ITER_FRAME_TOKEN_RECORD_SIZE        = 44
ITER_FRAME_LEXEME_PTR               = 48
ITER_FRAME_LEXEME_LEN               = 52
ITER_FRAME_PROGRAM_PTR              = 56
ITER_FRAME_PROGRAM_LEN              = 60
ITER_FRAME_RESULT_PTR               = 64
ITER_FRAME_RESULT_CAPACITY          = 68
ITER_FRAME_DIAGNOSTIC_PTR           = 72
ITER_FRAME_DIAGNOSTIC_CAPACITY      = 76
ITER_FRAME_RESUME_PTR               = 80
ITER_FRAME_RESUME_CAPACITY          = 84
ITER_FRAME_EXPR_REQUEST_PTR         = 88
ITER_FRAME_EXPR_REQUEST_SIZE        = 92
ITER_FRAME_EXPR_RESULT_PTR          = 96
ITER_FRAME_EXPR_RESULT_COUNT        = 100
ITER_FRAME_PARSER_CONTRACT_VERSION  = 104
ITER_FRAME_STEP_BUDGET              = 108
ITER_FRAME_FLAGS                    = 112

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
; Native whole-file iterator.
;
; Call ABI:
; - A0: PRVM_ITER_FRAME_V1 pointer
; - D0: iterator frame size in bytes
;
; Return ABI:
; - D0: route status or zero when every routed line succeeds
; - D1: number of nonblank routed lines
; - D2: one-based line number for the first failing line, or zero
; - D3: total logical line count observed
; ---------------------------------------------------------------------------

prvm_iterate_lines_68000:
        MOVEM.L D4-D7/A2-A6, -(SP)
        MOVEA.L A0, A6
        CLR.L D5
        CLR.L D6
        CLR.L D7

        CMPI.L #PRVM_ITER_FRAME_SIZE, D0
        BNE.W prvmIteratorInvalidArgument
        CMPI.L #PRVM_ITER_MAGIC_OPLI, ITER_FRAME_MAGIC(A6)
        BNE.W prvmIteratorInvalidArgument
        CMPI.W #PRVM_ITER_ABI_VERSION_V1, ITER_FRAME_ABI_VERSION(A6)
        BNE.W prvmIteratorInvalidArgument
        CMPI.W #PRVM_ITER_FRAME_SIZE, ITER_FRAME_FRAME_SIZE(A6)
        BNE.W prvmIteratorInvalidArgument

        MOVE.L ITER_FRAME_START_LINE_NUM(A6), D6
        TST.L D6
        BNE.S prvmIteratorStartLineReady
        MOVEQ #1, D6

prvmIteratorStartLineReady:
        MOVEA.L ITER_FRAME_SOURCE_PTR(A6), A2
        MOVE.L ITER_FRAME_SOURCE_LEN(A6), D4

prvmIteratorNextLine:
        TST.L D4
        BEQ.W prvmIteratorSuccess
        MOVEA.L A2, A3
        MOVE.L D4, D0
        BSR.W prvmIteratorFindLineEnd
        MOVE.L D0, D3
        MOVE.L D1, D4
        MOVEA.L A2, A0
        MOVE.L D3, D0
        BSR.W prvmIteratorTrimCr
        MOVE.L D0, D3
        MOVEA.L A2, A0
        MOVE.L D3, D0
        BSR.W prvmIteratorLineIsBlank
        TST.L D0
        BNE.S prvmIteratorSkipRoute

        MOVEA.L A2, A0
        MOVE.L D3, D0
        BSR.W prvmIteratorBuildRouteFrame
        LEA prvmIteratorRouteFrame(PC), A0
        MOVE.L #PRVM_ROUTE_FRAME_SIZE, D0
        MOVEA.L prvmIteratorRouteEntryPtr(PC), A1
        JSR (A1)
        TST.L D0
        BNE.W prvmIteratorFailFast
        ADDQ.L #1, D5

prvmIteratorSkipRoute:
        MOVEA.L A3, A2
        ADDQ.L #1, D6
        BRA.W prvmIteratorNextLine

prvmIteratorSuccess:
        MOVE.L #PRVM_ITER_STATUS_OK, D0
        MOVE.L D5, D1
        CLR.L D2
        MOVE.L D6, D3
        SUBQ.L #1, D3
        BRA.S prvmIteratorDone

prvmIteratorFailFast:
        MOVE.L D5, D1
        MOVE.L D6, D2
        MOVE.L D6, D3
        BRA.S prvmIteratorDone

prvmIteratorInvalidArgument:
        MOVE.L #PRVM_ITER_STATUS_INVALID_ARGUMENT, D0
        CLR.L D1
        CLR.L D2
        CLR.L D3

prvmIteratorDone:
        MOVEM.L (SP)+, D4-D7/A2-A6
        RTS

prvmIteratorFindLineEnd:
        CLR.L D1

prvmIteratorFindLineLoop:
        TST.L D0
        BEQ.S prvmIteratorFindDone
        MOVE.B (A3)+, D2
        SUBQ.L #1, D0
        CMPI.B #10, D2
        BEQ.S prvmIteratorFindDone
        ADDQ.L #1, D1
        BRA.S prvmIteratorFindLineLoop

prvmIteratorFindDone:
        MOVE.L D0, D2
        MOVE.L D1, D0
        MOVE.L D2, D1
        RTS

prvmIteratorTrimCr:
        TST.L D0
        BEQ.S prvmIteratorTrimDone
        MOVEA.L A0, A1
        ADDA.L D0, A1
        SUBQ.L #1, A1
        CMPI.B #13, (A1)
        BNE.S prvmIteratorTrimDone
        SUBQ.L #1, D0

prvmIteratorTrimDone:
        RTS

prvmIteratorLineIsBlank:
        TST.L D0
        BEQ.S prvmIteratorBlank
        SUBQ.L #1, D0

prvmIteratorBlankLoop:
        MOVE.B (A0)+, D2
        CMPI.B #32, D2
        BEQ.S prvmIteratorBlankNext
        CMPI.B #9, D2
        BNE.S prvmIteratorNotBlank

prvmIteratorBlankNext:
        DBRA D0, prvmIteratorBlankLoop

prvmIteratorBlank:
        MOVEQ #1, D0
        RTS

prvmIteratorNotBlank:
        CLR.L D0
        RTS

prvmIteratorBuildRouteFrame:
        LEA prvmIteratorRouteFrame(PC), A1
        MOVE.L #PRVM_ROUTE_MAGIC_OPLR, ROUTE_FRAME_MAGIC(A1)
        MOVE.W #PRVM_ROUTE_ABI_VERSION_V1, ROUTE_FRAME_ABI_VERSION(A1)
        MOVE.W #PRVM_ROUTE_FRAME_SIZE, ROUTE_FRAME_FRAME_SIZE(A1)
        MOVE.L ITER_FRAME_PROCESSOR_PTR(A6), ROUTE_FRAME_PROCESSOR_PTR(A1)
        MOVE.L ITER_FRAME_PROCESSOR_LEN(A6), ROUTE_FRAME_PROCESSOR_LEN(A1)
        MOVE.L ITER_FRAME_KIND_PTR(A6), ROUTE_FRAME_KIND_PTR(A1)
        MOVE.L ITER_FRAME_KIND_LEN(A6), ROUTE_FRAME_KIND_LEN(A1)
        MOVE.L D6, ROUTE_FRAME_LINE_NUM(A1)
        MOVE.L A0, ROUTE_FRAME_SOURCE_PTR(A1)
        MOVE.L D0, ROUTE_FRAME_SOURCE_LEN(A1)
        MOVE.L ITER_FRAME_TOKEN_PTR(A6), ROUTE_FRAME_TOKEN_PTR(A1)
        MOVE.L ITER_FRAME_TOKEN_COUNT(A6), ROUTE_FRAME_TOKEN_COUNT(A1)
        MOVE.W ITER_FRAME_TOKEN_RECORD_SIZE(A6), ROUTE_FRAME_TOKEN_RECORD_SIZE(A1)
        CLR.W 46(A1)
        MOVE.L ITER_FRAME_LEXEME_PTR(A6), ROUTE_FRAME_LEXEME_PTR(A1)
        MOVE.L ITER_FRAME_LEXEME_LEN(A6), ROUTE_FRAME_LEXEME_LEN(A1)
        MOVE.L ITER_FRAME_PROGRAM_PTR(A6), ROUTE_FRAME_PROGRAM_PTR(A1)
        MOVE.L ITER_FRAME_PROGRAM_LEN(A6), ROUTE_FRAME_PROGRAM_LEN(A1)
        MOVE.L ITER_FRAME_RESULT_PTR(A6), ROUTE_FRAME_RESULT_PTR(A1)
        MOVE.L ITER_FRAME_RESULT_CAPACITY(A6), ROUTE_FRAME_RESULT_CAPACITY(A1)
        MOVE.L ITER_FRAME_DIAGNOSTIC_PTR(A6), ROUTE_FRAME_DIAGNOSTIC_PTR(A1)
        MOVE.L ITER_FRAME_DIAGNOSTIC_CAPACITY(A6), ROUTE_FRAME_DIAGNOSTIC_CAPACITY(A1)
        MOVE.L ITER_FRAME_RESUME_PTR(A6), ROUTE_FRAME_RESUME_PTR(A1)
        MOVE.L ITER_FRAME_RESUME_CAPACITY(A6), ROUTE_FRAME_RESUME_CAPACITY(A1)
        MOVE.L ITER_FRAME_EXPR_REQUEST_PTR(A6), ROUTE_FRAME_EXPR_REQUEST_PTR(A1)
        MOVE.L ITER_FRAME_EXPR_REQUEST_SIZE(A6), ROUTE_FRAME_EXPR_REQUEST_SIZE(A1)
        MOVE.L ITER_FRAME_EXPR_RESULT_PTR(A6), ROUTE_FRAME_EXPR_RESULT_PTR(A1)
        MOVE.L ITER_FRAME_EXPR_RESULT_COUNT(A6), ROUTE_FRAME_EXPR_RESULT_COUNT(A1)
        MOVE.L ITER_FRAME_PARSER_CONTRACT_VERSION(A6), ROUTE_FRAME_PARSER_CONTRACT_VERSION(A1)
        MOVE.L ITER_FRAME_STEP_BUDGET(A6), ROUTE_FRAME_STEP_BUDGET(A1)
        MOVE.L ITER_FRAME_FLAGS(A6), ROUTE_FRAME_FLAGS(A1)
        RTS

prvmIteratorRouteFrame:
        .fill byte, 116, 0
prvmIteratorRouteEntryPtr:
        .long prvm_route_line_68000

        .endsection
        .endmodule