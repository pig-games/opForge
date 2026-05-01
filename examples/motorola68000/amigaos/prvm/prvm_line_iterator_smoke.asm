; FS-UAE-friendly native smoke executable for the PRVM line iterator.

        .module main
        .cpu 68020

SysBase                         = 4
RETURN_OK                       = 0
RETURN_FAIL                     = 20

OpenLibrary                     = -552
CloseLibrary                    = -414
PutStr                          = -948

PRVM_ITER_FRAME_SIZE            = 116
PRVM_ITER_MAGIC_OPLI            = $4F504C49
PRVM_ITER_ABI_VERSION_V1        = 1
PRVM_TOKEN_RECORD_SIZE          = 20
PRVM_PARSER_CONTRACT_VERSION_V2 = 2
PRVM_ITER_STATUS_OK             = 0
PRVM_DEBUG_PROGRAM_LEN          = 59

        .section entry, kind=code

start:
        MOVEQ #RETURN_FAIL, D7

        LEA dosName(PC), A1
        MOVEQ #36, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)

        TST.L D0
        BNE.S prvmIterSmokeHaveDos

        LEA dosName(PC), A1
        MOVEQ #0, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)
        TST.L D0
        BEQ.W prvmIterSmokeDone

prvmIterSmokeHaveDos:
        MOVEA.L D0, A5
        BSR.W prvmIterSmokeBuildFrame
        LEA iteratorFrame(PC), A0
        MOVE.L #PRVM_ITER_FRAME_SIZE, D0
        MOVEA.L prvmIteratorEntryPtr(PC), A1
        JSR (A1)

        LEA iteratorStatus(PC), A0
        MOVE.L D0, 0(A0)
        MOVE.L D1, 4(A0)
        MOVE.L D2, 8(A0)
        MOVE.L D3, 12(A0)

        BSR.W prvmIterSmokeValidateResult
        TST.L D0
        BNE.S prvmIterSmokeReportFailure

        LEA successText(PC), A1
        MOVE.L A1, D1
        BSR.W prvmIterSmokePutStr
        MOVEQ #RETURN_OK, D7
        BRA.S prvmIterSmokeCloseDos

prvmIterSmokeReportFailure:
        MOVE.L A1, D1
        BSR.W prvmIterSmokePutStr

prvmIterSmokeCloseDos:
        MOVEA.L A5, A1
        MOVEA.L SysBase.W, A6
        JSR CloseLibrary(A6)

prvmIterSmokeDone:
        MOVE.L D7, D0
        RTS

prvmIterSmokePutStr:
        MOVEA.L A5, A6
        JSR PutStr(A6)
        RTS

prvmIterSmokeBuildFrame:
        LEA iteratorFrame(PC), A0
        MOVE.L #PRVM_ITER_MAGIC_OPLI, 0(A0)
        MOVE.W #PRVM_ITER_ABI_VERSION_V1, 4(A0)
        MOVE.W #PRVM_ITER_FRAME_SIZE, 6(A0)
        LEA processorAsmText(PC), A1
        MOVE.L A1, 8(A0)
        MOVE.L #3, 12(A0)
        LEA kindStatementText(PC), A1
        MOVE.L A1, 16(A0)
        MOVE.L #9, 20(A0)
        MOVE.L #1, 24(A0)
        LEA sourceText(PC), A1
        MOVE.L A1, 28(A0)
        MOVE.L #21, 32(A0)
        LEA tokenRecord(PC), A1
        MOVE.L A1, 36(A0)
        MOVE.L #3, 40(A0)
        MOVE.W #PRVM_TOKEN_RECORD_SIZE, 44(A0)
        CLR.W 46(A0)
        LEA lexemeBytes(PC), A1
        MOVE.L A1, 48(A0)
        MOVE.L #8, 52(A0)
        LEA parserProgram(PC), A1
        MOVE.L A1, 56(A0)
        MOVE.L #PRVM_DEBUG_PROGRAM_LEN, 60(A0)
        LEA resultBuffer(PC), A1
        MOVE.L A1, 64(A0)
        MOVE.L #128, 68(A0)
        LEA diagnosticBuffer(PC), A1
        MOVE.L A1, 72(A0)
        MOVE.L #32, 76(A0)
        LEA resumeBuffer(PC), A1
        MOVE.L A1, 80(A0)
        MOVE.L #40, 84(A0)
        LEA exprRequestBuffer(PC), A1
        MOVE.L A1, 88(A0)
        MOVE.L #32, 92(A0)
        LEA exprResultBuffer(PC), A1
        MOVE.L A1, 96(A0)
        MOVE.L #0, 100(A0)
        MOVE.L #PRVM_PARSER_CONTRACT_VERSION_V2, 104(A0)
        MOVE.L #64, 108(A0)
        CLR.L 112(A0)
        RTS

prvmIterSmokeValidateResult:
        LEA iteratorStatus(PC), A0
        CMPI.L #PRVM_ITER_STATUS_OK, 0(A0)
        BNE.S prvmIterSmokeInvalidStatus
        CMPI.L #2, 4(A0)
        BNE.S prvmIterSmokeInvalidRouted
        TST.L 8(A0)
        BNE.S prvmIterSmokeInvalidFailLine
        CMPI.L #2, 12(A0)
        BNE.S prvmIterSmokeInvalidTotal
        CLR.L D0
        RTS

prvmIterSmokeInvalidStatus:
        LEA failureStatusText(PC), A1
        MOVEQ #1, D0
        RTS

prvmIterSmokeInvalidRouted:
        LEA failureRoutedText(PC), A1
        MOVEQ #1, D0
        RTS

prvmIterSmokeInvalidFailLine:
        LEA failureFailLineText(PC), A1
        MOVEQ #1, D0
        RTS

prvmIterSmokeInvalidTotal:
        LEA failureTotalText(PC), A1
        MOVEQ #1, D0
        RTS

dosName:
        .byte "dos.library",0
processorAsmText:
        .byte "asm"
kindStatementText:
        .byte "statement"
successText:
        .byte "OPFORGE-PRVM-ITER smoke OK",10,0
failureStatusText:
        .byte "OPFORGE-PRVM-ITER smoke FAIL status",10,0
failureRoutedText:
        .byte "OPFORGE-PRVM-ITER smoke FAIL routed",10,0
failureFailLineText:
        .byte "OPFORGE-PRVM-ITER smoke FAIL fail-line",10,0
failureTotalText:
        .byte "OPFORGE-PRVM-ITER smoke FAIL total",10,0

sourceText:
        .byte "start: NOP",10,"start: NOP"
lexemeBytes:
        .byte "startNOP"

parserProgram:
        .byte $60,$40,$13,$03,$08,$00,$64,$00
        .byte $14,$03,$0E,$00,$66,$00
        .byte $15,$03,$24,$00
        .byte $33,$04,".","o","r","g",$62,$20,$22,$02,$41,$50
        .byte $FF,$FF,$FF,$FF,$64,$00
        .byte $10,$03,$03,$30,$00,$20,$30,$65,$20,$01,$33,$00
        .byte $30,$62,$20,$41,$50,$FF,$FF,$FF,$FF,$64,$00

tokenRecord:
        .word 0
        .word 0
        .long 1
        .long 6
        .long 0
        .long 5
        .word 5
        .word 0
        .long 6
        .long 7
        .long 0
        .long 0
        .word 0
        .word 0
        .long 8
        .long 11
        .long 5
        .long 3

iteratorStatus:
        .long 0
iteratorRoutedCount:
        .long 0
iteratorFailLine:
        .long 0
iteratorTotalLines:
        .long 0

iteratorFrame:
        .fill byte, 116, 0
resultBuffer:
        .fill byte, 128, 0
diagnosticBuffer:
        .fill byte, 32, 0
resumeBuffer:
        .fill byte, 40, 0
exprRequestBuffer:
        .fill byte, 32, 0
exprResultBuffer:
        .fill byte, 32, 0
prvmIteratorEntryPtr:
        .long prvm_iterate_lines_68000

        .endsection
        .use prvm.amigaos.line_iterator (prvm_iterate_lines_68000)
        .output "build/prvm_line_iterator_smoke.hunk", format=hunk, sections=entry,code
        .endmodule