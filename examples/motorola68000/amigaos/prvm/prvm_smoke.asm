; FS-UAE-friendly native smoke executable for the PRVM interpreter.

        .module main
        .cpu 68020
        .use prvm.amigaos.interpreter (prvm_run_68000)

SysBase                         = 4
RETURN_OK                       = 0
RETURN_FAIL                     = 20

OpenLibrary                     = -552
CloseLibrary                    = -414
PutStr                          = -948

PRVM_REQUEST_FRAME_SIZE         = 112
PRVM_MAGIC_OPRP                 = $4F505250
PRVM_ABI_VERSION_V1             = 1
PRVM_CALL_MODE_START            = 0
PRVM_ENTRY_KIND_OPASM_STATEMENT = 1
PRVM_TOKEN_RECORD_SIZE          = 20
PRVM_PARSER_CONTRACT_VERSION_V2 = 2

PRVM_STATUS_OK                  = 0
PRVM_STATUS_EXPR_REQUEST        = 1
PRVM_STATUS_NEWLINE_UNSUPPORTED = 2
PRVM_STATUS_ENTRY_BOUNDARY      = 3
PRVM_STATUS_INVALID_ARGUMENT    = 4
PRVM_STATUS_INVALID_TOKEN       = 5
PRVM_STATUS_INVALID_PROGRAM     = 6
PRVM_STATUS_OUTPUT_OVERFLOW     = 7
PRVM_STATUS_UNSUPPORTED_OPCODE  = 9
PRVM_STATUS_BUDGET_EXCEEDED     = 12

PRVM_RESULT_BEGIN_STATEMENT     = 1
PRVM_RESULT_MNEMONIC_TEXT       = 3
PRVM_RESULT_FINISH_LINE         = 5

PRVM_OPCODE_END                 = $00
PRVM_OPCODE_ADVANCE             = $20
PRVM_OPCODE_LOAD_IDENTIFIER     = $30
PRVM_OPCODE_BEGIN_STATEMENT     = $60
PRVM_OPCODE_SET_MNEMONIC        = $62
PRVM_OPCODE_FINISH_LINE         = $64
PRVM_SMOKE_PROGRAM_LEN          = 6

        .section entry, kind=code

start:
        MOVEQ #RETURN_FAIL, D7

        LEA dosName(PC), A1
        MOVEQ #36, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)

        TST.L D0
        BNE.S prvmSmokeHaveDos

        LEA dosName(PC), A1
        MOVEQ #0, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)
        TST.L D0
        BEQ.W prvmSmokeDone

prvmSmokeHaveDos:
        MOVEA.L D0, A5
        LEA startedText(PC), A1
        MOVE.L A1, D1
        BSR.W prvmSmokePutStr

        BSR.W prvmSmokeBuildRequestFrame
        LEA requestFrame(PC), A0
        MOVE.L #PRVM_REQUEST_FRAME_SIZE, D0
        JSR prvm_run_68000.L

        LEA smokeStatus(PC), A0
        MOVE.L D0, 0(A0)
        MOVE.L D1, 4(A0)
        MOVE.L D2, 8(A0)
        MOVE.L D3, 12(A0)

        BSR.W prvmSmokeValidateResult
        TST.L D0
        BNE.S prvmSmokeReportFailure

        LEA successText(PC), A1
        MOVE.L A1, D1
        BSR.W prvmSmokePutStr
        MOVEQ #RETURN_OK, D7
        BRA.S prvmSmokeCloseDos

prvmSmokeReportFailure:
        MOVE.L A1, D1
        BSR.W prvmSmokePutStr

prvmSmokeCloseDos:
        MOVEA.L A5, A1
        MOVEA.L SysBase.W, A6
        JSR CloseLibrary(A6)

prvmSmokeDone:
        MOVE.L D7, D0
        RTS

prvmSmokePutStr:
        MOVEA.L A5, A6
        JSR PutStr(A6)
        RTS

prvmSmokeBuildRequestFrame:
        LEA requestFrame(PC), A0
        MOVE.L #PRVM_MAGIC_OPRP, 0(A0)
        MOVE.W #PRVM_ABI_VERSION_V1, 4(A0)
        MOVE.W #PRVM_REQUEST_FRAME_SIZE, 6(A0)
        MOVE.W #PRVM_CALL_MODE_START, 8(A0)
        MOVE.W #PRVM_ENTRY_KIND_OPASM_STATEMENT, 10(A0)
        MOVE.L #1, 12(A0)
        LEA sourceLine(PC), A1
        MOVE.L A1, 16(A0)
        MOVE.L #4, 20(A0)
        LEA tokenRecord(PC), A1
        MOVE.L A1, 24(A0)
        MOVE.L #1, 28(A0)
        MOVE.W #PRVM_TOKEN_RECORD_SIZE, 32(A0)
        CLR.W 34(A0)
        LEA lexemeBytes(PC), A1
        MOVE.L A1, 36(A0)
        MOVE.L #3, 40(A0)
        LEA parserProgram(PC), A1
        MOVE.L A1, 44(A0)
        MOVE.L #PRVM_SMOKE_PROGRAM_LEN, 48(A0)
        LEA resultBuffer(PC), A1
        MOVE.L A1, 52(A0)
        MOVE.L #96, 56(A0)
        LEA diagnosticBuffer(PC), A1
        MOVE.L A1, 60(A0)
        MOVE.L #32, 64(A0)
        LEA resumeBuffer(PC), A1
        MOVE.L A1, 68(A0)
        MOVE.L #16, 72(A0)
        LEA exprRequestBuffer(PC), A1
        MOVE.L A1, 76(A0)
        MOVE.L #32, 80(A0)
        CLR.L 84(A0)
        CLR.L 88(A0)
        MOVE.L #PRVM_PARSER_CONTRACT_VERSION_V2, 92(A0)
        MOVE.L #64, 96(A0)
        CLR.L 100(A0)
        CLR.L 104(A0)
        CLR.L 108(A0)
        RTS

prvmSmokeValidateResult:
        LEA smokeStatus(PC), A1
        LEA resultBuffer(PC), A0
        CMPI.L #PRVM_STATUS_OK, 0(A1)
        BNE.W prvmSmokeInvalidStatus
        CMPI.L #3, 4(A1)
        BNE.W prvmSmokeInvalidCount
        CMPI.L #1, 8(A1)
        BNE.W prvmSmokeInvalidCursor
        CMPI.L #96, 12(A1)
        BNE.W prvmSmokeInvalidBytes
        CMPI.W #PRVM_RESULT_BEGIN_STATEMENT, 0(A0)
        BNE.W prvmSmokeInvalidBegin
        CMPI.W #PRVM_RESULT_MNEMONIC_TEXT, 32(A0)
        BNE.W prvmSmokeInvalidMnemonic
        CMPI.L #2, 40(A0)
        BNE.W prvmSmokeInvalidColStart
        CMPI.L #5, 44(A0)
        BNE.W prvmSmokeInvalidColEnd
        CMPI.L #0, 48(A0)
        BNE.W prvmSmokeInvalidLexemeOffset
        CMPI.L #3, 52(A0)
        BNE.W prvmSmokeInvalidLexemeLen
        CMPI.W #PRVM_RESULT_FINISH_LINE, 64(A0)
        BNE.W prvmSmokeInvalidFinish
        LEA successText(PC), A1
        CLR.L D0
        RTS

prvmSmokeInvalidStatus:
        BSR.W prvmSmokeFormatStatus
        CMPI.L #PRVM_STATUS_EXPR_REQUEST, 0(A1)
        BEQ.S prvmSmokeInvalidStatusExpr
        CMPI.L #PRVM_STATUS_NEWLINE_UNSUPPORTED, 0(A1)
        BEQ.S prvmSmokeInvalidStatusNewline
        CMPI.L #PRVM_STATUS_ENTRY_BOUNDARY, 0(A1)
        BEQ.S prvmSmokeInvalidStatusEntry
        CMPI.L #PRVM_STATUS_INVALID_ARGUMENT, 0(A1)
        BEQ.S prvmSmokeInvalidStatusArgument
        CMPI.L #PRVM_STATUS_INVALID_TOKEN, 0(A1)
        BEQ.S prvmSmokeInvalidStatusToken
        CMPI.L #PRVM_STATUS_INVALID_PROGRAM, 0(A1)
        BEQ.S prvmSmokeInvalidStatusProgram
        CMPI.L #PRVM_STATUS_OUTPUT_OVERFLOW, 0(A1)
        BEQ.S prvmSmokeInvalidStatusOverflow
        CMPI.L #PRVM_STATUS_UNSUPPORTED_OPCODE, 0(A1)
        BEQ.S prvmSmokeInvalidStatusOpcode
        CMPI.L #PRVM_STATUS_BUDGET_EXCEEDED, 0(A1)
        BEQ.S prvmSmokeInvalidStatusBudget
        LEA failureStatusText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusExpr:
        LEA failureStatusExprText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusNewline:
        LEA failureStatusNewlineText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusEntry:
        LEA failureStatusEntryText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusArgument:
        LEA failureStatusArgumentText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusToken:
        LEA failureStatusTokenText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusProgram:
        LEA failureStatusProgramText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusOverflow:
        LEA failureStatusOverflowText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusOpcode:
        LEA failureStatusOpcodeText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidStatusBudget:
        LEA failureStatusBudgetText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidCount:
        LEA failureCountText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidCursor:
        LEA failureCursorText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidBytes:
        LEA failureBytesText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidBegin:
        LEA failureBeginText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidMnemonic:
        LEA failureMnemonicText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidColStart:
        LEA failureColStartText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidColEnd:
        LEA failureColEndText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidLexemeOffset:
        LEA failureLexemeOffsetText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidLexemeLen:
        LEA failureLexemeLenText(PC), A1
        BRA.S prvmSmokeInvalid

prvmSmokeInvalidFinish:
        LEA failureFinishText(PC), A1

prvmSmokeInvalid:
        MOVEQ #1, D0
        RTS

prvmSmokeFormatStatus:
        MOVE.L 0(A1), D0
        LEA failureStatusHexDigits(PC), A0
        MOVEQ #7, D2

prvmSmokeFormatStatusLoop:
        ROL.L #4, D0
        MOVE.L D0, D3
        ANDI.B #$0F, D3
        CMPI.B #10, D3
        BCS.S prvmSmokeFormatStatusDigit
        ADDI.B #7, D3

prvmSmokeFormatStatusDigit:
        ADDI.B #"0", D3
        MOVE.B D3, (A0)+
        DBRA D2, prvmSmokeFormatStatusLoop
        RTS

dosName:
        .byte "dos.library",0
startedText:
        .byte "OPFORGE-PRVM smoke start",10,0
successText:
        .byte "OPFORGE-PRVM smoke OK",10,0
failureText:
        .byte "OPFORGE-PRVM smoke FAIL",10,0
failureStatusText:
        .byte "OPFORGE-PRVM smoke FAIL status $"
failureStatusHexDigits:
        .byte "00000000",10,0
failureStatusExprText:
        .byte "OPFORGE-PRVM smoke FAIL status expr",10,0
failureStatusNewlineText:
        .byte "OPFORGE-PRVM smoke FAIL status newline",10,0
failureStatusEntryText:
        .byte "OPFORGE-PRVM smoke FAIL status entry",10,0
failureStatusArgumentText:
        .byte "OPFORGE-PRVM smoke FAIL status argument",10,0
failureStatusTokenText:
        .byte "OPFORGE-PRVM smoke FAIL status token",10,0
failureStatusProgramText:
        .byte "OPFORGE-PRVM smoke FAIL status program",10,0
failureStatusOverflowText:
        .byte "OPFORGE-PRVM smoke FAIL status overflow",10,0
failureStatusOpcodeText:
        .byte "OPFORGE-PRVM smoke FAIL status opcode",10,0
failureStatusBudgetText:
        .byte "OPFORGE-PRVM smoke FAIL status budget",10,0
failureCountText:
        .byte "OPFORGE-PRVM smoke FAIL count",10,0
failureCursorText:
        .byte "OPFORGE-PRVM smoke FAIL cursor",10,0
failureBytesText:
        .byte "OPFORGE-PRVM smoke FAIL bytes",10,0
failureBeginText:
        .byte "OPFORGE-PRVM smoke FAIL begin",10,0
failureMnemonicText:
        .byte "OPFORGE-PRVM smoke FAIL mnemonic",10,0
failureColStartText:
        .byte "OPFORGE-PRVM smoke FAIL col-start",10,0
failureColEndText:
        .byte "OPFORGE-PRVM smoke FAIL col-end",10,0
failureLexemeOffsetText:
        .byte "OPFORGE-PRVM smoke FAIL lexeme-offset",10,0
failureLexemeLenText:
        .byte "OPFORGE-PRVM smoke FAIL lexeme-len",10,0
failureFinishText:
        .byte "OPFORGE-PRVM smoke FAIL finish",10,0

sourceLine:
        .byte " NOP"

lexemeBytes:
        .byte "NOP"

parserProgram:
        .byte $60
        .byte $30
        .byte $62
        .byte $20
        .byte $64
        .byte $00
parserProgramEnd:

tokenRecord:
        .word 0
        .word 0
        .long 2
        .long 5
        .long 0
        .long 3

smokeStatus:
        .long 0
smokeResultCount:
        .long 0
smokeCursor:
        .long 0
smokeResultBytes:
        .long 0

requestFrame:
        .fill byte, 112, 0
resultBuffer:
        .fill byte, 96, 0
diagnosticBuffer:
        .fill byte, 32, 0
resumeBuffer:
        .fill byte, 16, 0
exprRequestBuffer:
        .fill byte, 32, 0

        .endsection
        .output "build/prvm_smoke.hunk", format=hunk, sections=entry,code
        .endmodule