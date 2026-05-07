; Optional AmigaOS PRVM report demo for the parity-locked interpreter.

        .module main
        .cpu 68020
        .use prvm.amigaos.interpreter (prvmRun68000)

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

PRVM_RESULT_BEGIN_STATEMENT     = 1
PRVM_RESULT_LABEL_TEXT          = 2
PRVM_RESULT_MNEMONIC_TEXT       = 3
PRVM_RESULT_FINISH_LINE         = 5

PRVM_OPCODE_END                 = $00
PRVM_OPCODE_JUMP                = $01
PRVM_OPCODE_JUMP_IF_FALSE       = $03
PRVM_OPCODE_PEEK_KIND           = $10
PRVM_OPCODE_IS_EOL              = $13
PRVM_OPCODE_PEEK_ASSIGNMENT     = $14
PRVM_OPCODE_PEEK_STAR_ORG       = $15
PRVM_OPCODE_ADVANCE             = $20
PRVM_OPCODE_CONSUME_OPERATOR    = $22
PRVM_OPCODE_LOAD_IDENTIFIER     = $30
PRVM_OPCODE_LOAD_INLINE_TEXT    = $33
PRVM_OPCODE_PARSE_OPTIONAL_LABEL = $40
PRVM_OPCODE_SCAN_COMMA_BOUNDARIES = $41
PRVM_OPCODE_PARSE_OPERAND_EXPR  = $50
PRVM_OPCODE_BEGIN_STATEMENT     = $60
PRVM_OPCODE_SET_MNEMONIC        = $62
PRVM_OPCODE_FINISH_LINE         = $64
PRVM_OPCODE_SET_DOT_MNEMONIC    = $65
PRVM_OPCODE_FINISH_ASSIGNMENT   = $66
PRVM_DEBUG_PROGRAM_LEN          = 59

        .section entry, kind=code

start:
        MOVEQ #RETURN_FAIL, D7

        LEA dosName(PC), A1
        MOVEQ #36, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)

        TST.L D0
        BNE.S prvmDebugCliHaveDos

        LEA dosName(PC), A1
        MOVEQ #0, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)
        TST.L D0
        BEQ.W prvmDebugCliDone

prvmDebugCliHaveDos:
        MOVEA.L D0, A5
        BSR.W prvmDebugCliBuildRequestFrame
        LEA requestFrame(PC), A0
        MOVE.L #PRVM_REQUEST_FRAME_SIZE, D0
        JSR prvmRun68000.L

        LEA prvmStatus(PC), A0
        MOVE.L D0, 0(A0)
        MOVE.L D1, 4(A0)
        MOVE.L D2, 8(A0)
        MOVE.L D3, 12(A0)

        BSR.W prvmDebugCliValidateResult
        TST.L D0
        BNE.S prvmDebugCliReportFailure

        LEA reportSuccessText(PC), A1
        MOVE.L A1, D1
        BSR.W prvmDebugCliPutStr
        MOVEQ #RETURN_OK, D7
        BRA.S prvmDebugCliCloseDos

prvmDebugCliReportFailure:
        BSR.W prvmDebugCliFormatStatus
        LEA reportFailureText(PC), A1
        MOVE.L A1, D1
        BSR.W prvmDebugCliPutStr

prvmDebugCliCloseDos:
        MOVEA.L A5, A1
        MOVEA.L SysBase.W, A6
        JSR CloseLibrary(A6)

prvmDebugCliDone:
        MOVE.L D7, D0
        RTS

prvmDebugCliPutStr:
        MOVEA.L A5, A6
        JSR PutStr(A6)
        RTS

prvmDebugCliBuildRequestFrame:
        LEA requestFrame(PC), A0
        MOVE.L #PRVM_MAGIC_OPRP, 0(A0)
        MOVE.W #PRVM_ABI_VERSION_V1, 4(A0)
        MOVE.W #PRVM_REQUEST_FRAME_SIZE, 6(A0)
        MOVE.W #PRVM_CALL_MODE_START, 8(A0)
        MOVE.W #PRVM_ENTRY_KIND_OPASM_STATEMENT, 10(A0)
        MOVE.L #1, 12(A0)
        LEA sourceLine(PC), A1
        MOVE.L A1, 16(A0)
        MOVE.L #10, 20(A0)
        LEA tokenRecord(PC), A1
        MOVE.L A1, 24(A0)
        MOVE.L #3, 28(A0)
        MOVE.W #PRVM_TOKEN_RECORD_SIZE, 32(A0)
        CLR.W 34(A0)
        LEA lexemeBytes(PC), A1
        MOVE.L A1, 36(A0)
        MOVE.L #8, 40(A0)
        LEA parserProgram(PC), A1
        MOVE.L A1, 44(A0)
        MOVE.L #PRVM_DEBUG_PROGRAM_LEN, 48(A0)
        LEA resultBuffer(PC), A1
        MOVE.L A1, 52(A0)
        MOVE.L #128, 56(A0)
        LEA diagnosticBuffer(PC), A1
        MOVE.L A1, 60(A0)
        MOVE.L #32, 64(A0)
        LEA resumeBuffer(PC), A1
        MOVE.L A1, 68(A0)
        MOVE.L #40, 72(A0)
        LEA exprRequestBuffer(PC), A1
        MOVE.L A1, 76(A0)
        MOVE.L #32, 80(A0)
        LEA exprResultBuffer(PC), A1
        MOVE.L A1, 84(A0)
        MOVE.L #0, 88(A0)
        MOVE.L #PRVM_PARSER_CONTRACT_VERSION_V2, 92(A0)
        MOVE.L #64, 96(A0)
        CLR.L 100(A0)
        CLR.L 104(A0)
        CLR.L 108(A0)
        RTS

prvmDebugCliValidateResult:
        LEA prvmStatus(PC), A1
        LEA resultBuffer(PC), A0
        CMPI.L #PRVM_STATUS_OK, 0(A1)
        BNE.W prvmDebugCliInvalid
        CMPI.L #4, 4(A1)
        BNE.W prvmDebugCliInvalid
        CMPI.L #3, 8(A1)
        BNE.W prvmDebugCliInvalid
        CMPI.L #128, 12(A1)
        BNE.W prvmDebugCliInvalid
        CMPI.W #PRVM_RESULT_BEGIN_STATEMENT, 0(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.W #PRVM_RESULT_LABEL_TEXT, 32(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.L #1, 40(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.L #6, 44(A0)
        BNE.W prvmDebugCliInvalid
        TST.L 48(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.L #5, 52(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.W #PRVM_RESULT_MNEMONIC_TEXT, 64(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.L #8, 72(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.L #11, 76(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.L #5, 80(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.L #3, 84(A0)
        BNE.W prvmDebugCliInvalid
        CMPI.W #PRVM_RESULT_FINISH_LINE, 96(A0)
        BNE.W prvmDebugCliInvalid
        CLR.L D0
        RTS

prvmDebugCliInvalid:
        MOVEQ #1, D0
        RTS

prvmDebugCliFormatStatus:
        LEA prvmStatus(PC), A1
        MOVE.L 0(A1), D0
        LEA reportFailureStatusHexDigits(PC), A0
        MOVEQ #7, D2

prvmDebugCliFormatStatusLoop:
        ROL.L #4, D0
        MOVE.L D0, D3
        ANDI.B #$0F, D3
        CMPI.B #10, D3
        BCS.S prvmDebugCliFormatStatusDigit
        ADDI.B #7, D3

prvmDebugCliFormatStatusDigit:
        ADDI.B #"0", D3
        MOVE.B D3, (A0)+
        DBRA D2, prvmDebugCliFormatStatusLoop
        RTS

dosName:
        .byte "dos.library",0
reportSuccessText:
        .byte "OPFORGE-PRVM 1",10
        .byte "STATUS 0",10
        .byte "RESULTS 4",10
        .byte "CURSOR 3",10
        .byte "BYTES 128",10
        .byte "RESULT 0 KIND begin_statement",10
        .byte "RESULT 1 KIND label_text START 1 END 6 LEN 5 LEXHEX 7374617274",10
        .byte "RESULT 2 KIND mnemonic_text START 8 END 11 LEN 3 LEXHEX 4E4F50",10
        .byte "RESULT 3 KIND finish_line",10
        .byte "END",10,0
reportFailureText:
        .byte "OPFORGE-PRVM 1",10
        .byte "STATUS $"
reportFailureStatusHexDigits:
        .byte "00000000",10
        .byte "RESULTS 0",10
        .byte "CURSOR 0",10
        .byte "BYTES 0",10
        .byte "END",10,0

sourceLine:
        .byte "start: NOP"

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

prvmStatus:
        .long 0
prvmResultCount:
        .long 0
prvmCursor:
        .long 0
prvmResultBytes:
        .long 0

requestFrame:
        .fill byte, 112, 0
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

        .endsection
        .output "build/prvm_debug_cli.hunk", format=hunk, sections=entry,code
        .endmodule
