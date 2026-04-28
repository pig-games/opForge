; Native parser VM module for the first AmigaOS PRVM interpreter slice.

        .module prvm.amigaos.interpreter
        .cpu 68020
        .pub

PRVM_MAGIC_OPRP                     = $4F505250
PRVM_REQUEST_FRAME_SIZE             = 112
PRVM_TOKEN_RECORD_SIZE              = 20
PRVM_RESULT_RECORD_SIZE             = 32
PRVM_DEFAULT_STEP_BUDGET            = 256

PRVM_FRAME_MAGIC                    = 0
PRVM_FRAME_ABI_VERSION              = 4
PRVM_FRAME_FRAME_SIZE               = 6
PRVM_FRAME_CALL_MODE                = 8
PRVM_FRAME_ENTRY_KIND               = 10
PRVM_FRAME_LINE_NUM                 = 12
PRVM_FRAME_SOURCE_PTR               = 16
PRVM_FRAME_SOURCE_LEN               = 20
PRVM_FRAME_TOKEN_PTR                = 24
PRVM_FRAME_TOKEN_COUNT              = 28
PRVM_FRAME_TOKEN_RECORD_SIZE        = 32
PRVM_FRAME_LEXEME_PTR               = 36
PRVM_FRAME_LEXEME_LEN               = 40
PRVM_FRAME_PROGRAM_PTR              = 44
PRVM_FRAME_PROGRAM_LEN              = 48
PRVM_FRAME_RESULT_PTR               = 52
PRVM_FRAME_RESULT_CAPACITY          = 56
PRVM_FRAME_DIAGNOSTIC_PTR           = 60
PRVM_FRAME_RESUME_PTR               = 68
PRVM_FRAME_EXPR_REQUEST_PTR         = 76
PRVM_FRAME_PARSER_CONTRACT_VERSION  = 92
PRVM_FRAME_STEP_BUDGET              = 96
PRVM_FRAME_FLAGS                    = 100

PRVM_STATUS_OK                      = 0
PRVM_STATUS_EXPR_REQUEST            = 1
PRVM_STATUS_NEWLINE_UNSUPPORTED     = 2
PRVM_STATUS_ENTRY_BOUNDARY          = 3
PRVM_STATUS_INVALID_ARGUMENT        = 4
PRVM_STATUS_INVALID_TOKEN           = 5
PRVM_STATUS_INVALID_PROGRAM         = 6
PRVM_STATUS_OUTPUT_OVERFLOW         = 7
PRVM_STATUS_UNSUPPORTED_OPCODE      = 9
PRVM_STATUS_BUDGET_EXCEEDED         = 12

PRVM_ENTRY_KIND_OPASM_STATEMENT     = 1
PRVM_CALL_MODE_START                = 0
PRVM_ABI_VERSION_V1                 = 1
PRVM_PARSER_CONTRACT_VERSION_V2     = 2

PRVM_TOKEN_KIND_IDENTIFIER          = 0

PRVM_RESULT_BEGIN_STATEMENT         = 1
PRVM_RESULT_MNEMONIC_TEXT           = 3
PRVM_RESULT_FINISH_LINE             = 5

PRVM_OPCODE_END                     = $00
PRVM_OPCODE_ADVANCE                 = $20
PRVM_OPCODE_LOAD_IDENTIFIER         = $30
PRVM_OPCODE_PARSE_OPTIONAL_LABEL    = $40
PRVM_OPCODE_BEGIN_STATEMENT         = $60
PRVM_OPCODE_SET_MNEMONIC            = $62
PRVM_OPCODE_FINISH_LINE             = $64

LOCAL_LOADED_FLAG                   = 0
LOCAL_LOADED_COL_START              = 4
LOCAL_LOADED_COL_END                = 8
LOCAL_LOADED_LEXEME_OFFSET          = 12
LOCAL_LOADED_LEXEME_LEN             = 16
LOCAL_FINISHED_FLAG                 = 20
LOCAL_STEP_COUNT                    = 24
LOCAL_SIZE                          = 28

        .section data, kind=data

abiMarker:
        .byte "OPFORGE-PRVM-ABI-V1",0

        .endsection

        .section code, kind=code

; ---------------------------------------------------------------------------
; Native parser VM entry.
;
; Call ABI:
; - A0: PRVM_REQUEST_FRAME_V1 pointer
; - D0: request frame size in bytes
;
; Return ABI:
; - D0: PRVM_STATUS_*
; - D1: result record count on success
; - D2: final token cursor or status-specific offset
; - D3: committed result bytes on success
; ---------------------------------------------------------------------------

prvm_run_68000:
        MOVEM.L D4-D7/A4-A6, -(SP)
        MOVE.L A0, D1
        TST.L D1
        BEQ prvmInvalidArgument
        CMPI.L #PRVM_REQUEST_FRAME_SIZE, D0
        BLT prvmInvalidArgument

        MOVEA.L A0, A4
        SUBA.L #LOCAL_SIZE, SP
        LEA 0(SP), A3
        CLR.L LOCAL_LOADED_FLAG(A3)
        CLR.L LOCAL_FINISHED_FLAG(A3)
        CLR.L LOCAL_STEP_COUNT(A3)

        CMPI.L #PRVM_MAGIC_OPRP, PRVM_FRAME_MAGIC(A4)
        BNE prvmInvalidArgumentWithLocals
        CMPI.W #PRVM_ABI_VERSION_V1, PRVM_FRAME_ABI_VERSION(A4)
        BNE prvmInvalidArgumentWithLocals
        MOVEQ #0, D0
        MOVE.W PRVM_FRAME_FRAME_SIZE(A4), D0
        CMPI.L #PRVM_REQUEST_FRAME_SIZE, D0
        BLT prvmInvalidArgumentWithLocals
        CMPI.W #PRVM_CALL_MODE_START, PRVM_FRAME_CALL_MODE(A4)
        BNE prvmInvalidArgumentWithLocals
        CMPI.W #PRVM_ENTRY_KIND_OPASM_STATEMENT, PRVM_FRAME_ENTRY_KIND(A4)
        BNE prvmEntryBoundary
        CMPI.W #PRVM_TOKEN_RECORD_SIZE, PRVM_FRAME_TOKEN_RECORD_SIZE(A4)
        BNE prvmInvalidArgumentWithLocals
        CMPI.L #PRVM_PARSER_CONTRACT_VERSION_V2, PRVM_FRAME_PARSER_CONTRACT_VERSION(A4)
        BNE prvmInvalidProgramAtCursor
        TST.L PRVM_FRAME_FLAGS(A4)
        BNE prvmInvalidArgumentWithLocals

        MOVE.L PRVM_FRAME_SOURCE_LEN(A4), D6
        BMI prvmInvalidArgumentWithLocals
        BEQ prvmValidateTokenBuffer
        MOVE.L PRVM_FRAME_SOURCE_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals

prvmValidateTokenBuffer:
        MOVE.L PRVM_FRAME_TOKEN_COUNT(A4), D4
        BMI prvmInvalidArgumentWithLocals
        BEQ prvmValidateLexemeBuffer
        MOVE.L PRVM_FRAME_TOKEN_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals

prvmValidateLexemeBuffer:
        MOVE.L PRVM_FRAME_LEXEME_LEN(A4), D0
        BMI prvmInvalidArgumentWithLocals
        BEQ prvmValidateProgramBuffer
        MOVE.L PRVM_FRAME_LEXEME_PTR(A4), D7
        TST.L D7
        BEQ prvmInvalidArgumentWithLocals

prvmValidateProgramBuffer:
        MOVE.L PRVM_FRAME_PROGRAM_LEN(A4), D6
        BLE prvmInvalidProgramAtCursor
        MOVE.L PRVM_FRAME_PROGRAM_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_RESULT_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_RESULT_CAPACITY(A4), D0
        BMI prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_DIAGNOSTIC_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_RESUME_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_EXPR_REQUEST_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals

        MOVEA.L PRVM_FRAME_SOURCE_PTR(A4), A0
        MOVE.L PRVM_FRAME_SOURCE_LEN(A4), D6
        CLR.L D0
prvmNewlineScanLoop:
        CMP.L D6, D0
        BCC prvmNewlineScanDone
        CMPI.B #10, 0(A0,D0.L)
        BEQ prvmNewlineUnsupported
        CMPI.B #13, 0(A0,D0.L)
        BEQ prvmNewlineUnsupported
        ADDQ.L #1, D0
        BRA prvmNewlineScanLoop

prvmNewlineUnsupported:
        CLR.L D1
        MOVE.L D0, D2
        CLR.L D3
        MOVEQ #PRVM_STATUS_NEWLINE_UNSUPPORTED, D0
        BRA prvmReturnWithLocals

prvmNewlineScanDone:
        MOVEA.L PRVM_FRAME_PROGRAM_PTR(A4), A5
        MOVE.L PRVM_FRAME_PROGRAM_LEN(A4), D6
        LEA 0(A5,D6.L), A6
        MOVE.L PRVM_FRAME_STEP_BUDGET(A4), D6
        BGT prvmStartProgram
        MOVE.L #PRVM_DEFAULT_STEP_BUDGET, D6

prvmStartProgram:
        CLR.L D1
        CLR.L D2
        CLR.L D3

prvmProgramLoop:
        MOVE.L LOCAL_STEP_COUNT(A3), D0
        ADDQ.L #1, D0
        MOVE.L D0, LOCAL_STEP_COUNT(A3)
        CMP.L D6, D0
        BHI prvmBudgetExceeded
        CMPA.L A6, A5
        BCC prvmInvalidProgramAtCursor

        MOVEQ #0, D7
        MOVE.B (A5)+, D7
        CMPI.B #PRVM_OPCODE_END, D7
        BEQ prvmOpcodeEnd
        CMPI.B #PRVM_OPCODE_ADVANCE, D7
        BEQ prvmOpcodeAdvance
        CMPI.B #PRVM_OPCODE_LOAD_IDENTIFIER, D7
        BEQ prvmOpcodeLoadIdentifier
        CMPI.B #PRVM_OPCODE_PARSE_OPTIONAL_LABEL, D7
        BEQ prvmProgramLoop
        CMPI.B #PRVM_OPCODE_BEGIN_STATEMENT, D7
        BEQ prvmOpcodeBeginStatement
        CMPI.B #PRVM_OPCODE_SET_MNEMONIC, D7
        BEQ prvmOpcodeSetMnemonic
        CMPI.B #PRVM_OPCODE_FINISH_LINE, D7
        BEQ prvmOpcodeFinishLine
        BRA prvmUnsupportedOpcode

prvmOpcodeEnd:
        TST.L LOCAL_FINISHED_FLAG(A3)
        BEQ prvmInvalidProgramAtCursor
        MOVEQ #PRVM_STATUS_OK, D0
        BRA prvmReturnWithLocals

prvmOpcodeAdvance:
        CMP.L D4, D2
        BCC prvmProgramLoop
        ADDQ.L #1, D2
        BRA prvmProgramLoop

prvmOpcodeLoadIdentifier:
        BSR.W prvmCurrentTokenPtr
        TST.L D0
        BNE prvmReturnWithLocals
        CMPI.W #PRVM_TOKEN_KIND_IDENTIFIER, 0(A1)
        BNE prvmInvalidTokenAtCursor
        MOVE.L 4(A1), D0
        BEQ prvmInvalidTokenAtCursor
        MOVE.L 8(A1), D7
        CMP.L D0, D7
        BCS prvmInvalidTokenAtCursor
        MOVE.L 12(A1), D0
        MOVE.L 16(A1), D7
        BEQ prvmInvalidTokenAtCursor
        MOVE.L D0, D5
        ADD.L D7, D5
        BCS prvmInvalidTokenAtCursor
        CMP.L PRVM_FRAME_LEXEME_LEN(A4), D5
        BHI prvmInvalidTokenAtCursor
        MOVE.L 4(A1), LOCAL_LOADED_COL_START(A3)
        MOVE.L 8(A1), LOCAL_LOADED_COL_END(A3)
        MOVE.L 12(A1), LOCAL_LOADED_LEXEME_OFFSET(A3)
        MOVE.L 16(A1), LOCAL_LOADED_LEXEME_LEN(A3)
        MOVE.L #1, LOCAL_LOADED_FLAG(A3)
        BRA prvmProgramLoop

prvmOpcodeBeginStatement:
        CLR.L LOCAL_LOADED_FLAG(A3)
        CLR.L LOCAL_FINISHED_FLAG(A3)
        BSR.W prvmEmitBeginStatement
        TST.L D0
        BNE prvmReturnWithLocals
        BRA prvmProgramLoop

prvmOpcodeSetMnemonic:
        TST.L LOCAL_LOADED_FLAG(A3)
        BEQ prvmInvalidProgramAtCursor
        BSR.W prvmEmitMnemonicText
        TST.L D0
        BNE prvmReturnWithLocals
        CLR.L LOCAL_LOADED_FLAG(A3)
        BRA prvmProgramLoop

prvmOpcodeFinishLine:
        BSR.W prvmEmitFinishLine
        TST.L D0
        BNE prvmReturnWithLocals
        MOVE.L #1, LOCAL_FINISHED_FLAG(A3)
        BRA prvmProgramLoop

prvmCurrentTokenPtr:
        CMP.L D4, D2
        BCC prvmCurrentTokenInvalid
        MOVE.L D2, D0
        LSL.L #4, D0
        MOVE.L D2, D7
        LSL.L #2, D7
        ADD.L D7, D0
        MOVEA.L PRVM_FRAME_TOKEN_PTR(A4), A1
        ADDA.L D0, A1
        CLR.L D0
        RTS

prvmCurrentTokenInvalid:
        CLR.L D1
        CLR.L D3
        MOVEQ #PRVM_STATUS_INVALID_TOKEN, D0
        RTS

prvmResultRecordPtr:
        MOVE.L D1, D0
        LSL.L #5, D0
        MOVE.L D0, D7
        ADDI.L #PRVM_RESULT_RECORD_SIZE, D7
        CMP.L PRVM_FRAME_RESULT_CAPACITY(A4), D7
        BHI prvmOutputOverflow
        MOVEA.L PRVM_FRAME_RESULT_PTR(A4), A2
        ADDA.L D0, A2
        CLR.L D0
        RTS

prvmCommitResultRecord:
        ADDQ.L #1, D1
        MOVE.L D1, D3
        LSL.L #5, D3
        CLR.L D0
        RTS

prvmEmitBeginStatement:
        BSR.W prvmResultRecordPtr
        TST.L D0
        BNE prvmEmitRecordReturn
        MOVE.W #PRVM_RESULT_BEGIN_STATEMENT, 0(A2)
        CLR.W 2(A2)
        MOVE.L PRVM_FRAME_LINE_NUM(A4), 4(A2)
        CLR.L 8(A2)
        CLR.L 12(A2)
        CLR.L 16(A2)
        CLR.L 20(A2)
        CLR.L 24(A2)
        CLR.L 28(A2)
        BRA prvmCommitResultRecord

prvmEmitMnemonicText:
        BSR.W prvmResultRecordPtr
        TST.L D0
        BNE prvmEmitRecordReturn
        MOVE.W #PRVM_RESULT_MNEMONIC_TEXT, 0(A2)
        CLR.W 2(A2)
        MOVE.L PRVM_FRAME_LINE_NUM(A4), 4(A2)
        MOVE.L LOCAL_LOADED_COL_START(A3), 8(A2)
        MOVE.L LOCAL_LOADED_COL_END(A3), 12(A2)
        MOVE.L LOCAL_LOADED_LEXEME_OFFSET(A3), 16(A2)
        MOVE.L LOCAL_LOADED_LEXEME_LEN(A3), 20(A2)
        CLR.L 24(A2)
        CLR.L 28(A2)
        BRA prvmCommitResultRecord

prvmEmitFinishLine:
        BSR.W prvmResultRecordPtr
        TST.L D0
        BNE prvmEmitRecordReturn
        MOVE.W #PRVM_RESULT_FINISH_LINE, 0(A2)
        CLR.W 2(A2)
        MOVE.L PRVM_FRAME_LINE_NUM(A4), 4(A2)
        CLR.L 8(A2)
        CLR.L 12(A2)
        CLR.L 16(A2)
        CLR.L 20(A2)
        CLR.L 24(A2)
        CLR.L 28(A2)
        BRA prvmCommitResultRecord

prvmEmitRecordReturn:
        RTS

prvmEntryBoundary:
        CLR.L D1
        CLR.L D2
        CLR.L D3
        MOVEQ #PRVM_STATUS_ENTRY_BOUNDARY, D0
        BRA prvmReturnWithLocals

prvmInvalidTokenAtCursor:
        CLR.L D1
        CLR.L D3
        MOVEQ #PRVM_STATUS_INVALID_TOKEN, D0
        BRA prvmReturnWithLocals

prvmInvalidProgramAtCursor:
        CLR.L D1
        CLR.L D3
        MOVEQ #PRVM_STATUS_INVALID_PROGRAM, D0
        BRA prvmReturnWithLocals

prvmOutputOverflow:
        MOVEQ #PRVM_STATUS_OUTPUT_OVERFLOW, D0
        RTS

prvmUnsupportedOpcode:
        CLR.L D1
        CLR.L D3
        MOVEQ #PRVM_STATUS_UNSUPPORTED_OPCODE, D0
        BRA prvmReturnWithLocals

prvmBudgetExceeded:
        CLR.L D1
        CLR.L D3
        MOVEQ #PRVM_STATUS_BUDGET_EXCEEDED, D0
        BRA prvmReturnWithLocals

prvmInvalidArgumentWithLocals:
        CLR.L D1
        CLR.L D2
        CLR.L D3
        MOVEQ #PRVM_STATUS_INVALID_ARGUMENT, D0
        BRA prvmReturnWithLocals

prvmReturnWithLocals:
        ADDA.L #LOCAL_SIZE, SP
        MOVEM.L (SP)+, D4-D7/A4-A6
        RTS

prvmInvalidArgument:
        CLR.L D1
        CLR.L D2
        CLR.L D3
        MOVEQ #PRVM_STATUS_INVALID_ARGUMENT, D0
        MOVEM.L (SP)+, D4-D7/A4-A6
        RTS

        .endsection
        .endmodule