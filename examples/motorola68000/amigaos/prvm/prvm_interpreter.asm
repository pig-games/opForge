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
PRVM_FRAME_RESUME_CAPACITY          = 72
PRVM_FRAME_EXPR_REQUEST_PTR         = 76
PRVM_FRAME_EXPR_REQUEST_SIZE        = 80
PRVM_FRAME_EXPR_RESULT_PTR          = 84
PRVM_FRAME_EXPR_RESULT_COUNT        = 88
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
PRVM_STATUS_INVALID_RESUME          = 10
PRVM_STATUS_EXPR_RESULT_INVALID     = 11
PRVM_STATUS_BUDGET_EXCEEDED         = 12

PRVM_ENTRY_KIND_OPASM_STATEMENT     = 1
PRVM_CALL_MODE_START                = 0
PRVM_CALL_MODE_RESUME               = 1
PRVM_ABI_VERSION_V1                 = 1
PRVM_PARSER_CONTRACT_VERSION_V2     = 2

PRVM_TOKEN_KIND_IDENTIFIER          = 0
PRVM_TOKEN_KIND_DOT                 = 7
PRVM_TOKEN_KIND_COMMA               = 4
PRVM_TOKEN_KIND_COLON               = 5

PRVM_RESULT_BEGIN_STATEMENT         = 1
PRVM_RESULT_LABEL_TEXT              = 2
PRVM_RESULT_MNEMONIC_TEXT           = 3
PRVM_RESULT_OPERAND_EXPR_SLOT       = 4
PRVM_RESULT_FINISH_LINE             = 5

PRVM_EXPR_REQUEST_RECORD_SIZE       = 32
PRVM_EXPR_RESULT_SLOT_SIZE          = 32
PRVM_EXPR_SLOT_READY                = 1
PRVM_EXPR_SLOT_READY_ERROR          = 2
PRVM_RESUME_MAGIC                   = $50525253
PRVM_RESUME_VERSION                 = 1
PRVM_RESUME_STATE_SIZE              = 40
PRVM_CONTINUATION_PARSE_OPERAND     = 1

PRVM_OPCODE_END                     = $00
PRVM_OPCODE_JUMP                    = $01
PRVM_OPCODE_JUMP_IF_FALSE           = $03
PRVM_OPCODE_CHECKPOINT              = $04
PRVM_OPCODE_ROLLBACK                = $05
PRVM_OPCODE_COMMIT                  = $06
PRVM_OPCODE_PEEK_KIND               = $10
PRVM_OPCODE_IS_EOL                  = $13
PRVM_OPCODE_PEEK_ASSIGNMENT         = $14
PRVM_OPCODE_PEEK_STAR_ORG           = $15
PRVM_OPCODE_ADVANCE                 = $20
PRVM_OPCODE_LOAD_IDENTIFIER         = $30
PRVM_OPCODE_PARSE_OPTIONAL_LABEL    = $40
PRVM_OPCODE_SCAN_COMMA_BOUNDARIES   = $41
PRVM_OPCODE_PARSE_OPERAND_EXPR      = $50
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
LOCAL_OPERAND_COUNT                 = 28
LOCAL_EXPR_START_TOKEN              = 32
LOCAL_EXPR_END_TOKEN                = 36
LOCAL_EXPR_SLOT_INDEX               = 40
LOCAL_LABEL_FLAG                    = 44
LOCAL_LABEL_COL_START               = 48
LOCAL_LABEL_COL_END                 = 52
LOCAL_LABEL_LEXEME_OFFSET           = 56
LOCAL_LABEL_LEXEME_LEN              = 60
LOCAL_BOOL_VALUE                    = 64
LOCAL_CHECKPOINT_DEPTH              = 68
LOCAL_CHECKPOINT_STACK              = 72
LOCAL_CHECKPOINT_RECORD_SIZE        = 28
LOCAL_CHECKPOINT_MAX_DEPTH          = 4
LOCAL_SIZE                          = 184

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
        CLR.L LOCAL_OPERAND_COUNT(A3)
        CLR.L LOCAL_LABEL_FLAG(A3)
        CLR.L LOCAL_BOOL_VALUE(A3)
        CLR.L LOCAL_CHECKPOINT_DEPTH(A3)

        CMPI.L #PRVM_MAGIC_OPRP, PRVM_FRAME_MAGIC(A4)
        BNE prvmInvalidArgumentWithLocals
        CMPI.W #PRVM_ABI_VERSION_V1, PRVM_FRAME_ABI_VERSION(A4)
        BNE prvmInvalidArgumentWithLocals
        MOVEQ #0, D0
        MOVE.W PRVM_FRAME_FRAME_SIZE(A4), D0
        CMPI.L #PRVM_REQUEST_FRAME_SIZE, D0
        BLT prvmInvalidArgumentWithLocals
        CMPI.W #PRVM_CALL_MODE_START, PRVM_FRAME_CALL_MODE(A4)
        BEQ prvmValidateEntryKind
        CMPI.W #PRVM_CALL_MODE_RESUME, PRVM_FRAME_CALL_MODE(A4)
        BNE prvmInvalidArgumentWithLocals
prvmValidateEntryKind:
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
        MOVE.L PRVM_FRAME_RESUME_CAPACITY(A4), D0
        CMPI.L #PRVM_RESUME_STATE_SIZE, D0
        BLT prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_EXPR_REQUEST_PTR(A4), D0
        TST.L D0
        BEQ prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_EXPR_REQUEST_SIZE(A4), D0
        CMPI.L #PRVM_EXPR_REQUEST_RECORD_SIZE, D0
        BLT prvmInvalidArgumentWithLocals
        MOVE.L PRVM_FRAME_EXPR_RESULT_COUNT(A4), D0
        BMI prvmInvalidArgumentWithLocals
        BEQ prvmValidateExpressionResultBufferDone
        MOVE.L PRVM_FRAME_EXPR_RESULT_PTR(A4), D7
        TST.L D7
        BEQ prvmInvalidArgumentWithLocals
prvmValidateExpressionResultBufferDone:

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
        CMPI.W #PRVM_CALL_MODE_RESUME, PRVM_FRAME_CALL_MODE(A4)
        BEQ prvmResumeFromExpression
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
        CMPI.B #PRVM_OPCODE_JUMP, D7
        BEQ prvmOpcodeJump
        CMPI.B #PRVM_OPCODE_JUMP_IF_FALSE, D7
        BEQ prvmOpcodeJumpIfFalse
        CMPI.B #PRVM_OPCODE_CHECKPOINT, D7
        BEQ prvmOpcodeCheckpoint
        CMPI.B #PRVM_OPCODE_ROLLBACK, D7
        BEQ prvmOpcodeRollback
        CMPI.B #PRVM_OPCODE_COMMIT, D7
        BEQ prvmOpcodeCommit
        CMPI.B #PRVM_OPCODE_PEEK_KIND, D7
        BEQ prvmOpcodePeekKind
        CMPI.B #PRVM_OPCODE_IS_EOL, D7
        BEQ prvmOpcodeIsEol
        CMPI.B #PRVM_OPCODE_PEEK_ASSIGNMENT, D7
        BEQ prvmOpcodePeekAssignment
        CMPI.B #PRVM_OPCODE_PEEK_STAR_ORG, D7
        BEQ prvmOpcodePeekStarOrg
        CMPI.B #PRVM_OPCODE_ADVANCE, D7
        BEQ prvmOpcodeAdvance
        CMPI.B #PRVM_OPCODE_LOAD_IDENTIFIER, D7
        BEQ prvmOpcodeLoadIdentifier
        CMPI.B #PRVM_OPCODE_PARSE_OPTIONAL_LABEL, D7
        BEQ prvmOpcodeParseOptionalLabel
        CMPI.B #PRVM_OPCODE_SCAN_COMMA_BOUNDARIES, D7
        BEQ prvmProgramLoop
        CMPI.B #PRVM_OPCODE_PARSE_OPERAND_EXPR, D7
        BEQ prvmOpcodeParseOperandExpr
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

prvmOpcodeJump:
        BSR.W prvmReadProgramTarget
        TST.L D0
        BNE prvmReturnWithLocals
        MOVEA.L D5, A5
        BRA prvmProgramLoop

prvmOpcodeJumpIfFalse:
        BSR.W prvmReadProgramTarget
        TST.L D0
        BNE prvmReturnWithLocals
        TST.L LOCAL_BOOL_VALUE(A3)
        BNE prvmProgramLoop
        MOVEA.L D5, A5
        BRA prvmProgramLoop

prvmOpcodeCheckpoint:
        BSR.W prvmPushCheckpoint
        TST.L D0
        BNE prvmReturnWithLocals
        BRA prvmProgramLoop

prvmOpcodeRollback:
        BSR.W prvmPopCheckpointAddress
        TST.L D0
        BNE prvmReturnWithLocals
        MOVE.L (A0)+, D2
        MOVE.L (A0)+, D1
        MOVE.L (A0)+, D3
        MOVE.L (A0)+, LOCAL_OPERAND_COUNT(A3)
        MOVE.L (A0)+, LOCAL_FINISHED_FLAG(A3)
        MOVE.L (A0)+, LOCAL_LABEL_FLAG(A3)
        MOVE.L (A0)+, LOCAL_BOOL_VALUE(A3)
        BRA prvmProgramLoop

prvmOpcodeCommit:
        BSR.W prvmPopCheckpointAddress
        TST.L D0
        BNE prvmReturnWithLocals
        BRA prvmProgramLoop

prvmOpcodePeekKind:
        CMPA.L A6, A5
        BCC prvmInvalidProgramAtCursor
        MOVEQ #0, D0
        MOVE.B (A5)+, D0
        BSR.W prvmPeekKind
        MOVE.L D0, LOCAL_BOOL_VALUE(A3)
        BRA prvmProgramLoop

prvmOpcodeIsEol:
        CLR.L LOCAL_BOOL_VALUE(A3)
        CMP.L D4, D2
        BCS prvmProgramLoop
        MOVE.L #1, LOCAL_BOOL_VALUE(A3)
        BRA prvmProgramLoop

prvmOpcodePeekAssignment:
        CLR.L LOCAL_BOOL_VALUE(A3)
        BRA prvmProgramLoop

prvmOpcodePeekStarOrg:
        CLR.L LOCAL_BOOL_VALUE(A3)
        BRA prvmProgramLoop

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

prvmOpcodeParseOptionalLabel:
        TST.L D2
        BNE prvmProgramLoop
        TST.L D4
        BEQ prvmProgramLoop
        CLR.L D0
        BSR.W prvmTokenPtrByIndex
        TST.L D0
        BNE prvmReturnWithLocals
        CMPI.W #PRVM_TOKEN_KIND_IDENTIFIER, 0(A1)
        BNE prvmProgramLoop
        CMPI.L #1, 4(A1)
        BNE prvmProgramLoop
        MOVE.L 12(A1), D0
        MOVE.L 16(A1), D7
        BEQ prvmInvalidTokenAtCursor
        MOVE.L D0, D5
        ADD.L D7, D5
        BCS prvmInvalidTokenAtCursor
        CMP.L PRVM_FRAME_LEXEME_LEN(A4), D5
        BHI prvmInvalidTokenAtCursor
        MOVE.L 4(A1), LOCAL_LABEL_COL_START(A3)
        MOVE.L 8(A1), LOCAL_LABEL_COL_END(A3)
        MOVE.L 12(A1), LOCAL_LABEL_LEXEME_OFFSET(A3)
        MOVE.L 16(A1), LOCAL_LABEL_LEXEME_LEN(A3)
        MOVE.L #1, LOCAL_LABEL_FLAG(A3)
        MOVEQ #1, D2
        CMPI.L #2, D4
        BCS prvmEmitOptionalLabel
        MOVEQ #1, D0
        BSR.W prvmTokenPtrByIndex
        TST.L D0
        BNE prvmReturnWithLocals
        CMPI.W #PRVM_TOKEN_KIND_COLON, 0(A1)
        BNE prvmEmitOptionalLabel
        MOVE.L 4(A1), D0
        CMP.L LOCAL_LABEL_COL_END(A3), D0
        BNE prvmEmitOptionalLabel
        MOVEQ #2, D2

prvmEmitOptionalLabel:
        BSR.W prvmEmitLabelText
        TST.L D0
        BNE prvmReturnWithLocals
        BRA prvmProgramLoop

prvmOpcodeBeginStatement:
        CLR.L LOCAL_LOADED_FLAG(A3)
        CLR.L LOCAL_FINISHED_FLAG(A3)
        CLR.L LOCAL_OPERAND_COUNT(A3)
        CLR.L LOCAL_LABEL_FLAG(A3)
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

prvmOpcodeParseOperandExpr:
        MOVEA.L A5, A0
        ADDA.L #4, A0
        CMPA.L A6, A0
        BHI prvmInvalidProgramAtCursor
        MOVE.B (A5)+, D0
        CMPI.B #$FF, D0
        BNE prvmUnsupportedOpcode
        MOVE.B (A5)+, D0
        CMPI.B #$FF, D0
        BNE prvmUnsupportedOpcode
        MOVE.B (A5)+, D0
        CMPI.B #$FF, D0
        BNE prvmUnsupportedOpcode
        MOVE.B (A5)+, D0
        CMPI.B #$FF, D0
        BNE prvmUnsupportedOpcode
        CMP.L D4, D2
        BCC prvmProgramLoop
        BRA prvmRequestOperandAtCursor

prvmRequestOperandAtCursor:
        MOVE.L D2, LOCAL_EXPR_START_TOKEN(A3)
        MOVE.L D2, D5
prvmFindOperandEndLoop:
        CMP.L D4, D5
        BCC prvmOperandEndFound
        MOVE.L D5, D0
        BSR.W prvmTokenPtrByIndex
        TST.L D0
        BNE prvmReturnWithLocals
        CMPI.W #PRVM_TOKEN_KIND_COMMA, 0(A1)
        BEQ prvmOperandEndFound
        ADDQ.L #1, D5
        BRA prvmFindOperandEndLoop

prvmOperandEndFound:
        MOVE.L D5, LOCAL_EXPR_END_TOKEN(A3)
        MOVE.L LOCAL_OPERAND_COUNT(A3), D0
        MOVE.L D0, LOCAL_EXPR_SLOT_INDEX(A3)
        BSR.W prvmWriteExpressionRequest
        TST.L D0
        BNE prvmReturnWithLocals
        BSR.W prvmWriteResumeState
        TST.L D0
        BNE prvmReturnWithLocals
        MOVE.L LOCAL_EXPR_SLOT_INDEX(A3), D1
        MOVE.L LOCAL_EXPR_START_TOKEN(A3), D2
        MOVE.L #PRVM_RESUME_STATE_SIZE, D3
        MOVEQ #PRVM_STATUS_EXPR_REQUEST, D0
        BRA prvmReturnWithLocals

prvmResumeFromExpression:
        MOVEA.L PRVM_FRAME_RESUME_PTR(A4), A2
        CMPI.L #PRVM_RESUME_MAGIC, 0(A2)
        BNE prvmInvalidResume
        CMPI.W #PRVM_RESUME_VERSION, 4(A2)
        BNE prvmInvalidResume
        CMPI.W #PRVM_RESUME_STATE_SIZE, 6(A2)
        BLT prvmInvalidResume
        CMPI.L #PRVM_CONTINUATION_PARSE_OPERAND, 8(A2)
        BNE prvmInvalidResume
        MOVE.L 12(A2), LOCAL_EXPR_SLOT_INDEX(A3)
        MOVE.L 20(A2), D2
        MOVE.L 24(A2), D1
        MOVE.L 28(A2), LOCAL_OPERAND_COUNT(A3)
        MOVE.L 32(A2), LOCAL_EXPR_START_TOKEN(A3)
        MOVE.L 36(A2), LOCAL_EXPR_END_TOKEN(A3)
        MOVE.L PRVM_FRAME_PROGRAM_PTR(A4), D0
        ADD.L 16(A2), D0
        MOVEA.L D0, A5
        CMPA.L A6, A5
        BHI prvmInvalidResume
        BSR.W prvmValidateExpressionResultSlot
        TST.L D0
        BNE prvmReturnWithLocals
        BSR.W prvmEmitOperandExprSlot
        TST.L D0
        BNE prvmReturnWithLocals
        MOVE.L LOCAL_OPERAND_COUNT(A3), D0
        ADDQ.L #1, D0
        MOVE.L D0, LOCAL_OPERAND_COUNT(A3)
        CMP.L D4, D2
        BCS prvmRequestOperandAtCursor
        BRA prvmProgramLoop

prvmCurrentTokenPtr:
        MOVE.L D2, D0
        BRA prvmTokenPtrByIndex

prvmTokenPtrByIndex:
        CMP.L D4, D0
        BCC prvmCurrentTokenInvalid
        LSL.L #4, D0
        MOVE.L D0, D7
        LSR.L #4, D7
        LSL.L #2, D7
        ADD.L D7, D0
        MOVEA.L PRVM_FRAME_TOKEN_PTR(A4), A1
        ADDA.L D0, A1
        CLR.L D0
        RTS

prvmReadProgramTarget:
        MOVEA.L A5, A0
        ADDA.L #2, A0
        CMPA.L A6, A0
        BHI prvmInvalidProgramAtCursor
        MOVEQ #0, D5
        MOVE.B (A5)+, D5
        MOVEQ #0, D7
        MOVE.B (A5)+, D7
        LSL.L #8, D7
        OR.L D7, D5
        MOVE.L PRVM_FRAME_PROGRAM_PTR(A4), D0
        ADD.L D5, D0
        MOVEA.L D0, A0
        CMPA.L A6, A0
        BHI prvmInvalidProgramAtCursor
        MOVE.L A0, D5
        CLR.L D0
        RTS

prvmPushCheckpoint:
        MOVE.L LOCAL_CHECKPOINT_DEPTH(A3), D0
        CMPI.L #LOCAL_CHECKPOINT_MAX_DEPTH, D0
        BCC prvmInvalidProgramAtCursor
        BSR.W prvmCheckpointAddressForDepth
        MOVE.L D2, (A0)+
        MOVE.L D1, (A0)+
        MOVE.L D3, (A0)+
        MOVE.L LOCAL_OPERAND_COUNT(A3), (A0)+
        MOVE.L LOCAL_FINISHED_FLAG(A3), (A0)+
        MOVE.L LOCAL_LABEL_FLAG(A3), (A0)+
        MOVE.L LOCAL_BOOL_VALUE(A3), (A0)+
        ADDQ.L #1, LOCAL_CHECKPOINT_DEPTH(A3)
        CLR.L D0
        RTS

prvmPopCheckpointAddress:
        MOVE.L LOCAL_CHECKPOINT_DEPTH(A3), D0
        BEQ prvmInvalidProgramAtCursor
        SUBQ.L #1, D0
        MOVE.L D0, LOCAL_CHECKPOINT_DEPTH(A3)
        BSR.W prvmCheckpointAddressForDepth
        CLR.L D0
        RTS

prvmCheckpointAddressForDepth:
        MOVE.L D0, D5
        LSL.L #5, D5
        MOVE.L D0, D7
        LSL.L #2, D7
        SUB.L D7, D5
        LEA LOCAL_CHECKPOINT_STACK(A3), A0
        ADDA.L D5, A0
        RTS

prvmPeekKind:
        CMP.L D4, D2
        BCC prvmPeekKindFalse
        MOVE.L D2, D5
        LSL.L #4, D5
        MOVE.L D2, D7
        LSL.L #2, D7
        ADD.L D7, D5
        MOVEA.L PRVM_FRAME_TOKEN_PTR(A4), A1
        ADDA.L D5, A1
        CMPI.B #$03, D0
        BEQ prvmPeekKindDot
        BRA prvmPeekKindFalse

prvmPeekKindDot:
        CMPI.W #PRVM_TOKEN_KIND_DOT, 0(A1)
        BNE prvmPeekKindFalse
        MOVEQ #1, D0
        RTS

prvmPeekKindFalse:
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

prvmEmitLabelText:
        TST.L LOCAL_LABEL_FLAG(A3)
        BEQ prvmEmitRecordReturn
        BSR.W prvmResultRecordPtr
        TST.L D0
        BNE prvmEmitRecordReturn
        MOVE.W #PRVM_RESULT_LABEL_TEXT, 0(A2)
        CLR.W 2(A2)
        MOVE.L PRVM_FRAME_LINE_NUM(A4), 4(A2)
        MOVE.L LOCAL_LABEL_COL_START(A3), 8(A2)
        MOVE.L LOCAL_LABEL_COL_END(A3), 12(A2)
        MOVE.L LOCAL_LABEL_LEXEME_OFFSET(A3), 16(A2)
        MOVE.L LOCAL_LABEL_LEXEME_LEN(A3), 20(A2)
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

prvmEmitOperandExprSlot:
        BSR.W prvmResultRecordPtr
        TST.L D0
        BNE prvmEmitRecordReturn
        MOVE.W #PRVM_RESULT_OPERAND_EXPR_SLOT, 0(A2)
        CLR.W 2(A2)
        MOVE.L 8(A1), 4(A2)
        MOVE.L 12(A1), 8(A2)
        MOVE.L 16(A1), 12(A2)
        MOVE.L LOCAL_OPERAND_COUNT(A3), 16(A2)
        MOVE.L LOCAL_EXPR_SLOT_INDEX(A3), 20(A2)
        MOVE.L LOCAL_EXPR_START_TOKEN(A3), 24(A2)
        MOVE.L LOCAL_EXPR_END_TOKEN(A3), 28(A2)
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

prvmWriteExpressionRequest:
        MOVEA.L PRVM_FRAME_EXPR_REQUEST_PTR(A4), A2
        MOVE.W #1, 0(A2)
        CLR.W 2(A2)
        MOVE.L LOCAL_OPERAND_COUNT(A3), 4(A2)
        MOVE.L LOCAL_EXPR_SLOT_INDEX(A3), 8(A2)
        MOVE.L LOCAL_EXPR_START_TOKEN(A3), 12(A2)
        MOVE.L LOCAL_EXPR_END_TOKEN(A3), 16(A2)
        MOVE.L LOCAL_EXPR_START_TOKEN(A3), D0
        CMP.L D4, D0
        BCC prvmWriteExpressionRequestEndSpan
        BSR.W prvmTokenPtrByIndex
        TST.L D0
        BNE prvmWriteExpressionRequestReturn
        MOVE.L PRVM_FRAME_LINE_NUM(A4), 20(A2)
        MOVE.L 4(A1), 24(A2)
        MOVE.L 8(A1), 28(A2)
        CLR.L D0
        RTS

prvmWriteExpressionRequestEndSpan:
        MOVE.L PRVM_FRAME_LINE_NUM(A4), 20(A2)
        CLR.L 24(A2)
        CLR.L 28(A2)
        CLR.L D0
prvmWriteExpressionRequestReturn:
        RTS

prvmWriteResumeState:
        MOVEA.L PRVM_FRAME_RESUME_PTR(A4), A2
        MOVE.L #PRVM_RESUME_MAGIC, 0(A2)
        MOVE.W #PRVM_RESUME_VERSION, 4(A2)
        MOVE.W #PRVM_RESUME_STATE_SIZE, 6(A2)
        MOVE.L #PRVM_CONTINUATION_PARSE_OPERAND, 8(A2)
        MOVE.L LOCAL_EXPR_SLOT_INDEX(A3), 12(A2)
        MOVE.L A5, D0
        SUB.L PRVM_FRAME_PROGRAM_PTR(A4), D0
        MOVE.L D0, 16(A2)
        MOVE.L LOCAL_EXPR_END_TOKEN(A3), D0
        CMP.L D4, D0
        BCC prvmWriteResumeCursor
        ADDQ.L #1, D0
prvmWriteResumeCursor:
        MOVE.L D0, 20(A2)
        MOVE.L D1, 24(A2)
        MOVE.L LOCAL_OPERAND_COUNT(A3), 28(A2)
        MOVE.L LOCAL_EXPR_START_TOKEN(A3), 32(A2)
        MOVE.L LOCAL_EXPR_END_TOKEN(A3), 36(A2)
        CLR.L D0
        RTS

prvmValidateExpressionResultSlot:
        MOVE.L LOCAL_EXPR_SLOT_INDEX(A3), D0
        CMP.L PRVM_FRAME_EXPR_RESULT_COUNT(A4), D0
        BCC prvmExpressionResultInvalid
        LSL.L #5, D0
        MOVEA.L PRVM_FRAME_EXPR_RESULT_PTR(A4), A1
        ADDA.L D0, A1
        MOVE.W 0(A1), D0
        CMPI.W #PRVM_EXPR_SLOT_READY, D0
        BEQ prvmValidateExpressionResultReady
        CMPI.W #PRVM_EXPR_SLOT_READY_ERROR, D0
        BNE prvmExpressionResultInvalid
prvmValidateExpressionResultReady:
        TST.W 2(A1)
        BNE prvmExpressionResultInvalid
        MOVE.L 4(A1), D0
        CMP.L LOCAL_EXPR_SLOT_INDEX(A3), D0
        BNE prvmExpressionResultInvalid
        CMPI.L #$FFFFFFFF, 24(A1)
        BNE prvmExpressionResultInvalid
        TST.L 28(A1)
        BNE prvmExpressionResultInvalid
        CLR.L D0
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

prvmInvalidResume:
        CLR.L D1
        CLR.L D3
        MOVEQ #PRVM_STATUS_INVALID_RESUME, D0
        BRA prvmReturnWithLocals

prvmExpressionResultInvalid:
        MOVE.L LOCAL_EXPR_SLOT_INDEX(A3), D1
        CLR.L D3
        MOVEQ #PRVM_STATUS_EXPR_RESULT_INVALID, D0
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