; Package-backed tokenizer-VM wrapper for the first tkpkg tokenize_line slice.

        .module tkpkg.amigaos.tokenizer_vm
        .cpu 68020
        .pub
        .use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN)
        .use tkpkg.amigaos.abi (STATUS_BAD_REQUEST_V1, STATUS_RUNTIME_ERROR_V1)
        .use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_CAPACITY, PACKAGE_STATE_PIPELINE_ACTIVE)
        .use tkpkg.amigaos.buffers (TOKEN_BUFFER_CAPACITY, TOKEN_RECORD_SIZE)
        .use tkpkg.amigaos.buffers (TOKEN_SCRATCH_CAPACITY, TOKENIZER_VM_STATE_TABLE_CAPACITY)
        .use tkpkg.amigaos.buffers (packageStateFlags, packageStorage, lastErrorBuffer)
        .use tkpkg.amigaos.buffers (activeTokenizerVmOffsetLo)
        .use tkpkg.amigaos.buffers (activeTokenizerVmStartStateLo, activeTokenizerVmStartStateHi)
        .use tkpkg.amigaos.buffers (activeTokenizerVmStateCountLo, activeTokenizerVmStateCountHi)
        .use tkpkg.amigaos.buffers (activeTokenizerVmStateTable)
        .use tkpkg.amigaos.buffers (activeTokenizerVmMaxErrorsPerLine)
        .use tkpkg.amigaos.buffers (activeTokenizerVmInvalidCharDiagLen, activeTokenizerVmInvalidCharDiagCode)
        .use tkpkg.amigaos.buffers (activeTokenizerVmUnterminatedStringDiagLen, activeTokenizerVmUnterminatedStringDiagCode)
        .use tkpkg.amigaos.buffers (activeTokenizerVmStepLimitDiagLen, activeTokenizerVmStepLimitDiagCode)
        .use tkpkg.amigaos.buffers (activeTokenizerVmTokenLimitDiagLen, activeTokenizerVmTokenLimitDiagCode)
        .use tkpkg.amigaos.buffers (activeTokenizerVmLexemeLimitDiagLen, activeTokenizerVmLexemeLimitDiagCode)
        .use tkpkg.amigaos.buffers (activeTokenizerVmErrorLimitDiagLen, activeTokenizerVmErrorLimitDiagCode)
        .use tkpkg.amigaos.buffers (tokenRecordBuffer, tokenScratchBuffer)
        .use tkpkg.amigaos.buffers (lastTokenCount, lastLexemeLen)
        .use tokvm.amigaos.tokenizer_vm (tokvm_run_68000, tokvm_set_step_budget_68000)
        .use tokvm.amigaos.tokenizer_vm (tokvm_set_program_state_table_68000)
        .use tokvm.amigaos.tokenizer_vm (tokvm_read_last_failure_68000)
        .use tokvm.amigaos.tokenizer_vm (TOKVM_DEFAULT_MAX_STEPS_PER_LINE)
        .use tokvm.amigaos.tokenizer_vm (TK_STATUS_SUCCESS, TK_STATUS_NEWLINE_UNSUPPORTED)
        .use tokvm.amigaos.tokenizer_vm (TK_STATUS_TOKEN_OVERFLOW, TK_STATUS_LEXEME_OVERFLOW)
        .use tokvm.amigaos.tokenizer_vm (TK_STATUS_VM_FAILURE, TK_STATUS_INVALID_ARGUMENT)
        .use tokvm.amigaos.tokenizer_vm (TK_STATUS_INVALID_PROGRAM, TK_STATUS_STEP_LIMIT_EXCEEDED)
        .use tokvm.amigaos.tokenizer_vm (TK_VM_FAILURE_KIND_FAIL, TK_VM_FAILURE_KIND_EMIT_DIAG)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_IDENTIFIER, TK_KIND_NUMBER, TK_KIND_STRING)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_COMMA, TK_KIND_COLON, TK_KIND_DOLLAR)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_DOT, TK_KIND_HASH, TK_KIND_QUESTION)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OPEN_BRACKET, TK_KIND_CLOSE_BRACKET)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OPEN_BRACE, TK_KIND_CLOSE_BRACE)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OPEN_PAREN, TK_KIND_CLOSE_PAREN)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_RANGE, TK_KIND_OP_RANGE_INCLUSIVE)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_PLUS, TK_KIND_OP_MINUS)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_MULTIPLY, TK_KIND_OP_POWER)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_DIVIDE, TK_KIND_OP_MOD)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_SHL, TK_KIND_OP_SHR)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_BIT_NOT, TK_KIND_OP_LOGIC_NOT)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_BIT_AND, TK_KIND_OP_BIT_OR)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_BIT_XOR)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_LOGIC_AND, TK_KIND_OP_LOGIC_OR)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_LOGIC_XOR, TK_KIND_OP_EQ)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_NE, TK_KIND_OP_GE)
        .use tokvm.amigaos.tokenizer_vm (TK_KIND_OP_GT, TK_KIND_OP_LE, TK_KIND_OP_LT)

TKVM_OPCODE_VERSION_V1                = 1
TKVM_STREAM_VERSION_V1                = 1
TKVM_STREAM_MODE_LINE                 = 1

NO_PIPELINE_TEXT_LEN                  = 40
BAD_PAYLOAD_TEXT_LEN                  = 42
NEWLINE_TEXT_LEN                      = 38
STEP_LIMIT_TEXT_LEN                   = 38
TOKEN_OVERFLOW_TEXT_LEN               = 33
LEXEME_OVERFLOW_TEXT_LEN              = 34
INVALID_PROGRAM_TEXT_LEN              = 35
BAD_PROGRAM_HEADER_TEXT_LEN           = 36
INVALID_ARGUMENT_TEXT_LEN             = 36
OUTPUT_OVERFLOW_TEXT_LEN              = 34
STEP_LIMIT_SUFFIX_LEN                 = 32
TOKEN_OVERFLOW_SUFFIX_LEN             = 26
LEXEME_OVERFLOW_SUFFIX_LEN            = 27
VM_FAILURE_REASON_SUFFIX_LEN          = 30
VM_EMIT_DIAG_SUFFIX_LEN               = 39
VM_DIAG_BUDGET_EXCEEDED_LEN           = 47
VM_FAILURE_FALLBACK_TEXT_LEN          = 28

LOCAL_OUTPUT_LEN                      = 0
LOCAL_OUTPUT_OVERFLOW                 = 4
LOCAL_RENDER_LINE                     = 8
LOCAL_DECIMAL_EMITTED                 = 12
LOCAL_SIZE                            = 16

        .section data, kind=data

noPipelineText:
        .byte "OTR001: tokenize_line requires set_pipeline", 0

badPayloadText:
        .byte "OTR002: tokenize_line requires a 4-byte line prefix", 0

newlineText:
        .byte "OTR004: tokenize_line rejects newline bytes", 0

stepLimitText:
        .byte "OTR901: tokenizer step budget exceeded", 0

tokenOverflowText:
        .byte "OTR901: tokenizer token overflow", 0

lexemeOverflowText:
        .byte "OTR901: tokenizer lexeme overflow", 0

invalidProgramText:
        .byte "OTR901: invalid tokenizer VM program", 0

badProgramHeaderText:
        .byte "OTR901: bad tokenizer VM header bytes", 0

invalidArgumentText:
        .byte "OTR901: invalid tokenize_line arguments", 0

outputOverflowText:
        .byte "OTR901: tokenizer output overflow", 0

stepLimitSuffixText:
        .byte ": tokenizer step budget exceeded"

tokenOverflowSuffixText:
        .byte ": tokenizer token overflow"

lexemeOverflowSuffixText:
        .byte ": tokenizer lexeme overflow"

vmFailureReasonSuffixText:
        .byte ": tokenizer VM failure reason "

vmEmitDiagSuffixText:
        .byte ": tokenizer VM emitted diagnostic slot "

vmDiagBudgetExceededText:
        .byte ": tokenizer VM diagnostic budget exceeded (1/0)"

vmFailureFallbackText:
        .byte "OTR901: tokenizer VM failure", 0

identifierPrefix:
        .byte "Identifier("

numberPrefix:
        .byte "Number { text: "

numberBasePrefix:
        .byte ", base: "

stringPrefix:
        .byte "String { raw: "

stringBytesPrefix:
        .byte ", bytes: ["

stringSuffix:
        .byte "] }"

operatorPrefix:
        .byte "Operator("

atSep:
        .byte "@"

colonSep:
        .byte ":"

dashSep:
        .byte "-"

newlineSep:
        .byte 10

closeParenText:
        .byte ")"

commaSpaceText:
        .byte ", "

commaText:
        .byte ","

hexDigits:
        .byte "0123456789ABCDEF"

decimalPowers:
        .long 1000000000
        .long 100000000
        .long 10000000
        .long 1000000
        .long 100000
        .long 10000
        .long 1000
        .long 100
        .long 10
        .long 1

kindCommaText:
        .byte "Comma"
kindColonText:
        .byte "Colon"
kindDollarText:
        .byte "Dollar"
kindDotText:
        .byte "Dot"
kindHashText:
        .byte "Hash"
kindQuestionText:
        .byte "Question"
kindOpenBracketText:
        .byte "OpenBracket"
kindCloseBracketText:
        .byte "CloseBracket"
kindOpenBraceText:
        .byte "OpenBrace"
kindCloseBraceText:
        .byte "CloseBrace"
kindOpenParenText:
        .byte "OpenParen"
kindCloseParenText:
        .byte "CloseParen"

opRangeText:
        .byte "Range"
opRangeInclusiveText:
        .byte "RangeInclusive"
opPlusText:
        .byte "Plus"
opMinusText:
        .byte "Minus"
opMultiplyText:
        .byte "Multiply"
opPowerText:
        .byte "Power"
opDivideText:
        .byte "Divide"
opModText:
        .byte "Mod"
opShlText:
        .byte "Shl"
opShrText:
        .byte "Shr"
opBitNotText:
        .byte "BitNot"
opLogicNotText:
        .byte "LogicNot"
opBitAndText:
        .byte "BitAnd"
opBitOrText:
        .byte "BitOr"
opBitXorText:
        .byte "BitXor"
opLogicAndText:
        .byte "LogicAnd"
opLogicOrText:
        .byte "LogicOr"
opLogicXorText:
        .byte "LogicXor"
opEqText:
        .byte "Eq"
opNeText:
        .byte "Ne"
opGeText:
        .byte "Ge"
opGtText:
        .byte "Gt"
opLeText:
        .byte "Le"
opLtText:
        .byte "Lt"

        .endsection

        .section code, kind=code

tkpkg_tokenizer_vm_tokenize_line_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        BTST #1, packageStateFlags
        BNE.S tkpkgTokenizerPipelineReady
        LEA noPipelineText, A1
        MOVEQ #NO_PIPELINE_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        BRA.W tkpkgTokenizerDone

tkpkgTokenizerPipelineReady:
        BSR.W tkpkg_tokenizer_vm_read_line_payload_v1
        TST.B D0
        BNE.W tkpkgTokenizerDone
        BSR.W tkpkg_tokenizer_vm_read_program_v1
        TST.B D0
        BNE.W tkpkgTokenizerDone
        MOVE.L #TOKVM_DEFAULT_MAX_STEPS_PER_LINE, D0
        JSR tokvm_set_step_budget_68000
        MOVEA.L A3, A5
        MOVE.L D3, D7
        CMPI.B #1, (A5)
        BNE.W tkpkgTokenizerDebugBadProgramHeader
        CMPI.B #8, 1(A5)
        BNE.W tkpkgTokenizerDebugBadProgramHeader
        MOVE.L D6, -(SP)
        LEA activeTokenizerVmStateTable, A0
        MOVEQ #0, D0
        MOVE.B activeTokenizerVmStateCountLo, D0
        MOVEQ #0, D1
        MOVE.B activeTokenizerVmStateCountHi, D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D1
        MOVE.B activeTokenizerVmStartStateLo, D1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmStartStateHi, D2
        LSL.W #8, D2
        OR.W D2, D1
        JSR tokvm_set_program_state_table_68000
        MOVEA.L A4, A0
        MOVE.L D4, D0
        LEA tokenRecordBuffer, A1
        MOVEQ #0, D1
        MOVE.W #TOKEN_BUFFER_CAPACITY, D1
        LEA tokenScratchBuffer, A2
        MOVEQ #0, D2
        MOVE.W #TOKEN_SCRATCH_CAPACITY, D2
        MOVEA.L A5, A3
        MOVE.L D7, D3
        JSR tokvm_run_68000
        MOVE.L (SP)+, D6
        CMPI.B #TK_STATUS_SUCCESS, D0
        BEQ.S tkpkgTokenizerRender
        BSR.W tkpkg_tokenizer_vm_status_message_v1
        BRA.W tkpkgTokenizerDone

tkpkgTokenizerRender:
        MOVE.W D1, lastTokenCount
        MOVE.W D3, lastLexemeLen
        BSR.W tkpkg_tokenizer_vm_validate_result_v1
        TST.B D0
        BNE.W tkpkgTokenizerInvalidProgram
        BSR.W tkpkg_tokenizer_vm_render_output_v1

tkpkgTokenizerDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_tokenizer_vm_read_line_payload_v1:
        MOVEQ #0, D0
        MOVE.B CB_INPUT_LEN(A0), D0
        MOVEQ #0, D1
        MOVE.B 19(A0), D1
        LSL.W #8, D1
        OR.W D1, D0
        CMPI.W #4, D0
        BLO.S tkpkgTokenizerBadPayload
        MOVE.W D0, D4
        SUBQ.W #4, D4
        MOVEQ #0, D0
        MOVE.B CB_INPUT_PTR(A0), D0
        MOVEQ #0, D1
        MOVE.B 17(A0), D1
        LSL.W #8, D1
        OR.W D1, D0
        TST.W D0
        BEQ.S tkpkgTokenizerBadPayload
        LEA 0(A0, D0.W), A4
        MOVEQ #0, D6
        MOVE.B (A4)+, D6
        MOVEQ #0, D1
        MOVE.B (A4)+, D1
        LSL.L #8, D1
        OR.L D1, D6
        MOVEQ #0, D1
        MOVE.B (A4)+, D1
        LSL.L #8, D1
        LSL.L #8, D1
        OR.L D1, D6
        MOVEQ #0, D1
        MOVE.B (A4)+, D1
        LSL.L #8, D1
        LSL.L #8, D1
        LSL.L #8, D1
        OR.L D1, D6
        MOVEQ #0, D0
        RTS

tkpkgTokenizerBadPayload:
        LEA badPayloadText, A1
        MOVEQ #BAD_PAYLOAD_TEXT_LEN, D1
        MOVEQ #STATUS_BAD_REQUEST_V1, D0
        RTS

tkpkg_tokenizer_vm_read_program_v1:
        LEA activeTokenizerVmOffsetLo, A1
        MOVEQ #0, D0
        MOVE.B (A1)+, D0
        MOVEQ #0, D1
        MOVE.B (A1)+, D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D2
        MOVE.B (A1)+, D2
        MOVEQ #0, D1
        MOVE.B (A1)+, D1
        LSL.W #8, D1
        OR.W D1, D2
        TST.W D2
        BEQ.W tkpkgTokenizerInvalidProgram
        LEA packageStorage, A2
        LEA 0(A2, D0.W), A2
        MOVEA.L A2, A6
        ADDA.L D2, A6
        MOVEQ #1, D0
        BSR.W tkpkg_tokenizer_vm_require_bytes_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        ADDQ.W #1, A2
        BSR.W tkpkg_tokenizer_vm_skip_string_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        BSR.W tkpkg_tokenizer_vm_read_u16_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        CMPI.W #TKVM_OPCODE_VERSION_V1, D0
        BNE.W tkpkgTokenizerInvalidProgram
        BSR.W tkpkg_tokenizer_vm_read_u16_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        MOVE.B D0, activeTokenizerVmStartStateLo
        LSR.W #8, D0
        MOVE.B D0, activeTokenizerVmStartStateHi
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        TST.L D0
        BEQ.W tkpkgTokenizerInvalidProgram
        CMPI.L #TOKENIZER_VM_STATE_TABLE_CAPACITY, D0
        BHI.W tkpkgTokenizerInvalidProgram
        MOVE.B D0, activeTokenizerVmStateCountLo
        LSR.L #8, D0
        MOVE.B D0, activeTokenizerVmStateCountHi
        MOVEQ #0, D0
        MOVE.B activeTokenizerVmStateCountLo, D0
        MOVEQ #0, D1
        MOVE.B activeTokenizerVmStateCountHi, D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVE.W D0, D7
        LEA activeTokenizerVmStateTable, A3
        SUBQ.W #1, D7

tkpkgTokenizerSkipStateOffsets:
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        MOVE.L D0, (A3)+
        DBF D7, tkpkgTokenizerSkipStateOffsets
        BSR.W tkpkg_tokenizer_vm_read_u16_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        CMPI.W #TKVM_STREAM_VERSION_V1, D0
        BNE.W tkpkgTokenizerInvalidProgram
        MOVEQ #1, D0
        BSR.W tkpkg_tokenizer_vm_require_bytes_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        MOVEQ #0, D0
        MOVE.B (A2)+, D0
        CMPI.B #TKVM_STREAM_MODE_LINE, D0
        BNE.W tkpkgTokenizerInvalidProgram
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        MOVE.L D0, D5
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        MOVE.L D0, activeTokenizerVmMaxErrorsPerLine
        LEA activeTokenizerVmInvalidCharDiagCode, A3
        LEA activeTokenizerVmInvalidCharDiagLen, A1
        BSR.W tkpkg_tokenizer_vm_read_string_into_slot_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        LEA activeTokenizerVmUnterminatedStringDiagCode, A3
        LEA activeTokenizerVmUnterminatedStringDiagLen, A1
        BSR.W tkpkg_tokenizer_vm_read_string_into_slot_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        LEA activeTokenizerVmStepLimitDiagCode, A3
        LEA activeTokenizerVmStepLimitDiagLen, A1
        BSR.W tkpkg_tokenizer_vm_read_string_into_slot_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        LEA activeTokenizerVmTokenLimitDiagCode, A3
        LEA activeTokenizerVmTokenLimitDiagLen, A1
        BSR.W tkpkg_tokenizer_vm_read_string_into_slot_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        LEA activeTokenizerVmLexemeLimitDiagCode, A3
        LEA activeTokenizerVmLexemeLimitDiagLen, A1
        BSR.W tkpkg_tokenizer_vm_read_string_into_slot_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        LEA activeTokenizerVmErrorLimitDiagCode, A3
        LEA activeTokenizerVmErrorLimitDiagLen, A1
        BSR.W tkpkg_tokenizer_vm_read_string_into_slot_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        BSR.W tkpkg_tokenizer_vm_read_bytes_field_v1
        TST.B D1
        BNE.W tkpkgTokenizerInvalidProgram
        TST.W D3
        BEQ.W tkpkgTokenizerInvalidProgram
        MOVEQ #0, D0
        MOVE.B activeTokenizerVmStartStateLo, D0
        MOVEQ #0, D1
        MOVE.B activeTokenizerVmStartStateHi, D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D1
        MOVE.B activeTokenizerVmStateCountLo, D1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmStateCountHi, D2
        LSL.W #8, D2
        OR.W D2, D1
        CMP.W D1, D0
        BCC.W tkpkgTokenizerInvalidProgram
        MOVEQ #0, D0
        RTS

tkpkgTokenizerInvalidProgram:
        LEA invalidProgramText, A1
        MOVEQ #INVALID_PROGRAM_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgTokenizerDebugBadProgramHeader:
        LEA badProgramHeaderText, A1
        MOVEQ #BAD_PROGRAM_HEADER_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkg_tokenizer_vm_status_message_v1:
        CMPI.B #TK_STATUS_NEWLINE_UNSUPPORTED, D0
        BEQ.S tkpkgTokenizerStatusNewline
        CMPI.B #TK_STATUS_STEP_LIMIT_EXCEEDED, D0
        BEQ.S tkpkgTokenizerStatusStepLimit
        CMPI.B #TK_STATUS_TOKEN_OVERFLOW, D0
        BEQ.S tkpkgTokenizerStatusTokenOverflow
        CMPI.B #TK_STATUS_LEXEME_OVERFLOW, D0
        BEQ.S tkpkgTokenizerStatusLexemeOverflow
        CMPI.B #TK_STATUS_VM_FAILURE, D0
        BNE.S tkpkgTokenizerStatusCheckInvalidArgument
        BRA.W tkpkgTokenizerStatusVmFailure
tkpkgTokenizerStatusCheckInvalidArgument:
        CMPI.B #TK_STATUS_INVALID_ARGUMENT, D0
        BNE.S tkpkgTokenizerStatusFallbackInvalidProgram
        BRA.W tkpkgTokenizerStatusInvalidArgument
tkpkgTokenizerStatusFallbackInvalidProgram:
        LEA invalidProgramText, A1
        MOVEQ #INVALID_PROGRAM_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgTokenizerStatusNewline:
        LEA newlineText, A1
        MOVEQ #NEWLINE_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgTokenizerStatusStepLimit:
        BSR.W tkpkg_tokenizer_vm_begin_status_buffer_v1
        MOVEQ #2, D0
        BSR.W tkpkg_tokenizer_vm_get_diag_code_v1
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        LEA stepLimitSuffixText, A1
        MOVEQ #STEP_LIMIT_SUFFIX_LEN, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        BRA.W tkpkg_tokenizer_vm_finish_status_buffer_v1

tkpkgTokenizerStatusTokenOverflow:
        BSR.W tkpkg_tokenizer_vm_begin_status_buffer_v1
        MOVEQ #3, D0
        BSR.W tkpkg_tokenizer_vm_get_diag_code_v1
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        LEA tokenOverflowSuffixText, A1
        MOVEQ #TOKEN_OVERFLOW_SUFFIX_LEN, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        BRA.W tkpkg_tokenizer_vm_finish_status_buffer_v1

tkpkgTokenizerStatusLexemeOverflow:
        BSR.W tkpkg_tokenizer_vm_begin_status_buffer_v1
        MOVEQ #4, D0
        BSR.W tkpkg_tokenizer_vm_get_diag_code_v1
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        LEA lexemeOverflowSuffixText, A1
        MOVEQ #LEXEME_OVERFLOW_SUFFIX_LEN, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        BRA.W tkpkg_tokenizer_vm_finish_status_buffer_v1

tkpkgTokenizerStatusVmFailure:
        JSR tokvm_read_last_failure_68000
        CMPI.W #TK_VM_FAILURE_KIND_FAIL, D0
        BEQ.W tkpkgTokenizerStatusVmFailReason
        CMPI.W #TK_VM_FAILURE_KIND_EMIT_DIAG, D0
        BEQ.W tkpkgTokenizerStatusVmEmitDiag
        LEA vmFailureFallbackText, A1
        MOVEQ #VM_FAILURE_FALLBACK_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgTokenizerStatusVmFailReason:
        BSR.W tkpkg_tokenizer_vm_begin_status_buffer_v1
        MOVE.L D1, D6
        MOVEQ #0, D0
        BSR.W tkpkg_tokenizer_vm_get_diag_code_v1
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        LEA vmFailureReasonSuffixText, A1
        MOVEQ #VM_FAILURE_REASON_SUFFIX_LEN, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVE.L D6, D0
        BSR.W tkpkg_tokenizer_vm_append_u32_v1
        BRA.W tkpkg_tokenizer_vm_finish_status_buffer_v1

tkpkgTokenizerStatusVmEmitDiag:
        MOVE.L D1, D6
        TST.L activeTokenizerVmMaxErrorsPerLine
        BEQ.S tkpkgTokenizerStatusVmDiagBudgetExceeded
        BSR.W tkpkg_tokenizer_vm_begin_status_buffer_v1
        MOVE.L D6, D0
        BSR.W tkpkg_tokenizer_vm_get_diag_code_v1
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        LEA vmEmitDiagSuffixText, A1
        MOVEQ #VM_EMIT_DIAG_SUFFIX_LEN, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVE.L D6, D0
        BSR.W tkpkg_tokenizer_vm_append_u32_v1
        BRA.W tkpkg_tokenizer_vm_finish_status_buffer_v1

tkpkgTokenizerStatusVmDiagBudgetExceeded:
        BSR.W tkpkg_tokenizer_vm_begin_status_buffer_v1
        MOVEQ #5, D0
        BSR.W tkpkg_tokenizer_vm_get_diag_code_v1
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        LEA vmDiagBudgetExceededText, A1
        MOVEQ #VM_DIAG_BUDGET_EXCEEDED_LEN, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        BRA.W tkpkg_tokenizer_vm_finish_status_buffer_v1

tkpkgTokenizerStatusInvalidArgument:
        LEA invalidArgumentText, A1
        MOVEQ #INVALID_ARGUMENT_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkg_tokenizer_vm_begin_status_buffer_v1:
        MOVE.L (SP)+, D0
        SUBA.L #LOCAL_SIZE, SP
        MOVE.L D0, -(SP)
        LEA 4(SP), A4
        CLR.L LOCAL_OUTPUT_LEN(A4)
        CLR.L LOCAL_OUTPUT_OVERFLOW(A4)
        CLR.B lastErrorBuffer
        RTS

tkpkg_tokenizer_vm_finish_status_buffer_v1:
        TST.L LOCAL_OUTPUT_OVERFLOW(A4)
        BNE.S tkpkgTokenizerFinishStatusOverflow
        LEA lastErrorBuffer, A1
        MOVE.L LOCAL_OUTPUT_LEN(A4), D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        ADDA.L #LOCAL_SIZE, SP
        RTS

tkpkgTokenizerFinishStatusOverflow:
        LEA outputOverflowText, A1
        MOVEQ #OUTPUT_OVERFLOW_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        ADDA.L #LOCAL_SIZE, SP
        RTS

tkpkg_tokenizer_vm_get_diag_code_v1:
        CMPI.B #1, D0
        BEQ.S tkpkgTokenizerDiagUnterminatedString
        CMPI.B #2, D0
        BEQ.S tkpkgTokenizerDiagStepLimit
        CMPI.B #3, D0
        BEQ.S tkpkgTokenizerDiagTokenLimit
        CMPI.B #4, D0
        BEQ.S tkpkgTokenizerDiagLexemeLimit
        CMPI.B #5, D0
        BEQ.S tkpkgTokenizerDiagErrorLimit
        LEA activeTokenizerVmInvalidCharDiagCode, A1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmInvalidCharDiagLen, D2
        RTS

tkpkgTokenizerDiagUnterminatedString:
        LEA activeTokenizerVmUnterminatedStringDiagCode, A1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmUnterminatedStringDiagLen, D2
        RTS

tkpkgTokenizerDiagStepLimit:
        LEA activeTokenizerVmStepLimitDiagCode, A1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmStepLimitDiagLen, D2
        RTS

tkpkgTokenizerDiagTokenLimit:
        LEA activeTokenizerVmTokenLimitDiagCode, A1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmTokenLimitDiagLen, D2
        RTS

tkpkgTokenizerDiagLexemeLimit:
        LEA activeTokenizerVmLexemeLimitDiagCode, A1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmLexemeLimitDiagLen, D2
        RTS

tkpkgTokenizerDiagErrorLimit:
        LEA activeTokenizerVmErrorLimitDiagCode, A1
        MOVEQ #0, D2
        MOVE.B activeTokenizerVmErrorLimitDiagLen, D2
        RTS

tkpkg_tokenizer_vm_validate_result_v1:
        MOVEM.L D1-D7/A0, -(SP)
        CMP.L #TOKEN_BUFFER_CAPACITY, D1
        BHI.S tkpkgTokenizerValidateInvalid
        CMP.L D4, D2
        BHI.S tkpkgTokenizerValidateInvalid
        CMP.L #TOKEN_SCRATCH_CAPACITY, D3
        BHI.S tkpkgTokenizerValidateInvalid
        LEA tokenRecordBuffer, A0
        MOVEQ #0, D5

tkpkgTokenizerValidateLoop:
        CMP.L D1, D5
        BCC.S tkpkgTokenizerValidateOk
        MOVEQ #0, D0
        MOVE.W (A0), D0
        CMPI.L #TK_KIND_OP_LT, D0
        BGT.S tkpkgTokenizerValidateInvalid
        MOVE.L 4(A0), D6
        TST.L D6
        BEQ.S tkpkgTokenizerValidateInvalid
        MOVE.L D4, D7
        ADDQ.L #1, D7
        CMP.L D7, D6
        BHI.S tkpkgTokenizerValidateInvalid
        MOVE.L 8(A0), D6
        TST.L D6
        BEQ.S tkpkgTokenizerValidateInvalid
        CMP.L D7, D6
        BHI.S tkpkgTokenizerValidateInvalid
        CMP.L 4(A0), D6
        BLT.S tkpkgTokenizerValidateInvalid
        MOVE.L 12(A0), D6
        CMP.L D3, D6
        BHI.S tkpkgTokenizerValidateInvalid
        MOVE.L 16(A0), D7
        ADD.L D6, D7
        CMP.L D3, D7
        BHI.S tkpkgTokenizerValidateInvalid
        ADDA.L #TOKEN_RECORD_SIZE, A0
        ADDQ.L #1, D5
        BRA.S tkpkgTokenizerValidateLoop

tkpkgTokenizerValidateOk:
        MOVEQ #0, D0
        MOVEM.L (SP)+, D1-D7/A0
        RTS

tkpkgTokenizerValidateInvalid:
        MOVEQ #1, D0
        MOVEM.L (SP)+, D1-D7/A0
        RTS

tkpkg_tokenizer_vm_render_output_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        SUBA.L #LOCAL_SIZE, SP
        LEA 0(SP), A4
        MOVE.L D1, D7
        CLR.L LOCAL_OUTPUT_LEN(A4)
        CLR.L LOCAL_OUTPUT_OVERFLOW(A4)
        MOVE.L D6, LOCAL_RENDER_LINE(A4)
        CLR.B lastErrorBuffer
        MOVEQ #0, D6

tkpkgTokenizerRenderLoop:
        CMP.L D7, D6
        BCC.S tkpkgTokenizerRenderDone
        MOVE.L D6, D0
        BSR.W tkpkg_tokenizer_vm_record_ptr_v1
        MOVEA.L A0, A5
        MOVEQ #0, D0
        MOVE.W (A5), D0
        MOVE.L 12(A5), D2
        MOVE.L 16(A5), D3
        LEA tokenScratchBuffer, A6
        ADDA.L D2, A6
        BSR.W tkpkg_tokenizer_vm_append_kind_debug_v1
        BSR.W tkpkg_tokenizer_vm_append_literal_at_v1
        MOVE.L LOCAL_RENDER_LINE(A4), D0
        BSR.W tkpkg_tokenizer_vm_append_u32_v1
        BSR.W tkpkg_tokenizer_vm_append_literal_colon_v1
        MOVE.L 4(A5), D0
        BSR.W tkpkg_tokenizer_vm_append_u32_v1
        BSR.W tkpkg_tokenizer_vm_append_literal_dash_v1
        MOVE.L 8(A5), D0
        BSR.W tkpkg_tokenizer_vm_append_u32_v1
        BSR.W tkpkg_tokenizer_vm_append_literal_newline_v1
        TST.L LOCAL_OUTPUT_OVERFLOW(A4)
        BNE.S tkpkgTokenizerRenderOverflow
        ADDQ.L #1, D6
        BRA.S tkpkgTokenizerRenderLoop

tkpkgTokenizerRenderDone:
        MOVE.L LOCAL_OUTPUT_LEN(A4), D1
        MOVEQ #0, D0
        ADDA.L #LOCAL_SIZE, SP
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkgTokenizerRenderOverflow:
        LEA outputOverflowText, A1
        MOVEQ #OUTPUT_OVERFLOW_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        ADDA.L #LOCAL_SIZE, SP
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_tokenizer_vm_append_kind_debug_v1:
        CMPI.W #TK_KIND_IDENTIFIER, D0
        BEQ.W tkpkgTokenizerAppendIdentifier
        CMPI.W #TK_KIND_NUMBER, D0
        BEQ.W tkpkgTokenizerAppendNumber
        CMPI.W #TK_KIND_STRING, D0
        BEQ.W tkpkgTokenizerAppendString
        CMPI.W #TK_KIND_COMMA, D0
        BEQ.W tkpkgTokenizerAppendBareComma
        CMPI.W #TK_KIND_COLON, D0
        BEQ.W tkpkgTokenizerAppendBareColon
        CMPI.W #TK_KIND_DOLLAR, D0
        BEQ.W tkpkgTokenizerAppendBareDollar
        CMPI.W #TK_KIND_DOT, D0
        BEQ.W tkpkgTokenizerAppendBareDot
        CMPI.W #TK_KIND_HASH, D0
        BEQ.W tkpkgTokenizerAppendBareHash
        CMPI.W #TK_KIND_QUESTION, D0
        BEQ.W tkpkgTokenizerAppendBareQuestion
        CMPI.W #TK_KIND_OPEN_BRACKET, D0
        BEQ.W tkpkgTokenizerAppendBareOpenBracket
        CMPI.W #TK_KIND_CLOSE_BRACKET, D0
        BEQ.W tkpkgTokenizerAppendBareCloseBracket
        CMPI.W #TK_KIND_OPEN_BRACE, D0
        BEQ.W tkpkgTokenizerAppendBareOpenBrace
        CMPI.W #TK_KIND_CLOSE_BRACE, D0
        BEQ.W tkpkgTokenizerAppendBareCloseBrace
        CMPI.W #TK_KIND_OPEN_PAREN, D0
        BEQ.W tkpkgTokenizerAppendBareOpenParen
        CMPI.W #TK_KIND_CLOSE_PAREN, D0
        BEQ.W tkpkgTokenizerAppendBareCloseParen
        BRA.W tkpkgTokenizerAppendOperator

tkpkgTokenizerAppendIdentifier:
        LEA identifierPrefix, A1
        MOVEQ #11, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVEA.L A6, A1
        BSR.W tkpkg_tokenizer_vm_append_quoted_v1
        BRA.W tkpkg_tokenizer_vm_append_literal_close_paren_v1

tkpkgTokenizerAppendNumber:
        LEA numberPrefix, A1
        MOVEQ #15, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVEA.L A6, A1
        BSR.W tkpkg_tokenizer_vm_append_upper_quoted_v1
        LEA numberBasePrefix, A1
        MOVEQ #8, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVEQ #10, D0
        TST.L D3
        BEQ.S tkpkgTokenizerAppendNumberBaseDone
        MOVEQ #0, D1
        MOVE.B (A6), D1
        CMPI.B #'$', D1
        BEQ.S tkpkgTokenizerAppendNumberHex
        CMPI.B #'%', D1
        BEQ.S tkpkgTokenizerAppendNumberBin
        MOVEQ #0, D2
        MOVEA.L A6, A1
        ADDA.L D3, A1
        SUBQ.L #1, A1
        MOVE.B (A1), D2
        CMPI.B #'a', D2
        BLO.S tkpkgTokenizerAppendNumberSuffix
        CMPI.B #'z', D2
        BHI.S tkpkgTokenizerAppendNumberSuffix
        ANDI.B #$DF, D2
tkpkgTokenizerAppendNumberSuffix:
        CMPI.B #'H', D2
        BEQ.S tkpkgTokenizerAppendNumberHex
        CMPI.B #'B', D2
        BEQ.S tkpkgTokenizerAppendNumberBin
        CMPI.B #'O', D2
        BEQ.S tkpkgTokenizerAppendNumberOct
        CMPI.B #'Q', D2
        BNE.S tkpkgTokenizerAppendNumberBaseDone
tkpkgTokenizerAppendNumberOct:
        MOVEQ #8, D0
        BRA.S tkpkgTokenizerAppendNumberBaseDone
tkpkgTokenizerAppendNumberHex:
        MOVEQ #16, D0
        BRA.S tkpkgTokenizerAppendNumberBaseDone
tkpkgTokenizerAppendNumberBin:
        MOVEQ #2, D0
tkpkgTokenizerAppendNumberBaseDone:
        BSR.W tkpkg_tokenizer_vm_append_u32_v1
        LEA stringSuffix, A1
        ADDQ.L #1, A1
        MOVEQ #2, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        RTS

tkpkgTokenizerAppendString:
        LEA stringPrefix, A1
        MOVEQ #14, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVEA.L A6, A1
        BSR.W tkpkg_tokenizer_vm_append_string_raw_v1
        LEA stringBytesPrefix, A1
        MOVEQ #10, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVEA.L A6, A1
        BSR.W tkpkg_tokenizer_vm_append_byte_list_v1
        LEA stringSuffix, A1
        MOVEQ #3, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        RTS

tkpkgTokenizerAppendBareComma:
        LEA kindCommaText, A1
        MOVEQ #5, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareColon:
        LEA kindColonText, A1
        MOVEQ #5, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareDollar:
        LEA kindDollarText, A1
        MOVEQ #6, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareDot:
        LEA kindDotText, A1
        MOVEQ #3, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareHash:
        LEA kindHashText, A1
        MOVEQ #4, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareQuestion:
        LEA kindQuestionText, A1
        MOVEQ #8, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareOpenBracket:
        LEA kindOpenBracketText, A1
        MOVEQ #11, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareCloseBracket:
        LEA kindCloseBracketText, A1
        MOVEQ #12, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareOpenBrace:
        LEA kindOpenBraceText, A1
        MOVEQ #9, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareCloseBrace:
        LEA kindCloseBraceText, A1
        MOVEQ #10, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareOpenParen:
        LEA kindOpenParenText, A1
        MOVEQ #9, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerAppendBareCloseParen:
        LEA kindCloseParenText, A1
        MOVEQ #10, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1

tkpkgTokenizerAppendOperator:
        MOVE.L D0, -(SP)
        LEA operatorPrefix, A1
        MOVEQ #9, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVE.L (SP)+, D0
        BSR.W tkpkg_tokenizer_vm_append_operator_name_v1
        BRA.W tkpkg_tokenizer_vm_append_literal_close_paren_v1

tkpkg_tokenizer_vm_append_operator_name_v1:
        CMPI.W #TK_KIND_OP_RANGE, D0
        BEQ.W tkpkgTokenizerOpRange
        CMPI.W #TK_KIND_OP_RANGE_INCLUSIVE, D0
        BEQ.W tkpkgTokenizerOpRangeInclusive
        CMPI.W #TK_KIND_OP_PLUS, D0
        BEQ.W tkpkgTokenizerOpPlus
        CMPI.W #TK_KIND_OP_MINUS, D0
        BEQ.W tkpkgTokenizerOpMinus
        CMPI.W #TK_KIND_OP_MULTIPLY, D0
        BEQ.W tkpkgTokenizerOpMultiply
        CMPI.W #TK_KIND_OP_POWER, D0
        BEQ.W tkpkgTokenizerOpPower
        CMPI.W #TK_KIND_OP_DIVIDE, D0
        BEQ.W tkpkgTokenizerOpDivide
        CMPI.W #TK_KIND_OP_MOD, D0
        BEQ.W tkpkgTokenizerOpMod
        CMPI.W #TK_KIND_OP_SHL, D0
        BEQ.W tkpkgTokenizerOpShl
        CMPI.W #TK_KIND_OP_SHR, D0
        BEQ.W tkpkgTokenizerOpShr
        CMPI.W #TK_KIND_OP_BIT_NOT, D0
        BEQ.W tkpkgTokenizerOpBitNot
        CMPI.W #TK_KIND_OP_LOGIC_NOT, D0
        BEQ.W tkpkgTokenizerOpLogicNot
        CMPI.W #TK_KIND_OP_BIT_AND, D0
        BEQ.W tkpkgTokenizerOpBitAnd
        CMPI.W #TK_KIND_OP_BIT_OR, D0
        BEQ.W tkpkgTokenizerOpBitOr
        CMPI.W #TK_KIND_OP_BIT_XOR, D0
        BEQ.W tkpkgTokenizerOpBitXor
        CMPI.W #TK_KIND_OP_LOGIC_AND, D0
        BEQ.W tkpkgTokenizerOpLogicAnd
        CMPI.W #TK_KIND_OP_LOGIC_OR, D0
        BEQ.W tkpkgTokenizerOpLogicOr
        CMPI.W #TK_KIND_OP_LOGIC_XOR, D0
        BEQ.W tkpkgTokenizerOpLogicXor
        CMPI.W #TK_KIND_OP_EQ, D0
        BEQ.W tkpkgTokenizerOpEq
        CMPI.W #TK_KIND_OP_NE, D0
        BEQ.W tkpkgTokenizerOpNe
        CMPI.W #TK_KIND_OP_GE, D0
        BEQ.W tkpkgTokenizerOpGe
        CMPI.W #TK_KIND_OP_GT, D0
        BEQ.W tkpkgTokenizerOpGt
        CMPI.W #TK_KIND_OP_LE, D0
        BEQ.W tkpkgTokenizerOpLe
        LEA opLtText, A1
        MOVEQ #2, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpRange:
        LEA opRangeText, A1
        MOVEQ #5, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpRangeInclusive:
        LEA opRangeInclusiveText, A1
        MOVEQ #14, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpPlus:
        LEA opPlusText, A1
        MOVEQ #4, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpMinus:
        LEA opMinusText, A1
        MOVEQ #5, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpMultiply:
        LEA opMultiplyText, A1
        MOVEQ #8, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpPower:
        LEA opPowerText, A1
        MOVEQ #5, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpDivide:
        LEA opDivideText, A1
        MOVEQ #6, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpMod:
        LEA opModText, A1
        MOVEQ #3, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpShl:
        LEA opShlText, A1
        MOVEQ #3, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpShr:
        LEA opShrText, A1
        MOVEQ #3, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpBitNot:
        LEA opBitNotText, A1
        MOVEQ #6, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpLogicNot:
        LEA opLogicNotText, A1
        MOVEQ #8, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpBitAnd:
        LEA opBitAndText, A1
        MOVEQ #6, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpBitOr:
        LEA opBitOrText, A1
        MOVEQ #5, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpBitXor:
        LEA opBitXorText, A1
        MOVEQ #6, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpLogicAnd:
        LEA opLogicAndText, A1
        MOVEQ #8, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpLogicOr:
        LEA opLogicOrText, A1
        MOVEQ #7, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpLogicXor:
        LEA opLogicXorText, A1
        MOVEQ #8, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpEq:
        LEA opEqText, A1
        MOVEQ #2, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpNe:
        LEA opNeText, A1
        MOVEQ #2, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpGe:
        LEA opGeText, A1
        MOVEQ #2, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpGt:
        LEA opGtText, A1
        MOVEQ #2, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1
tkpkgTokenizerOpLe:
        LEA opLeText, A1
        MOVEQ #2, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1

tkpkg_tokenizer_vm_append_quoted_v1:
        MOVEM.L D0-D1/D5, -(SP)
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVE.L D3, D5
        BEQ.S tkpkgTokenizerQuotedClose

tkpkgTokenizerQuotedLoop:
        MOVEQ #0, D0
        MOVE.B (A1)+, D0
        BSR.W tkpkg_tokenizer_vm_append_escaped_char_v1
        SUBQ.L #1, D5
        BNE.S tkpkgTokenizerQuotedLoop

tkpkgTokenizerQuotedClose:
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEM.L (SP)+, D0-D1/D5
        RTS

tkpkg_tokenizer_vm_append_upper_quoted_v1:
        MOVEM.L D0-D1/D5, -(SP)
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVE.L D3, D5
        BEQ.S tkpkgTokenizerUpperQuotedClose

tkpkgTokenizerUpperQuotedLoop:
        MOVEQ #0, D0
        MOVE.B (A1)+, D0
        CMPI.B #'a', D0
        BLO.S tkpkgTokenizerUpperQuotedEmit
        CMPI.B #'z', D0
        BHI.S tkpkgTokenizerUpperQuotedEmit
        ANDI.B #$DF, D0
tkpkgTokenizerUpperQuotedEmit:
        BSR.W tkpkg_tokenizer_vm_append_escaped_char_v1
        SUBQ.L #1, D5
        BNE.S tkpkgTokenizerUpperQuotedLoop

tkpkgTokenizerUpperQuotedClose:
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEM.L (SP)+, D0-D1/D5
        RTS

tkpkg_tokenizer_vm_append_string_raw_v1:
        MOVEM.L D0-D1/D5/A1, -(SP)
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_escaped_char_v1
        MOVE.L D3, D5
        BEQ.S tkpkgTokenizerStringRawClose

tkpkgTokenizerStringRawLoop:
        MOVEQ #0, D0
        MOVE.B (A1)+, D0
        BSR.W tkpkg_tokenizer_vm_append_escaped_char_v1
        SUBQ.L #1, D5
        BNE.S tkpkgTokenizerStringRawLoop

tkpkgTokenizerStringRawClose:
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_escaped_char_v1
        MOVEQ #'"', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEM.L (SP)+, D0-D1/D5/A1
        RTS

tkpkg_tokenizer_vm_append_byte_list_v1:
        MOVEM.L D0-D1/D5/A1, -(SP)
        MOVE.L D3, D5
        BEQ.S tkpkgTokenizerByteListDone

tkpkgTokenizerByteListLoop:
        MOVEQ #0, D0
        MOVE.B (A1)+, D0
        BSR.W tkpkg_tokenizer_vm_append_u32_v1
        SUBQ.L #1, D5
        BEQ.S tkpkgTokenizerByteListDone
        MOVE.L A1, -(SP)
        LEA commaSpaceText, A1
        MOVEQ #2, D2
        BSR.W tkpkg_tokenizer_vm_append_bytes_v1
        MOVEA.L (SP)+, A1
        BRA.S tkpkgTokenizerByteListLoop

tkpkgTokenizerByteListDone:
        MOVEM.L (SP)+, D0-D1/D5/A1
        RTS

tkpkg_tokenizer_vm_append_escaped_char_v1:
        CMPI.B #'\\', D0
        BEQ.S tkpkgTokenizerEscapeBackslash
        CMPI.B #'"', D0
        BEQ.S tkpkgTokenizerEscapeQuote
        CMPI.B #10, D0
        BEQ.S tkpkgTokenizerEscapeLf
        CMPI.B #13, D0
        BEQ.S tkpkgTokenizerEscapeCr
        CMPI.B #9, D0
        BEQ.S tkpkgTokenizerEscapeTab
        BRA.W tkpkg_tokenizer_vm_append_char_v1

tkpkgTokenizerEscapeBackslash:
        MOVEQ #'\\', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEQ #'\\', D0
        BRA.W tkpkg_tokenizer_vm_append_char_v1

tkpkgTokenizerEscapeQuote:
        MOVEQ #'\\', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEQ #'"', D0
        BRA.W tkpkg_tokenizer_vm_append_char_v1

tkpkgTokenizerEscapeLf:
        MOVEQ #'\\', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEQ #'n', D0
        BRA.W tkpkg_tokenizer_vm_append_char_v1

tkpkgTokenizerEscapeCr:
        MOVEQ #'\\', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEQ #'r', D0
        BRA.W tkpkg_tokenizer_vm_append_char_v1

tkpkgTokenizerEscapeTab:
        MOVEQ #'\\', D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVEQ #'t', D0
        BRA.W tkpkg_tokenizer_vm_append_char_v1

tkpkg_tokenizer_vm_append_u32_v1:
        MOVEM.L D1-D5/A1, -(SP)
        LEA decimalPowers, A1
        CLR.L D4
        MOVEQ #9, D5

tkpkgTokenizerAppendU32Loop:
        MOVE.L (A1)+, D2
        MOVEQ #0, D3

tkpkgTokenizerAppendDigitCount:
        CMP.L D2, D0
        BLO.S tkpkgTokenizerAppendDigitReady
        SUB.L D2, D0
        ADDQ.B #1, D3
        BRA.S tkpkgTokenizerAppendDigitCount

tkpkgTokenizerAppendDigitReady:
        TST.L D4
        BNE.S tkpkgTokenizerAppendDigitEmit
        TST.B D3
        BNE.S tkpkgTokenizerAppendDigitStart
        TST.W D5
        BNE.S tkpkgTokenizerAppendDigitSkip

tkpkgTokenizerAppendDigitStart:
        MOVEQ #1, D4

tkpkgTokenizerAppendDigitEmit:
        MOVE.L D0, -(SP)
        MOVEQ #'0', D4
        ADD.B D3, D4
        MOVE.L D4, D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        MOVE.L (SP)+, D0

tkpkgTokenizerAppendDigitSkip:
        DBF D5, tkpkgTokenizerAppendU32Loop
        MOVEM.L (SP)+, D1-D5/A1
        RTS

tkpkg_tokenizer_vm_append_bytes_v1:
        TST.W D2
        BEQ.S tkpkgTokenizerAppendBytesDone

tkpkgTokenizerAppendBytesLoop:
        MOVEQ #0, D0
        MOVE.B (A1)+, D0
        BSR.W tkpkg_tokenizer_vm_append_char_v1
        SUBQ.W #1, D2
        BNE.S tkpkgTokenizerAppendBytesLoop

tkpkgTokenizerAppendBytesDone:
        RTS

tkpkg_tokenizer_vm_append_char_v1:
        MOVE.L A1, -(SP)
        MOVE.L LOCAL_OUTPUT_LEN(A4), D1
        CMPI.L #LAST_ERROR_BUFFER_CAPACITY - 1, D1
        BCS.S tkpkgTokenizerAppendCharStore
        MOVEQ #1, D1
        MOVE.L D1, LOCAL_OUTPUT_OVERFLOW(A4)
        MOVEA.L (SP)+, A1
        RTS

tkpkgTokenizerAppendCharStore:
        LEA lastErrorBuffer, A1
        MOVE.B D0, 0(A1, D1.L)
        ADDQ.L #1, D1
        MOVE.L D1, LOCAL_OUTPUT_LEN(A4)
        CLR.B 0(A1, D1.L)
        MOVEA.L (SP)+, A1
        RTS

tkpkg_tokenizer_vm_number_base_v1:
        MOVEQ #10, D0
        TST.L D3
        BEQ.S tkpkgTokenizerNumberBaseDone
        MOVEQ #0, D1
        MOVE.B (A1), D1
        CMPI.B #'$', D1
        BEQ.S tkpkgTokenizerNumberBaseHex
        CMPI.B #'%', D1
        BEQ.S tkpkgTokenizerNumberBaseBin
        MOVEQ #0, D2
        MOVEA.L A1, A0
        ADDA.L D3, A0
        SUBQ.L #1, A0
        MOVE.B (A0), D2
        CMPI.B #'a', D2
        BLO.S tkpkgTokenizerNumberBaseSuffix
        CMPI.B #'z', D2
        BHI.S tkpkgTokenizerNumberBaseSuffix
        ANDI.B #$DF, D2

tkpkgTokenizerNumberBaseSuffix:
        CMPI.B #'H', D2
        BEQ.S tkpkgTokenizerNumberBaseHex
        CMPI.B #'B', D2
        BEQ.S tkpkgTokenizerNumberBaseBin
        CMPI.B #'O', D2
        BEQ.S tkpkgTokenizerNumberBaseOct
        CMPI.B #'Q', D2
        BEQ.S tkpkgTokenizerNumberBaseOct
        BRA.S tkpkgTokenizerNumberBaseDone

tkpkgTokenizerNumberBaseHex:
        MOVEQ #16, D0
        RTS
tkpkgTokenizerNumberBaseBin:
        MOVEQ #2, D0
        RTS
tkpkgTokenizerNumberBaseOct:
        MOVEQ #8, D0
tkpkgTokenizerNumberBaseDone:
        RTS

tkpkg_tokenizer_vm_record_ptr_v1:
        MOVE.L D0, D1
        ADD.L D1, D1
        MOVEA.L D1, A0
        ADD.L D1, D1
        ADD.L D1, D1
        ADD.L D1, D1
        ADDA.L D1, A0
        MOVE.L D0, D1
        ADD.L D1, D1
        ADDA.L D1, A0
        LEA tokenRecordBuffer, A1
        ADDA.L A0, A1
        MOVEA.L A1, A0
        RTS

tkpkg_tokenizer_vm_append_literal_at_v1:
        LEA atSep, A1
        MOVEQ #1, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1

tkpkg_tokenizer_vm_append_literal_colon_v1:
        LEA colonSep, A1
        MOVEQ #1, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1

tkpkg_tokenizer_vm_append_literal_dash_v1:
        LEA dashSep, A1
        MOVEQ #1, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1

tkpkg_tokenizer_vm_append_literal_newline_v1:
        LEA newlineSep, A1
        MOVEQ #1, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1

tkpkg_tokenizer_vm_append_literal_close_paren_v1:
        LEA closeParenText, A1
        MOVEQ #1, D2
        BRA.W tkpkg_tokenizer_vm_append_bytes_v1

tkpkg_tokenizer_vm_skip_string_v1:
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.S tkpkgTokenizerSkipStringDone
        BSR.W tkpkg_tokenizer_vm_require_bytes_v1
        TST.B D1
        BNE.S tkpkgTokenizerSkipStringDone
        ADDA.L D0, A2
tkpkgTokenizerSkipStringDone:
        RTS

tkpkg_tokenizer_vm_read_string_into_slot_v1:
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.S tkpkgTokenizerReadStringDone
        BSR.W tkpkg_tokenizer_vm_require_bytes_v1
        TST.B D1
        BNE.S tkpkgTokenizerReadStringDone
        MOVE.L D0, D2
        CMPI.L #31, D2
        BLS.S tkpkgTokenizerReadStringLenReady
        MOVEQ #31, D2
tkpkgTokenizerReadStringLenReady:
        MOVE.B D2, (A1)
        MOVE.L D2, D1
        MOVEA.L A2, A0
tkpkgTokenizerReadStringCopyLoop:
        TST.L D1
        BEQ.S tkpkgTokenizerReadStringCopyDone
        MOVE.B (A0)+, (A3)+
        SUBQ.L #1, D1
        BRA.S tkpkgTokenizerReadStringCopyLoop
tkpkgTokenizerReadStringCopyDone:
        CLR.B (A3)
        ADDA.L D0, A2
        MOVEQ #0, D1
tkpkgTokenizerReadStringDone:
        RTS

tkpkg_tokenizer_vm_read_bytes_field_v1:
        BSR.W tkpkg_tokenizer_vm_read_u32_le_v1
        TST.B D1
        BNE.S tkpkgTokenizerReadBytesDone
        BSR.W tkpkg_tokenizer_vm_require_bytes_v1
        TST.B D1
        BNE.S tkpkgTokenizerReadBytesDone
        MOVEA.L A2, A3
        MOVE.L D0, D3
        ADDA.L D0, A2
        MOVEQ #0, D1
tkpkgTokenizerReadBytesDone:
        RTS

tkpkg_tokenizer_vm_read_u16_le_v1:
        MOVEQ #2, D0
        BSR.W tkpkg_tokenizer_vm_require_bytes_v1
        TST.B D1
        BNE.S tkpkgTokenizerReadU16Done
        MOVEQ #0, D0
        MOVE.B (A2)+, D0
        MOVEQ #0, D1
        MOVE.B (A2)+, D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D1
tkpkgTokenizerReadU16Done:
        RTS

tkpkg_tokenizer_vm_read_u32_le_v1:
        MOVEQ #4, D0
        BSR.W tkpkg_tokenizer_vm_require_bytes_v1
        TST.B D1
        BNE.S tkpkgTokenizerReadU32Done
        MOVEQ #0, D0
        MOVE.B (A2)+, D0
        MOVEQ #0, D1
        MOVE.B (A2)+, D1
        LSL.L #8, D1
        OR.L D1, D0
        MOVEQ #0, D1
        MOVE.B (A2)+, D1
        LSL.L #8, D1
        LSL.L #8, D1
        OR.L D1, D0
        MOVEQ #0, D1
        MOVE.B (A2)+, D1
        LSL.L #8, D1
        LSL.L #8, D1
        LSL.L #8, D1
        OR.L D1, D0
        MOVEQ #0, D1
tkpkgTokenizerReadU32Done:
        RTS

tkpkg_tokenizer_vm_require_bytes_v1:
        CMPA.L A6, A2
        BHI.S tkpkgTokenizerRequireBytesFail
        MOVE.L A6, D1
        SUB.L A2, D1
        CMP.L D1, D0
        BHI.S tkpkgTokenizerRequireBytesFail
        MOVEQ #0, D1
        RTS

tkpkgTokenizerRequireBytesFail:
        MOVEQ #1, D1
        RTS

        .endsection
        .endmodule
