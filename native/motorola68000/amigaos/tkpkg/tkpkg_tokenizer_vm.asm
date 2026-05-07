; Package-backed tokenizer-VM wrapper for the first tkpkg tokenize_line slice.

	.module tkpkg.amigaos.tokenizer_vm
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN)
	.use tkpkg.amigaos.abi (STATUS_BAD_REQUEST_V1, STATUS_RUNTIME_ERROR_V1)
	.use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_CAPACITY, PACKAGE_STATE_PIPELINE_ACTIVE)
	.use tkpkg.amigaos.buffers (TOKEN_BUFFER_CAPACITY, TOKEN_RECORD_SIZE)
	.use tkpkg.amigaos.buffers (TOKEN_SCRATCH_CAPACITY, TOKENIZER_VM_STATE_TABLE_CAPACITY)
	.use tkpkg.amigaos.buffers (PackageStateFlags, PackageStorage, LastErrorBuffer)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmOffsetLo)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmStartStateLo, ActiveTokenizerVmStartStateHi)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmStateCountLo, ActiveTokenizerVmStateCountHi)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmStateTable)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmMaxErrorsPerLine)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmInvalidCharDiagLen, ActiveTokenizerVmInvalidCharDiagCode)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmUnterminatedStringDiagLen, ActiveTokenizerVmUnterminatedStringDiagCode)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmStepLimitDiagLen, ActiveTokenizerVmStepLimitDiagCode)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmTokenLimitDiagLen, ActiveTokenizerVmTokenLimitDiagCode)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmLexemeLimitDiagLen, ActiveTokenizerVmLexemeLimitDiagCode)
	.use tkpkg.amigaos.buffers (ActiveTokenizerVmErrorLimitDiagLen, ActiveTokenizerVmErrorLimitDiagCode)
	.use tkpkg.amigaos.buffers (TokenRecordBuffer, TokenScratchBuffer)
	.use tkpkg.amigaos.buffers (LastTokenCount, LastLexemeLen)
	.use tokvm.amigaos.tokenizer_vm (tokvmRun68000, tokvmSetStepBudget68000)
	.use tokvm.amigaos.tokenizer_vm (tokvmSetProgramStateTable68000)
	.use tokvm.amigaos.tokenizer_vm (tokvmReadLastFailure68000)
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

NoPipelineText
	.byte "OTR001: tokenize_line requires set_pipeline", 0

BadPayloadText
	.byte "OTR002: tokenize_line requires a 4-byte line prefix", 0

NewlineText
	.byte "OTR004: tokenize_line rejects newline bytes", 0

StepLimitText
	.byte "OTR901: tokenizer step budget exceeded", 0

TokenOverflowText
	.byte "OTR901: tokenizer token overflow", 0

LexemeOverflowText
	.byte "OTR901: tokenizer lexeme overflow", 0

InvalidProgramText
	.byte "OTR901: invalid tokenizer VM program", 0

BadProgramHeaderText
	.byte "OTR901: bad tokenizer VM header bytes", 0

InvalidArgumentText
	.byte "OTR901: invalid tokenize_line arguments", 0

OutputOverflowText
	.byte "OTR901: tokenizer output overflow", 0

StepLimitSuffixText
	.byte ": tokenizer step budget exceeded"

TokenOverflowSuffixText
	.byte ": tokenizer token overflow"

LexemeOverflowSuffixText
	.byte ": tokenizer lexeme overflow"

VmFailureReasonSuffixText
	.byte ": tokenizer VM failure reason "

VmEmitDiagSuffixText
	.byte ": tokenizer VM emitted diagnostic slot "

VmDiagBudgetExceededText
	.byte ": tokenizer VM diagnostic budget exceeded (1/0)"

VmFailureFallbackText
	.byte "OTR901: tokenizer VM failure", 0

IdentifierPrefix
	.byte "Identifier("

NumberPrefix
	.byte "Number { text: "

NumberBasePrefix
	.byte ", base: "

StringPrefix
	.byte "String { raw: "

StringBytesPrefix
	.byte ", bytes: ["

StringSuffix
	.byte "] }"

OperatorPrefix
	.byte "Operator("

AtSep
	.byte "@"

ColonSep
	.byte ":"

DashSep
	.byte "-"

NewlineSep
	.byte 10

CloseParenText
	.byte ")"

CommaSpaceText
	.byte ", "

CommaText
	.byte ","

HexDigits
	.byte "0123456789ABCDEF"

DecimalPowers
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

KindCommaText
	.byte "Comma"
KindColonText
	.byte "Colon"
KindDollarText
	.byte "Dollar"
KindDotText
	.byte "Dot"
KindHashText
	.byte "Hash"
KindQuestionText
	.byte "Question"
KindOpenBracketText
	.byte "OpenBracket"
KindCloseBracketText
	.byte "CloseBracket"
KindOpenBraceText
	.byte "OpenBrace"
KindCloseBraceText
	.byte "CloseBrace"
KindOpenParenText
	.byte "OpenParen"
KindCloseParenText
	.byte "CloseParen"

OpRangeText
	.byte "Range"
OpRangeInclusiveText
	.byte "RangeInclusive"
OpPlusText
	.byte "Plus"
OpMinusText
	.byte "Minus"
OpMultiplyText
	.byte "Multiply"
OpPowerText
	.byte "Power"
OpDivideText
	.byte "Divide"
OpModText
	.byte "Mod"
OpShlText
	.byte "Shl"
OpShrText
	.byte "Shr"
OpBitNotText
	.byte "BitNot"
OpLogicNotText
	.byte "LogicNot"
OpBitAndText
	.byte "BitAnd"
OpBitOrText
	.byte "BitOr"
OpBitXorText
	.byte "BitXor"
OpLogicAndText
	.byte "LogicAnd"
OpLogicOrText
	.byte "LogicOr"
OpLogicXorText
	.byte "LogicXor"
OpEqText
	.byte "Eq"
OpNeText
	.byte "Ne"
OpGeText
	.byte "Ge"
OpGtText
	.byte "Gt"
OpLeText
	.byte "Le"
OpLtText
	.byte "Lt"

	.endsection

	.section code, kind=code

; ---------------------------------------------------------------------------
; Tokenize one source line through the active package-backed tokenizer VM.
;
; This entry bridges a selected tkpkg runtime pipeline to the lower-level tokvm
; interpreter. It reads the line payload from the control block, decodes the
; active TKVM package record, executes tokvm, then renders compact token records
; into the service output buffer.
;
; Inputs:
; - A0: validated tkpkg control block whose input window contains
;   `<u32 line-number-le><source-bytes>`.
; - activeTokenizerVm* fields identify the selected package TKVM program.
;
; Outputs:
; - D0: 0 on success, STATUS_BAD_REQUEST_V1 or STATUS_RUNTIME_ERROR_V1 on
;   failure.
; - D1: rendered output byte length on success.
; - lastTokenCount/lastLexemeLen and token buffers are updated on success.
; ---------------------------------------------------------------------------

tkpkgTokenizerVmTokenizeLineV1
	movem.l d2-d7/a2-a6, -(sp)
	btst #1, PackageStateFlags  ; require set_pipeline before executing any package VM program
	bne.s tkpkgTokenizerPipelineReady
	lea NoPipelineText, a1
	moveq #NO_PIPELINE_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	bra.w tkpkgTokenizerDone

tkpkgTokenizerPipelineReady
	bsr.w tkpkgTokenizerVmReadLinePayloadV1
	tst.b d0
	bne.w tkpkgTokenizerDone
	bsr.w tkpkgTokenizerVmReadProgramV1
	tst.b d0
	bne.w tkpkgTokenizerDone
	move.l #TOKVM_DEFAULT_MAX_STEPS_PER_LINE, d0
	jsr tokvmSetStepBudget68000  ; keep tkpkg-driven tokenizer runs under the bounded VM budget
	movea.l a3, a5  ; A5 keeps program bytes while A3 is reused for tokvm call ABI
	move.l d3, d7  ; D7 keeps program length while record metadata is decoded
	cmpi.b #1, (a5)
	bne.w tkpkgTokenizerDebugBadProgramHeader
	cmpi.b #8, 1(a5)
	bne.w tkpkgTokenizerDebugBadProgramHeader
	move.l d6, -(sp)
	lea ActiveTokenizerVmStateTable, a0
	moveq #0, d0
	move.b ActiveTokenizerVmStateCountLo, d0
	moveq #0, d1
	move.b ActiveTokenizerVmStateCountHi, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	move.b ActiveTokenizerVmStartStateLo, d1
	moveq #0, d2
	move.b ActiveTokenizerVmStartStateHi, d2
	lsl.w #8, d2
	or.w d2, d1
	jsr tokvmSetProgramStateTable68000  ; install package state table into shared tokvm core
	movea.l a4, a0  ; tokvm input pointer: source bytes after line-number prefix
	move.l d4, d0  ; tokvm input length: source byte count
	lea TokenRecordBuffer, a1  ; tokvm output token records
	moveq #0, d1
	move.w #TOKEN_BUFFER_CAPACITY, d1
	lea TokenScratchBuffer, a2  ; lexeme scratch mirrors Rust portable-token lexeme storage
	moveq #0, d2
	move.w #TOKEN_SCRATCH_CAPACITY, d2
	movea.l a5, a3  ; tokvm program pointer
	move.l d7, d3  ; tokvm program length
	jsr tokvmRun68000
	move.l (sp)+, d6
	cmpi.b #TK_STATUS_SUCCESS, d0
	beq.s tkpkgTokenizerRender
	bsr.w tkpkgTokenizerVmStatusMessageV1
	bra.w tkpkgTokenizerDone

tkpkgTokenizerRender
	move.w d1, LastTokenCount
	move.w d3, LastLexemeLen
	bsr.w tkpkgTokenizerVmValidateResultV1
	tst.b d0
	bne.w tkpkgTokenizerInvalidProgram
	bsr.w tkpkgTokenizerVmRenderOutputV1

tkpkgTokenizerDone
	movem.l (sp)+, d2-d7/a2-a6
	rts

; Read the line-number-prefixed tokenizer service payload.
tkpkgTokenizerVmReadLinePayloadV1
	moveq #0, d0
	move.b CB_INPUT_LEN(a0), d0
	moveq #0, d1
	move.b 19(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #4, d0
	blo.s tkpkgTokenizerBadPayload
	move.w d0, d4
	subq.w #4, d4
	moveq #0, d0
	move.b CB_INPUT_PTR(a0), d0
	moveq #0, d1
	move.b 17(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	tst.w d0
	beq.s tkpkgTokenizerBadPayload
	lea 0(a0, d0.W), a4
	moveq #0, d6
	move.b (a4)+, d6
	moveq #0, d1
	move.b (a4)+, d1
	lsl.l #8, d1
	or.l d1, d6
	moveq #0, d1
	move.b (a4)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d6
	moveq #0, d1
	move.b (a4)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d6
	moveq #0, d0
	rts

tkpkgTokenizerBadPayload
	lea BadPayloadText, a1
	moveq #BAD_PAYLOAD_TEXT_LEN, d1
	moveq #STATUS_BAD_REQUEST_V1, d0
	rts

; Decode the active TKVM package record and expose program bytes/state table.
tkpkgTokenizerVmReadProgramV1
	lea ActiveTokenizerVmOffsetLo, a1
	moveq #0, d0
	move.b (a1)+, d0
	moveq #0, d1
	move.b (a1)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d1
	move.b (a1)+, d1
	lsl.w #8, d1
	or.w d1, d2
	tst.w d2
	beq.w tkpkgTokenizerInvalidProgram
	lea PackageStorage, a2
	lea 0(a2, d0.W), a2
	movea.l a2, a6
	adda.l d2, a6
	moveq #1, d0
	bsr.w tkpkgTokenizerVmRequireBytesV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	addq.w #1, a2
	bsr.w tkpkgTokenizerVmSkipStringV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	bsr.w tkpkgTokenizerVmReadU16LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	cmpi.w #TKVM_OPCODE_VERSION_V1, d0
	bne.w tkpkgTokenizerInvalidProgram
	bsr.w tkpkgTokenizerVmReadU16LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	move.b d0, ActiveTokenizerVmStartStateLo
	lsr.w #8, d0
	move.b d0, ActiveTokenizerVmStartStateHi
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	tst.l d0
	beq.w tkpkgTokenizerInvalidProgram
	cmpi.l #TOKENIZER_VM_STATE_TABLE_CAPACITY, d0
	bhi.w tkpkgTokenizerInvalidProgram
	move.b d0, ActiveTokenizerVmStateCountLo
	lsr.l #8, d0
	move.b d0, ActiveTokenizerVmStateCountHi
	moveq #0, d0
	move.b ActiveTokenizerVmStateCountLo, d0
	moveq #0, d1
	move.b ActiveTokenizerVmStateCountHi, d1
	lsl.w #8, d1
	or.w d1, d0
	move.w d0, d7
	lea ActiveTokenizerVmStateTable, a3
	subq.w #1, d7

tkpkgTokenizerSkipStateOffsets
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	move.l d0, (a3)+
	dbf d7, tkpkgTokenizerSkipStateOffsets
	bsr.w tkpkgTokenizerVmReadU16LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	cmpi.w #TKVM_STREAM_VERSION_V1, d0
	bne.w tkpkgTokenizerInvalidProgram
	moveq #1, d0
	bsr.w tkpkgTokenizerVmRequireBytesV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	moveq #0, d0
	move.b (a2)+, d0
	cmpi.b #TKVM_STREAM_MODE_LINE, d0
	bne.w tkpkgTokenizerInvalidProgram
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	move.l d0, d5
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	move.l d0, ActiveTokenizerVmMaxErrorsPerLine
	lea ActiveTokenizerVmInvalidCharDiagCode, a3
	lea ActiveTokenizerVmInvalidCharDiagLen, a1
	bsr.w tkpkgTokenizerVmReadStringIntoSlotV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	lea ActiveTokenizerVmUnterminatedStringDiagCode, a3
	lea ActiveTokenizerVmUnterminatedStringDiagLen, a1
	bsr.w tkpkgTokenizerVmReadStringIntoSlotV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	lea ActiveTokenizerVmStepLimitDiagCode, a3
	lea ActiveTokenizerVmStepLimitDiagLen, a1
	bsr.w tkpkgTokenizerVmReadStringIntoSlotV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	lea ActiveTokenizerVmTokenLimitDiagCode, a3
	lea ActiveTokenizerVmTokenLimitDiagLen, a1
	bsr.w tkpkgTokenizerVmReadStringIntoSlotV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	lea ActiveTokenizerVmLexemeLimitDiagCode, a3
	lea ActiveTokenizerVmLexemeLimitDiagLen, a1
	bsr.w tkpkgTokenizerVmReadStringIntoSlotV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	lea ActiveTokenizerVmErrorLimitDiagCode, a3
	lea ActiveTokenizerVmErrorLimitDiagLen, a1
	bsr.w tkpkgTokenizerVmReadStringIntoSlotV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	bsr.w tkpkgTokenizerVmReadBytesFieldV1
	tst.b d1
	bne.w tkpkgTokenizerInvalidProgram
	tst.w d3
	beq.w tkpkgTokenizerInvalidProgram
	moveq #0, d0
	move.b ActiveTokenizerVmStartStateLo, d0
	moveq #0, d1
	move.b ActiveTokenizerVmStartStateHi, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	move.b ActiveTokenizerVmStateCountLo, d1
	moveq #0, d2
	move.b ActiveTokenizerVmStateCountHi, d2
	lsl.w #8, d2
	or.w d2, d1
	cmp.w d1, d0
	bcc.w tkpkgTokenizerInvalidProgram
	moveq #0, d0
	rts

tkpkgTokenizerInvalidProgram
	lea InvalidProgramText, a1
	moveq #INVALID_PROGRAM_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgTokenizerDebugBadProgramHeader
	lea BadProgramHeaderText, a1
	moveq #BAD_PROGRAM_HEADER_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

; Convert a tokvm status/failure code into the tkpkg runtime diagnostic string.
tkpkgTokenizerVmStatusMessageV1
	cmpi.b #TK_STATUS_NEWLINE_UNSUPPORTED, d0
	beq.s tkpkgTokenizerStatusNewline
	cmpi.b #TK_STATUS_STEP_LIMIT_EXCEEDED, d0
	beq.s tkpkgTokenizerStatusStepLimit
	cmpi.b #TK_STATUS_TOKEN_OVERFLOW, d0
	beq.s tkpkgTokenizerStatusTokenOverflow
	cmpi.b #TK_STATUS_LEXEME_OVERFLOW, d0
	beq.s tkpkgTokenizerStatusLexemeOverflow
	cmpi.b #TK_STATUS_VM_FAILURE, d0
	bne.s tkpkgTokenizerStatusCheckInvalidArgument
	bra.w tkpkgTokenizerStatusVmFailure
tkpkgTokenizerStatusCheckInvalidArgument
	cmpi.b #TK_STATUS_INVALID_ARGUMENT, d0
	bne.s tkpkgTokenizerStatusFallbackInvalidProgram
	bra.w tkpkgTokenizerStatusInvalidArgument
tkpkgTokenizerStatusFallbackInvalidProgram
	lea InvalidProgramText, a1
	moveq #INVALID_PROGRAM_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgTokenizerStatusNewline
	lea NewlineText, a1
	moveq #NEWLINE_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgTokenizerStatusStepLimit
	bsr.w tkpkgTokenizerVmBeginStatusBufferV1
	moveq #2, d0
	bsr.w tkpkgTokenizerVmGetDiagCodeV1
	bsr.w tkpkgTokenizerVmAppendBytesV1
	lea StepLimitSuffixText, a1
	moveq #STEP_LIMIT_SUFFIX_LEN, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	bra.w tkpkgTokenizerVmFinishStatusBufferV1

tkpkgTokenizerStatusTokenOverflow
	bsr.w tkpkgTokenizerVmBeginStatusBufferV1
	moveq #3, d0
	bsr.w tkpkgTokenizerVmGetDiagCodeV1
	bsr.w tkpkgTokenizerVmAppendBytesV1
	lea TokenOverflowSuffixText, a1
	moveq #TOKEN_OVERFLOW_SUFFIX_LEN, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	bra.w tkpkgTokenizerVmFinishStatusBufferV1

tkpkgTokenizerStatusLexemeOverflow
	bsr.w tkpkgTokenizerVmBeginStatusBufferV1
	moveq #4, d0
	bsr.w tkpkgTokenizerVmGetDiagCodeV1
	bsr.w tkpkgTokenizerVmAppendBytesV1
	lea LexemeOverflowSuffixText, a1
	moveq #LEXEME_OVERFLOW_SUFFIX_LEN, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	bra.w tkpkgTokenizerVmFinishStatusBufferV1

tkpkgTokenizerStatusVmFailure
	jsr tokvmReadLastFailure68000
	cmpi.w #TK_VM_FAILURE_KIND_FAIL, d0
	beq.w tkpkgTokenizerStatusVmFailReason
	cmpi.w #TK_VM_FAILURE_KIND_EMIT_DIAG, d0
	beq.w tkpkgTokenizerStatusVmEmitDiag
	lea VmFailureFallbackText, a1
	moveq #VM_FAILURE_FALLBACK_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgTokenizerStatusVmFailReason
	bsr.w tkpkgTokenizerVmBeginStatusBufferV1
	move.l d1, d6
	moveq #0, d0
	bsr.w tkpkgTokenizerVmGetDiagCodeV1
	bsr.w tkpkgTokenizerVmAppendBytesV1
	lea VmFailureReasonSuffixText, a1
	moveq #VM_FAILURE_REASON_SUFFIX_LEN, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	move.l d6, d0
	bsr.w tkpkgTokenizerVmAppendU32V1
	bra.w tkpkgTokenizerVmFinishStatusBufferV1

tkpkgTokenizerStatusVmEmitDiag
	move.l d1, d6
	tst.l ActiveTokenizerVmMaxErrorsPerLine
	beq.s tkpkgTokenizerStatusVmDiagBudgetExceeded
	bsr.w tkpkgTokenizerVmBeginStatusBufferV1
	move.l d6, d0
	bsr.w tkpkgTokenizerVmGetDiagCodeV1
	bsr.w tkpkgTokenizerVmAppendBytesV1
	lea VmEmitDiagSuffixText, a1
	moveq #VM_EMIT_DIAG_SUFFIX_LEN, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	move.l d6, d0
	bsr.w tkpkgTokenizerVmAppendU32V1
	bra.w tkpkgTokenizerVmFinishStatusBufferV1

tkpkgTokenizerStatusVmDiagBudgetExceeded
	bsr.w tkpkgTokenizerVmBeginStatusBufferV1
	moveq #5, d0
	bsr.w tkpkgTokenizerVmGetDiagCodeV1
	bsr.w tkpkgTokenizerVmAppendBytesV1
	lea VmDiagBudgetExceededText, a1
	moveq #VM_DIAG_BUDGET_EXCEEDED_LEN, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	bra.w tkpkgTokenizerVmFinishStatusBufferV1

tkpkgTokenizerStatusInvalidArgument
	lea InvalidArgumentText, a1
	moveq #INVALID_ARGUMENT_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	rts

tkpkgTokenizerVmBeginStatusBufferV1
	move.l (sp)+, d0
	suba.l #LOCAL_SIZE, sp
	move.l d0, -(sp)
	lea 4(sp), a4
	clr.l LOCAL_OUTPUT_LEN(a4)
	clr.l LOCAL_OUTPUT_OVERFLOW(a4)
	clr.b LastErrorBuffer
	rts

tkpkgTokenizerVmFinishStatusBufferV1
	tst.l LOCAL_OUTPUT_OVERFLOW(a4)
	bne.s tkpkgTokenizerFinishStatusOverflow
	lea LastErrorBuffer, a1
	move.l LOCAL_OUTPUT_LEN(a4), d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	adda.l #LOCAL_SIZE, sp
	rts

tkpkgTokenizerFinishStatusOverflow
	lea OutputOverflowText, a1
	moveq #OUTPUT_OVERFLOW_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	adda.l #LOCAL_SIZE, sp
	rts

tkpkgTokenizerVmGetDiagCodeV1
	cmpi.b #1, d0
	beq.s tkpkgTokenizerDiagUnterminatedString
	cmpi.b #2, d0
	beq.s tkpkgTokenizerDiagStepLimit
	cmpi.b #3, d0
	beq.s tkpkgTokenizerDiagTokenLimit
	cmpi.b #4, d0
	beq.s tkpkgTokenizerDiagLexemeLimit
	cmpi.b #5, d0
	beq.s tkpkgTokenizerDiagErrorLimit
	lea ActiveTokenizerVmInvalidCharDiagCode, a1
	moveq #0, d2
	move.b ActiveTokenizerVmInvalidCharDiagLen, d2
	rts

tkpkgTokenizerDiagUnterminatedString
	lea ActiveTokenizerVmUnterminatedStringDiagCode, a1
	moveq #0, d2
	move.b ActiveTokenizerVmUnterminatedStringDiagLen, d2
	rts

tkpkgTokenizerDiagStepLimit
	lea ActiveTokenizerVmStepLimitDiagCode, a1
	moveq #0, d2
	move.b ActiveTokenizerVmStepLimitDiagLen, d2
	rts

tkpkgTokenizerDiagTokenLimit
	lea ActiveTokenizerVmTokenLimitDiagCode, a1
	moveq #0, d2
	move.b ActiveTokenizerVmTokenLimitDiagLen, d2
	rts

tkpkgTokenizerDiagLexemeLimit
	lea ActiveTokenizerVmLexemeLimitDiagCode, a1
	moveq #0, d2
	move.b ActiveTokenizerVmLexemeLimitDiagLen, d2
	rts

tkpkgTokenizerDiagErrorLimit
	lea ActiveTokenizerVmErrorLimitDiagCode, a1
	moveq #0, d2
	move.b ActiveTokenizerVmErrorLimitDiagLen, d2
	rts

; Validate tokvm output counts and spans before rendering report bytes.
tkpkgTokenizerVmValidateResultV1
	movem.l d1-d7/a0, -(sp)
	cmp.l #TOKEN_BUFFER_CAPACITY, d1
	bhi.s tkpkgTokenizerValidateInvalid
	cmp.l d4, d2
	bhi.s tkpkgTokenizerValidateInvalid
	cmp.l #TOKEN_SCRATCH_CAPACITY, d3
	bhi.s tkpkgTokenizerValidateInvalid
	lea TokenRecordBuffer, a0
	moveq #0, d5

tkpkgTokenizerValidateLoop
	cmp.l d1, d5
	bcc.s tkpkgTokenizerValidateOk
	moveq #0, d0
	move.w (a0), d0
	cmpi.l #TK_KIND_OP_LT, d0
	bgt.s tkpkgTokenizerValidateInvalid
	move.l 4(a0), d6
	tst.l d6
	beq.s tkpkgTokenizerValidateInvalid
	move.l d4, d7
	addq.l #1, d7
	cmp.l d7, d6
	bhi.s tkpkgTokenizerValidateInvalid
	move.l 8(a0), d6
	tst.l d6
	beq.s tkpkgTokenizerValidateInvalid
	cmp.l d7, d6
	bhi.s tkpkgTokenizerValidateInvalid
	cmp.l 4(a0), d6
	blt.s tkpkgTokenizerValidateInvalid
	move.l 12(a0), d6
	cmp.l d3, d6
	bhi.s tkpkgTokenizerValidateInvalid
	move.l 16(a0), d7
	add.l d6, d7
	cmp.l d3, d7
	bhi.s tkpkgTokenizerValidateInvalid
	adda.l #TOKEN_RECORD_SIZE, a0
	addq.l #1, d5
	bra.s tkpkgTokenizerValidateLoop

tkpkgTokenizerValidateOk
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0
	rts

tkpkgTokenizerValidateInvalid
	moveq #1, d0
	movem.l (sp)+, d1-d7/a0
	rts

; Render token records into the line-oriented tkpkg tokenizer service output.
tkpkgTokenizerVmRenderOutputV1
	movem.l d2-d7/a2-a6, -(sp)
	suba.l #LOCAL_SIZE, sp
	lea 0(sp), a4
	move.l d1, d7
	clr.l LOCAL_OUTPUT_LEN(a4)
	clr.l LOCAL_OUTPUT_OVERFLOW(a4)
	move.l d6, LOCAL_RENDER_LINE(a4)
	clr.b LastErrorBuffer
	moveq #0, d6

tkpkgTokenizerRenderLoop
	cmp.l d7, d6
	bcc.s tkpkgTokenizerRenderDone
	move.l d6, d0
	bsr.w tkpkgTokenizerVmRecordPtrV1
	movea.l a0, a5
	moveq #0, d0
	move.w (a5), d0
	move.l 12(a5), d2
	move.l 16(a5), d3
	lea TokenScratchBuffer, a6
	adda.l d2, a6
	bsr.w tkpkgTokenizerVmAppendKindDebugV1
	bsr.w tkpkgTokenizerVmAppendLiteralAtV1
	move.l LOCAL_RENDER_LINE(a4), d0
	bsr.w tkpkgTokenizerVmAppendU32V1
	bsr.w tkpkgTokenizerVmAppendLiteralColonV1
	move.l 4(a5), d0
	bsr.w tkpkgTokenizerVmAppendU32V1
	bsr.w tkpkgTokenizerVmAppendLiteralDashV1
	move.l 8(a5), d0
	bsr.w tkpkgTokenizerVmAppendU32V1
	bsr.w tkpkgTokenizerVmAppendLiteralNewlineV1
	tst.l LOCAL_OUTPUT_OVERFLOW(a4)
	bne.s tkpkgTokenizerRenderOverflow
	addq.l #1, d6
	bra.s tkpkgTokenizerRenderLoop

tkpkgTokenizerRenderDone
	move.l LOCAL_OUTPUT_LEN(a4), d1
	moveq #0, d0
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgTokenizerRenderOverflow
	lea OutputOverflowText, a1
	moveq #OUTPUT_OVERFLOW_TEXT_LEN, d1
	moveq #STATUS_RUNTIME_ERROR_V1, d0
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgTokenizerVmAppendKindDebugV1
	cmpi.w #TK_KIND_IDENTIFIER, d0
	beq.w tkpkgTokenizerAppendIdentifier
	cmpi.w #TK_KIND_NUMBER, d0
	beq.w tkpkgTokenizerAppendNumber
	cmpi.w #TK_KIND_STRING, d0
	beq.w tkpkgTokenizerAppendString
	cmpi.w #TK_KIND_COMMA, d0
	beq.w tkpkgTokenizerAppendBareComma
	cmpi.w #TK_KIND_COLON, d0
	beq.w tkpkgTokenizerAppendBareColon
	cmpi.w #TK_KIND_DOLLAR, d0
	beq.w tkpkgTokenizerAppendBareDollar
	cmpi.w #TK_KIND_DOT, d0
	beq.w tkpkgTokenizerAppendBareDot
	cmpi.w #TK_KIND_HASH, d0
	beq.w tkpkgTokenizerAppendBareHash
	cmpi.w #TK_KIND_QUESTION, d0
	beq.w tkpkgTokenizerAppendBareQuestion
	cmpi.w #TK_KIND_OPEN_BRACKET, d0
	beq.w tkpkgTokenizerAppendBareOpenBracket
	cmpi.w #TK_KIND_CLOSE_BRACKET, d0
	beq.w tkpkgTokenizerAppendBareCloseBracket
	cmpi.w #TK_KIND_OPEN_BRACE, d0
	beq.w tkpkgTokenizerAppendBareOpenBrace
	cmpi.w #TK_KIND_CLOSE_BRACE, d0
	beq.w tkpkgTokenizerAppendBareCloseBrace
	cmpi.w #TK_KIND_OPEN_PAREN, d0
	beq.w tkpkgTokenizerAppendBareOpenParen
	cmpi.w #TK_KIND_CLOSE_PAREN, d0
	beq.w tkpkgTokenizerAppendBareCloseParen
	bra.w tkpkgTokenizerAppendOperator

tkpkgTokenizerAppendIdentifier
	lea IdentifierPrefix, a1
	moveq #11, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	movea.l a6, a1
	bsr.w tkpkgTokenizerVmAppendQuotedV1
	bra.w tkpkgTokenizerVmAppendLiteralCloseParenV1

tkpkgTokenizerAppendNumber
	lea NumberPrefix, a1
	moveq #15, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	movea.l a6, a1
	bsr.w tkpkgTokenizerVmAppendUpperQuotedV1
	lea NumberBasePrefix, a1
	moveq #8, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	moveq #10, d0
	tst.l d3
	beq.s tkpkgTokenizerAppendNumberBaseDone
	moveq #0, d1
	move.b (a6), d1
	cmpi.b #'$', d1
	beq.s tkpkgTokenizerAppendNumberHex
	cmpi.b #'%', d1
	beq.s tkpkgTokenizerAppendNumberBin
	moveq #0, d2
	movea.l a6, a1
	adda.l d3, a1
	subq.l #1, a1
	move.b (a1), d2
	cmpi.b #'a', d2
	blo.s tkpkgTokenizerAppendNumberSuffix
	cmpi.b #'z', d2
	bhi.s tkpkgTokenizerAppendNumberSuffix
	andi.b #$DF, d2
tkpkgTokenizerAppendNumberSuffix
	cmpi.b #'H', d2
	beq.s tkpkgTokenizerAppendNumberHex
	cmpi.b #'B', d2
	beq.s tkpkgTokenizerAppendNumberBin
	cmpi.b #'O', d2
	beq.s tkpkgTokenizerAppendNumberOct
	cmpi.b #'Q', d2
	bne.s tkpkgTokenizerAppendNumberBaseDone
tkpkgTokenizerAppendNumberOct
	moveq #8, d0
	bra.s tkpkgTokenizerAppendNumberBaseDone
tkpkgTokenizerAppendNumberHex
	moveq #16, d0
	bra.s tkpkgTokenizerAppendNumberBaseDone
tkpkgTokenizerAppendNumberBin
	moveq #2, d0
tkpkgTokenizerAppendNumberBaseDone
	bsr.w tkpkgTokenizerVmAppendU32V1
	lea StringSuffix, a1
	addq.l #1, a1
	moveq #2, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	rts

tkpkgTokenizerAppendString
	lea StringPrefix, a1
	moveq #14, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	movea.l a6, a1
	bsr.w tkpkgTokenizerVmAppendStringRawV1
	lea StringBytesPrefix, a1
	moveq #10, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	movea.l a6, a1
	bsr.w tkpkgTokenizerVmAppendByteListV1
	lea StringSuffix, a1
	moveq #3, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	rts

tkpkgTokenizerAppendBareComma
	lea KindCommaText, a1
	moveq #5, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareColon
	lea KindColonText, a1
	moveq #5, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareDollar
	lea KindDollarText, a1
	moveq #6, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareDot
	lea KindDotText, a1
	moveq #3, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareHash
	lea KindHashText, a1
	moveq #4, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareQuestion
	lea KindQuestionText, a1
	moveq #8, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareOpenBracket
	lea KindOpenBracketText, a1
	moveq #11, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareCloseBracket
	lea KindCloseBracketText, a1
	moveq #12, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareOpenBrace
	lea KindOpenBraceText, a1
	moveq #9, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareCloseBrace
	lea KindCloseBraceText, a1
	moveq #10, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareOpenParen
	lea KindOpenParenText, a1
	moveq #9, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerAppendBareCloseParen
	lea KindCloseParenText, a1
	moveq #10, d2
	bra.w tkpkgTokenizerVmAppendBytesV1

tkpkgTokenizerAppendOperator
	move.l d0, -(sp)
	lea OperatorPrefix, a1
	moveq #9, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	move.l (sp)+, d0
	bsr.w tkpkgTokenizerVmAppendOperatorNameV1
	bra.w tkpkgTokenizerVmAppendLiteralCloseParenV1

tkpkgTokenizerVmAppendOperatorNameV1
	cmpi.w #TK_KIND_OP_RANGE, d0
	beq.w tkpkgTokenizerOpRange
	cmpi.w #TK_KIND_OP_RANGE_INCLUSIVE, d0
	beq.w tkpkgTokenizerOpRangeInclusive
	cmpi.w #TK_KIND_OP_PLUS, d0
	beq.w tkpkgTokenizerOpPlus
	cmpi.w #TK_KIND_OP_MINUS, d0
	beq.w tkpkgTokenizerOpMinus
	cmpi.w #TK_KIND_OP_MULTIPLY, d0
	beq.w tkpkgTokenizerOpMultiply
	cmpi.w #TK_KIND_OP_POWER, d0
	beq.w tkpkgTokenizerOpPower
	cmpi.w #TK_KIND_OP_DIVIDE, d0
	beq.w tkpkgTokenizerOpDivide
	cmpi.w #TK_KIND_OP_MOD, d0
	beq.w tkpkgTokenizerOpMod
	cmpi.w #TK_KIND_OP_SHL, d0
	beq.w tkpkgTokenizerOpShl
	cmpi.w #TK_KIND_OP_SHR, d0
	beq.w tkpkgTokenizerOpShr
	cmpi.w #TK_KIND_OP_BIT_NOT, d0
	beq.w tkpkgTokenizerOpBitNot
	cmpi.w #TK_KIND_OP_LOGIC_NOT, d0
	beq.w tkpkgTokenizerOpLogicNot
	cmpi.w #TK_KIND_OP_BIT_AND, d0
	beq.w tkpkgTokenizerOpBitAnd
	cmpi.w #TK_KIND_OP_BIT_OR, d0
	beq.w tkpkgTokenizerOpBitOr
	cmpi.w #TK_KIND_OP_BIT_XOR, d0
	beq.w tkpkgTokenizerOpBitXor
	cmpi.w #TK_KIND_OP_LOGIC_AND, d0
	beq.w tkpkgTokenizerOpLogicAnd
	cmpi.w #TK_KIND_OP_LOGIC_OR, d0
	beq.w tkpkgTokenizerOpLogicOr
	cmpi.w #TK_KIND_OP_LOGIC_XOR, d0
	beq.w tkpkgTokenizerOpLogicXor
	cmpi.w #TK_KIND_OP_EQ, d0
	beq.w tkpkgTokenizerOpEq
	cmpi.w #TK_KIND_OP_NE, d0
	beq.w tkpkgTokenizerOpNe
	cmpi.w #TK_KIND_OP_GE, d0
	beq.w tkpkgTokenizerOpGe
	cmpi.w #TK_KIND_OP_GT, d0
	beq.w tkpkgTokenizerOpGt
	cmpi.w #TK_KIND_OP_LE, d0
	beq.w tkpkgTokenizerOpLe
	lea OpLtText, a1
	moveq #2, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpRange
	lea OpRangeText, a1
	moveq #5, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpRangeInclusive
	lea OpRangeInclusiveText, a1
	moveq #14, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpPlus
	lea OpPlusText, a1
	moveq #4, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpMinus
	lea OpMinusText, a1
	moveq #5, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpMultiply
	lea OpMultiplyText, a1
	moveq #8, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpPower
	lea OpPowerText, a1
	moveq #5, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpDivide
	lea OpDivideText, a1
	moveq #6, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpMod
	lea OpModText, a1
	moveq #3, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpShl
	lea OpShlText, a1
	moveq #3, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpShr
	lea OpShrText, a1
	moveq #3, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpBitNot
	lea OpBitNotText, a1
	moveq #6, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpLogicNot
	lea OpLogicNotText, a1
	moveq #8, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpBitAnd
	lea OpBitAndText, a1
	moveq #6, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpBitOr
	lea OpBitOrText, a1
	moveq #5, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpBitXor
	lea OpBitXorText, a1
	moveq #6, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpLogicAnd
	lea OpLogicAndText, a1
	moveq #8, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpLogicOr
	lea OpLogicOrText, a1
	moveq #7, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpLogicXor
	lea OpLogicXorText, a1
	moveq #8, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpEq
	lea OpEqText, a1
	moveq #2, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpNe
	lea OpNeText, a1
	moveq #2, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpGe
	lea OpGeText, a1
	moveq #2, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpGt
	lea OpGtText, a1
	moveq #2, d2
	bra.w tkpkgTokenizerVmAppendBytesV1
tkpkgTokenizerOpLe
	lea OpLeText, a1
	moveq #2, d2
	bra.w tkpkgTokenizerVmAppendBytesV1

tkpkgTokenizerVmAppendQuotedV1
	movem.l d0-d1/d5, -(sp)
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	move.l d3, d5
	beq.s tkpkgTokenizerQuotedClose

tkpkgTokenizerQuotedLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w tkpkgTokenizerVmAppendEscapedCharV1
	subq.l #1, d5
	bne.s tkpkgTokenizerQuotedLoop

tkpkgTokenizerQuotedClose
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	movem.l (sp)+, d0-d1/d5
	rts

tkpkgTokenizerVmAppendUpperQuotedV1
	movem.l d0-d1/d5, -(sp)
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	move.l d3, d5
	beq.s tkpkgTokenizerUpperQuotedClose

tkpkgTokenizerUpperQuotedLoop
	moveq #0, d0
	move.b (a1)+, d0
	cmpi.b #'a', d0
	blo.s tkpkgTokenizerUpperQuotedEmit
	cmpi.b #'z', d0
	bhi.s tkpkgTokenizerUpperQuotedEmit
	andi.b #$DF, d0
tkpkgTokenizerUpperQuotedEmit
	bsr.w tkpkgTokenizerVmAppendEscapedCharV1
	subq.l #1, d5
	bne.s tkpkgTokenizerUpperQuotedLoop

tkpkgTokenizerUpperQuotedClose
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	movem.l (sp)+, d0-d1/d5
	rts

tkpkgTokenizerVmAppendStringRawV1
	movem.l d0-d1/d5/a1, -(sp)
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendEscapedCharV1
	move.l d3, d5
	beq.s tkpkgTokenizerStringRawClose

tkpkgTokenizerStringRawLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w tkpkgTokenizerVmAppendEscapedCharV1
	subq.l #1, d5
	bne.s tkpkgTokenizerStringRawLoop

tkpkgTokenizerStringRawClose
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendEscapedCharV1
	moveq #'"', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	movem.l (sp)+, d0-d1/d5/a1
	rts

tkpkgTokenizerVmAppendByteListV1
	movem.l d0-d1/d5/a1, -(sp)
	move.l d3, d5
	beq.s tkpkgTokenizerByteListDone

tkpkgTokenizerByteListLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w tkpkgTokenizerVmAppendU32V1
	subq.l #1, d5
	beq.s tkpkgTokenizerByteListDone
	move.l a1, -(sp)
	lea CommaSpaceText, a1
	moveq #2, d2
	bsr.w tkpkgTokenizerVmAppendBytesV1
	movea.l (sp)+, a1
	bra.s tkpkgTokenizerByteListLoop

tkpkgTokenizerByteListDone
	movem.l (sp)+, d0-d1/d5/a1
	rts

tkpkgTokenizerVmAppendEscapedCharV1
	cmpi.b #'\\', d0
	beq.s tkpkgTokenizerEscapeBackslash
	cmpi.b #'"', d0
	beq.s tkpkgTokenizerEscapeQuote
	cmpi.b #10, d0
	beq.s tkpkgTokenizerEscapeLf
	cmpi.b #13, d0
	beq.s tkpkgTokenizerEscapeCr
	cmpi.b #9, d0
	beq.s tkpkgTokenizerEscapeTab
	bra.w tkpkgTokenizerVmAppendCharV1

tkpkgTokenizerEscapeBackslash
	moveq #'\\', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	moveq #'\\', d0
	bra.w tkpkgTokenizerVmAppendCharV1

tkpkgTokenizerEscapeQuote
	moveq #'\\', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	moveq #'"', d0
	bra.w tkpkgTokenizerVmAppendCharV1

tkpkgTokenizerEscapeLf
	moveq #'\\', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	moveq #'n', d0
	bra.w tkpkgTokenizerVmAppendCharV1

tkpkgTokenizerEscapeCr
	moveq #'\\', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	moveq #'r', d0
	bra.w tkpkgTokenizerVmAppendCharV1

tkpkgTokenizerEscapeTab
	moveq #'\\', d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	moveq #'t', d0
	bra.w tkpkgTokenizerVmAppendCharV1

tkpkgTokenizerVmAppendU32V1
	movem.l d1-d5/a1, -(sp)
	lea DecimalPowers, a1
	clr.l d4
	moveq #9, d5

tkpkgTokenizerAppendU32Loop
	move.l (a1)+, d2
	moveq #0, d3

tkpkgTokenizerAppendDigitCount
	cmp.l d2, d0
	blo.s tkpkgTokenizerAppendDigitReady
	sub.l d2, d0
	addq.b #1, d3
	bra.s tkpkgTokenizerAppendDigitCount

tkpkgTokenizerAppendDigitReady
	tst.l d4
	bne.s tkpkgTokenizerAppendDigitEmit
	tst.b d3
	bne.s tkpkgTokenizerAppendDigitStart
	tst.w d5
	bne.s tkpkgTokenizerAppendDigitSkip

tkpkgTokenizerAppendDigitStart
	moveq #1, d4

tkpkgTokenizerAppendDigitEmit
	move.l d0, -(sp)
	moveq #'0', d4
	add.b d3, d4
	move.l d4, d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	move.l (sp)+, d0

tkpkgTokenizerAppendDigitSkip
	dbf d5, tkpkgTokenizerAppendU32Loop
	movem.l (sp)+, d1-d5/a1
	rts

tkpkgTokenizerVmAppendBytesV1
	tst.w d2
	beq.s tkpkgTokenizerAppendBytesDone

tkpkgTokenizerAppendBytesLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w tkpkgTokenizerVmAppendCharV1
	subq.w #1, d2
	bne.s tkpkgTokenizerAppendBytesLoop

tkpkgTokenizerAppendBytesDone
	rts

tkpkgTokenizerVmAppendCharV1
	move.l a1, -(sp)
	move.l LOCAL_OUTPUT_LEN(a4), d1
	cmpi.l #LAST_ERROR_BUFFER_CAPACITY - 1, d1
	bcs.s tkpkgTokenizerAppendCharStore
	moveq #1, d1
	move.l d1, LOCAL_OUTPUT_OVERFLOW(a4)
	movea.l (sp)+, a1
	rts

tkpkgTokenizerAppendCharStore
	lea LastErrorBuffer, a1
	move.b d0, 0(a1, d1.l)
	addq.l #1, d1
	move.l d1, LOCAL_OUTPUT_LEN(a4)
	clr.b 0(a1, d1.l)
	movea.l (sp)+, a1
	rts

tkpkgTokenizerVmNumberBaseV1
	moveq #10, d0
	tst.l d3
	beq.s tkpkgTokenizerNumberBaseDone
	moveq #0, d1
	move.b (a1), d1
	cmpi.b #'$', d1
	beq.s tkpkgTokenizerNumberBaseHex
	cmpi.b #'%', d1
	beq.s tkpkgTokenizerNumberBaseBin
	moveq #0, d2
	movea.l a1, a0
	adda.l d3, a0
	subq.l #1, a0
	move.b (a0), d2
	cmpi.b #'a', d2
	blo.s tkpkgTokenizerNumberBaseSuffix
	cmpi.b #'z', d2
	bhi.s tkpkgTokenizerNumberBaseSuffix
	andi.b #$DF, d2

tkpkgTokenizerNumberBaseSuffix
	cmpi.b #'H', d2
	beq.s tkpkgTokenizerNumberBaseHex
	cmpi.b #'B', d2
	beq.s tkpkgTokenizerNumberBaseBin
	cmpi.b #'O', d2
	beq.s tkpkgTokenizerNumberBaseOct
	cmpi.b #'Q', d2
	beq.s tkpkgTokenizerNumberBaseOct
	bra.s tkpkgTokenizerNumberBaseDone

tkpkgTokenizerNumberBaseHex
	moveq #16, d0
	rts
tkpkgTokenizerNumberBaseBin
	moveq #2, d0
	rts
tkpkgTokenizerNumberBaseOct
	moveq #8, d0
tkpkgTokenizerNumberBaseDone
	rts

tkpkgTokenizerVmRecordPtrV1
	move.l d0, d1
	add.l d1, d1
	movea.l d1, a0
	add.l d1, d1
	add.l d1, d1
	add.l d1, d1
	adda.l d1, a0
	move.l d0, d1
	add.l d1, d1
	adda.l d1, a0
	lea TokenRecordBuffer, a1
	adda.l a0, a1
	movea.l a1, a0
	rts

tkpkgTokenizerVmAppendLiteralAtV1
	lea AtSep, a1
	moveq #1, d2
	bra.w tkpkgTokenizerVmAppendBytesV1

tkpkgTokenizerVmAppendLiteralColonV1
	lea ColonSep, a1
	moveq #1, d2
	bra.w tkpkgTokenizerVmAppendBytesV1

tkpkgTokenizerVmAppendLiteralDashV1
	lea DashSep, a1
	moveq #1, d2
	bra.w tkpkgTokenizerVmAppendBytesV1

tkpkgTokenizerVmAppendLiteralNewlineV1
	lea NewlineSep, a1
	moveq #1, d2
	bra.w tkpkgTokenizerVmAppendBytesV1

tkpkgTokenizerVmAppendLiteralCloseParenV1
	lea CloseParenText, a1
	moveq #1, d2
	bra.w tkpkgTokenizerVmAppendBytesV1

tkpkgTokenizerVmSkipStringV1
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.s tkpkgTokenizerSkipStringDone
	bsr.w tkpkgTokenizerVmRequireBytesV1
	tst.b d1
	bne.s tkpkgTokenizerSkipStringDone
	adda.l d0, a2
tkpkgTokenizerSkipStringDone
	rts

tkpkgTokenizerVmReadStringIntoSlotV1
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.s tkpkgTokenizerReadStringDone
	bsr.w tkpkgTokenizerVmRequireBytesV1
	tst.b d1
	bne.s tkpkgTokenizerReadStringDone
	move.l d0, d2
	cmpi.l #31, d2
	bls.s tkpkgTokenizerReadStringLenReady
	moveq #31, d2
tkpkgTokenizerReadStringLenReady
	move.b d2, (a1)
	move.l d2, d1
	movea.l a2, a0
tkpkgTokenizerReadStringCopyLoop
	tst.l d1
	beq.s tkpkgTokenizerReadStringCopyDone
	move.b (a0)+, (a3)+
	subq.l #1, d1
	bra.s tkpkgTokenizerReadStringCopyLoop
tkpkgTokenizerReadStringCopyDone
	clr.b (a3)
	adda.l d0, a2
	moveq #0, d1
tkpkgTokenizerReadStringDone
	rts

tkpkgTokenizerVmReadBytesFieldV1
	bsr.w tkpkgTokenizerVmReadU32LeV1
	tst.b d1
	bne.s tkpkgTokenizerReadBytesDone
	bsr.w tkpkgTokenizerVmRequireBytesV1
	tst.b d1
	bne.s tkpkgTokenizerReadBytesDone
	movea.l a2, a3
	move.l d0, d3
	adda.l d0, a2
	moveq #0, d1
tkpkgTokenizerReadBytesDone
	rts

tkpkgTokenizerVmReadU16LeV1
	moveq #2, d0
	bsr.w tkpkgTokenizerVmRequireBytesV1
	tst.b d1
	bne.s tkpkgTokenizerReadU16Done
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
tkpkgTokenizerReadU16Done
	rts

tkpkgTokenizerVmReadU32LeV1
	moveq #4, d0
	bsr.w tkpkgTokenizerVmRequireBytesV1
	tst.b d1
	bne.s tkpkgTokenizerReadU32Done
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
tkpkgTokenizerReadU32Done
	rts

tkpkgTokenizerVmRequireBytesV1
	cmpa.l a6, a2
	bhi.s tkpkgTokenizerRequireBytesFail
	move.l a6, d1
	sub.l a2, d1
	cmp.l d1, d0
	bhi.s tkpkgTokenizerRequireBytesFail
	moveq #0, d1
	rts

tkpkgTokenizerRequireBytesFail
	moveq #1, d1
	rts

	.endsection
	.endmodule
