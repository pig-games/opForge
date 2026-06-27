; Package-backed tokenizer-VM wrapper for the first tkpkg tokenize_line slice.

	.module tkpkg.amigaos.tokenizer_vm
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkvm.amigaos.runtime
	.use tkvm.amigaos.control
	.use tkvm.amigaos.state

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

AtDelimiterText
	.byte "@"

ColonDelimiterText
	.byte ":"

DashDelimiterText
	.byte "-"

NewlineDelimiterText
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
; This entry bridges a selected tkpkg runtime pipeline to the lower-level tkvm
; interpreter. It reads the line payload from the control block, decodes the
; active TKVM package record, executes tkvm, then renders compact token records
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

	.pub
tkpkgTokenizerVmTokenizeLineV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	btst #1, buffers.PackageStateFlags  ; require set_pipeline before executing any package VM program
	bne.s pipelineReady
	lea NoPipelineText, a1
	moveq #NO_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w tokenizeDone

pipelineReady
	bsr.w readLinePayload
	bne.w tokenizeDone
	bsr.w readProgram
	bne.w tokenizeDone
	move.l #state.TKVM_DEFAULT_MAX_STEPS_PER_LINE, d0
	jsr control.tkvmSetStepBudget68000  ; keep tkpkg-driven tokenizer runs under the bounded VM budget
	movea.l a3, a5  ; A5 keeps program bytes while A3 is reused for tkvm call ABI
	move.l d3, d7  ; D7 keeps program length while record metadata is decoded
	cmpi.b #1, (a5)
	bne.w badProgramHeader
	cmpi.b #8, 1(a5)
	bne.w badProgramHeader
	move.l d6, -(sp)
	lea buffers.ActiveTokenizerVmStateTable, a0
	moveq #0, d0
	move.b buffers.ActiveTokenizerVmStateCountLo, d0
	moveq #0, d1
	move.b buffers.ActiveTokenizerVmStateCountHi, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	move.b buffers.ActiveTokenizerVmStartStateLo, d1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmStartStateHi, d2
	lsl.w #8, d2
	or.w d2, d1
	jsr control.tkvmSetProgramStateTable68000  ; install package state table into shared tkvm core
	movea.l a4, a0  ; tkvm input pointer: source bytes after line-number prefix
	move.l d4, d0  ; tkvm input length: source byte count
	lea buffers.TokenRecordBuffer, a1  ; tkvm output token records
	moveq #0, d1
	move.w #buffers.TOKEN_BUFFER_CAPACITY, d1
	lea buffers.TokenScratchBuffer, a2  ; lexeme scratch mirrors Rust portable-token lexeme storage
	moveq #0, d2
	move.w #buffers.TOKEN_SCRATCH_CAPACITY, d2
	movea.l a5, a3  ; tkvm program pointer
	move.l d7, d3  ; tkvm program length
	jsr runtime.tkvmRun68000
	move.l (sp)+, d6
	cmpi.b #runtime.TK_STATUS_SUCCESS, d0
	beq.s render
	bsr.w statusMessage
	bra.w tokenizeDone

render
	move.w d1, buffers.LastTokenCount
	move.w d3, buffers.LastLexemeLen
	bsr.w validateResult
	bne.w invalidProgram
	bsr.w renderOutput
	bra.w tokenizeDone

invalidProgram
	lea InvalidProgramText, a1
	moveq #INVALID_PROGRAM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.w tokenizeDone

badProgramHeader
	lea BadProgramHeaderText, a1
	moveq #BAD_PROGRAM_HEADER_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0

tokenizeDone
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; tkpkgTokenizerVmTokenizeLineV1
	.priv

; Read the line-number-prefixed tokenizer service payload.
readLinePayload	.block
	moveq #0, d0
	move.b abi.CB_INPUT_LEN(a0), d0
	moveq #0, d1
	move.b 19(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #4, d0
	blo.s badPayload
	moveq #0, d4
	move.w d0, d4
	subq.w #4, d4
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0
	moveq #0, d1
	move.b 17(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	beq.s badPayload
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

badPayload
	lea BadPayloadText, a1
	moveq #BAD_PAYLOAD_TEXT_LEN, d1
	moveq #abi.STATUS_BAD_REQUEST_V1, d0
	rts
	.bend  ; readLinePayload

; Decode the active TKVM package record and expose program bytes/state table.
readProgram	.block
	lea buffers.ActiveTokenizerVmOffsetLo, a1
	moveq #0, d0
	move.b (a1)+, d0
	moveq #0, d1
	move.b (a1)+, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a1)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a1)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d1
	move.b (a1)+, d1
	lsl.l #8, d1
	or.l d1, d2
	moveq #0, d1
	move.b (a1)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d2
	moveq #0, d1
	move.b (a1)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d2
	beq.w invalidProgram
	lea buffers.PackageStorage, a2
	lea 0(a2, d0.l), a2
	movea.l a2, a6
	adda.l d2, a6
	moveq #1, d0
	bsr.w requireBytes
	bne.w invalidProgram
	addq.w #1, a2
	bsr.w skipString
	bne.w invalidProgram
	bsr.w readU16Le
	bne.w invalidProgram
	cmpi.w #TKVM_OPCODE_VERSION_V1, d0
	bne.w invalidProgram
	bsr.w readU16Le
	bne.w invalidProgram
	move.b d0, buffers.ActiveTokenizerVmStartStateLo
	lsr.w #8, d0
	move.b d0, buffers.ActiveTokenizerVmStartStateHi
	bsr.w readU32Le
	bne.w invalidProgram
	tst.l d0
	beq.w invalidProgram
	cmpi.l #buffers.TOKENIZER_VM_STATE_TABLE_CAPACITY, d0
	bhi.w invalidProgram
	move.b d0, buffers.ActiveTokenizerVmStateCountLo
	lsr.l #8, d0
	move.b d0, buffers.ActiveTokenizerVmStateCountHi
	moveq #0, d0
	move.b buffers.ActiveTokenizerVmStateCountLo, d0
	moveq #0, d1
	move.b buffers.ActiveTokenizerVmStateCountHi, d1
	lsl.w #8, d1
	or.w d1, d0
	move.w d0, d7
	lea buffers.ActiveTokenizerVmStateTable, a3
	subq.w #1, d7

skipStateOffsets
	bsr.w readU32Le
	bne.w invalidProgram
	move.l d0, (a3)+
	dbf d7, skipStateOffsets
	bsr.w readU16Le
	bne.w invalidProgram
	cmpi.w #TKVM_STREAM_VERSION_V1, d0
	bne.w invalidProgram
	moveq #1, d0
	bsr.w requireBytes
	bne.w invalidProgram
	moveq #0, d0
	move.b (a2)+, d0
	cmpi.b #TKVM_STREAM_MODE_LINE, d0
	bne.w invalidProgram
	bsr.w readU32Le
	bne.w invalidProgram
	move.l d0, d5
	bsr.w readU32Le
	bne.w invalidProgram
	bsr.w readU32Le
	bne.w invalidProgram
	bsr.w readU32Le
	bne.w invalidProgram
	move.l d0, buffers.ActiveTokenizerVmMaxErrorsPerLine
	lea buffers.ActiveTokenizerVmInvalidCharDiagCode, a3
	lea buffers.ActiveTokenizerVmInvalidCharDiagLen, a1
	bsr.w readStringIntoSlot
	bne.w invalidProgram
	lea buffers.ActiveTokenizerVmUnterminatedStringDiagCode, a3
	lea buffers.ActiveTokenizerVmUnterminatedStringDiagLen, a1
	bsr.w readStringIntoSlot
	bne.w invalidProgram
	lea buffers.ActiveTokenizerVmStepLimitDiagCode, a3
	lea buffers.ActiveTokenizerVmStepLimitDiagLen, a1
	bsr.w readStringIntoSlot
	bne.w invalidProgram
	lea buffers.ActiveTokenizerVmTokenLimitDiagCode, a3
	lea buffers.ActiveTokenizerVmTokenLimitDiagLen, a1
	bsr.w readStringIntoSlot
	bne.w invalidProgram
	lea buffers.ActiveTokenizerVmLexemeLimitDiagCode, a3
	lea buffers.ActiveTokenizerVmLexemeLimitDiagLen, a1
	bsr.w readStringIntoSlot
	bne.w invalidProgram
	lea buffers.ActiveTokenizerVmErrorLimitDiagCode, a3
	lea buffers.ActiveTokenizerVmErrorLimitDiagLen, a1
	bsr.w readStringIntoSlot
	bne.w invalidProgram
	bsr.w readBytesField
	bne.w invalidProgram
	tst.w d3
	beq.w invalidProgram
	moveq #0, d0
	move.b buffers.ActiveTokenizerVmStartStateLo, d0
	moveq #0, d1
	move.b buffers.ActiveTokenizerVmStartStateHi, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	move.b buffers.ActiveTokenizerVmStateCountLo, d1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmStateCountHi, d2
	lsl.w #8, d2
	or.w d2, d1
	cmp.w d1, d0
	bcc.w invalidProgram
	moveq #0, d0
	rts

invalidProgram
	lea InvalidProgramText, a1
	moveq #INVALID_PROGRAM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; readProgram

; Convert a tkvm status/failure code into the tkpkg runtime diagnostic string.
statusMessage	.block
	cmpi.b #runtime.TK_STATUS_NEWLINE_UNSUPPORTED, d0
	beq.s statusNewline
	cmpi.b #runtime.TK_STATUS_STEP_LIMIT_EXCEEDED, d0
	beq.s statusStepLimit
	cmpi.b #runtime.TK_STATUS_TOKEN_OVERFLOW, d0
	beq.s statusTokenOverflow
	cmpi.b #runtime.TK_STATUS_LEXEME_OVERFLOW, d0
	beq.s statusLexemeOverflow
	cmpi.b #runtime.TK_STATUS_VM_FAILURE, d0
	bne.s checkInvalidArgument
	bra.w statusVmFailure
checkInvalidArgument
	cmpi.b #runtime.TK_STATUS_INVALID_ARGUMENT, d0
	bne.s fallbackInvalidProgram
	bra.w statusInvalidArgument
fallbackInvalidProgram
	lea InvalidProgramText, a1
	moveq #INVALID_PROGRAM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

statusNewline
	lea NewlineText, a1
	moveq #NEWLINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

statusStepLimit
	bsr.w beginStatusBuffer
	moveq #2, d0
	bsr.w getDiagCode
	bsr.w appendBytes
	lea StepLimitSuffixText, a1
	moveq #STEP_LIMIT_SUFFIX_LEN, d2
	bsr.w appendBytes
	bra.w finishStatusBuffer

statusTokenOverflow
	bsr.w beginStatusBuffer
	moveq #3, d0
	bsr.w getDiagCode
	bsr.w appendBytes
	lea TokenOverflowSuffixText, a1
	moveq #TOKEN_OVERFLOW_SUFFIX_LEN, d2
	bsr.w appendBytes
	bra.w finishStatusBuffer

statusLexemeOverflow
	bsr.w beginStatusBuffer
	moveq #4, d0
	bsr.w getDiagCode
	bsr.w appendBytes
	lea LexemeOverflowSuffixText, a1
	moveq #LEXEME_OVERFLOW_SUFFIX_LEN, d2
	bsr.w appendBytes
	bra.w finishStatusBuffer

statusVmFailure
	jsr control.tkvmReadLastFailure68000
	cmpi.w #runtime.TK_VM_FAILURE_KIND_FAIL, d0
	beq.w statusVmFailReason
	cmpi.w #runtime.TK_VM_FAILURE_KIND_EMIT_DIAG, d0
	beq.w statusVmEmitDiag
	lea VmFailureFallbackText, a1
	moveq #VM_FAILURE_FALLBACK_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

statusVmFailReason
	bsr.w beginStatusBuffer
	move.l d1, d6
	moveq #0, d0
	bsr.w getDiagCode
	bsr.w appendBytes
	lea VmFailureReasonSuffixText, a1
	moveq #VM_FAILURE_REASON_SUFFIX_LEN, d2
	bsr.w appendBytes
	move.l d6, d0
	bsr.w appendU32
	bra.w finishStatusBuffer

statusVmEmitDiag
	move.l d1, d6
	tst.l buffers.ActiveTokenizerVmMaxErrorsPerLine
	beq.s statusVmDiagBudgetExceeded
	bsr.w beginStatusBuffer
	move.l d6, d0
	bsr.w getDiagCode
	bsr.w appendBytes
	lea VmEmitDiagSuffixText, a1
	moveq #VM_EMIT_DIAG_SUFFIX_LEN, d2
	bsr.w appendBytes
	move.l d6, d0
	bsr.w appendU32
	bra.w finishStatusBuffer

statusVmDiagBudgetExceeded
	bsr.w beginStatusBuffer
	moveq #5, d0
	bsr.w getDiagCode
	bsr.w appendBytes
	lea VmDiagBudgetExceededText, a1
	moveq #VM_DIAG_BUDGET_EXCEEDED_LEN, d2
	bsr.w appendBytes
	bra.w finishStatusBuffer

statusInvalidArgument
	lea InvalidArgumentText, a1
	moveq #INVALID_ARGUMENT_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; statusMessage

beginStatusBuffer	.block
	move.l (sp)+, d0
	suba.l #LOCAL_SIZE, sp
	move.l d0, -(sp)
	lea 4(sp), a4
	clr.l LOCAL_OUTPUT_LEN(a4)
	clr.l LOCAL_OUTPUT_OVERFLOW(a4)
	clr.b buffers.LastErrorBuffer
	rts
	.bend  ; beginStatusBuffer

finishStatusBuffer	.block
	tst.l LOCAL_OUTPUT_OVERFLOW(a4)
	bne.s finishStatusOverflow
	lea buffers.LastErrorBuffer, a1
	move.l LOCAL_OUTPUT_LEN(a4), d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	adda.l #LOCAL_SIZE, sp
	rts

finishStatusOverflow
	lea OutputOverflowText, a1
	moveq #OUTPUT_OVERFLOW_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	adda.l #LOCAL_SIZE, sp
	rts
	.bend  ; finishStatusBuffer

getDiagCode	.block
	cmpi.b #1, d0
	beq.s diagUnterminatedString
	cmpi.b #2, d0
	beq.s diagStepLimit
	cmpi.b #3, d0
	beq.s diagTokenLimit
	cmpi.b #4, d0
	beq.s diagLexemeLimit
	cmpi.b #5, d0
	beq.s diagErrorLimit
	lea buffers.ActiveTokenizerVmInvalidCharDiagCode, a1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmInvalidCharDiagLen, d2
	rts

diagUnterminatedString
	lea buffers.ActiveTokenizerVmUnterminatedStringDiagCode, a1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmUnterminatedStringDiagLen, d2
	rts

diagStepLimit
	lea buffers.ActiveTokenizerVmStepLimitDiagCode, a1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmStepLimitDiagLen, d2
	rts

diagTokenLimit
	lea buffers.ActiveTokenizerVmTokenLimitDiagCode, a1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmTokenLimitDiagLen, d2
	rts

diagLexemeLimit
	lea buffers.ActiveTokenizerVmLexemeLimitDiagCode, a1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmLexemeLimitDiagLen, d2
	rts

diagErrorLimit
	lea buffers.ActiveTokenizerVmErrorLimitDiagCode, a1
	moveq #0, d2
	move.b buffers.ActiveTokenizerVmErrorLimitDiagLen, d2
	rts
	.bend  ; getDiagCode

; Validate tkvm output counts and spans before rendering report bytes.
validateResult	.block
	movem.l d1-d7/a0, -(sp)
	cmp.l #buffers.TOKEN_BUFFER_CAPACITY, d1
	bhi.s validateInvalid
	cmp.l d4, d2
	bhi.s validateInvalid
	cmp.l #buffers.TOKEN_SCRATCH_CAPACITY, d3
	bhi.s validateInvalid
	lea buffers.TokenRecordBuffer, a0
	moveq #0, d5

validateLoop
	cmp.l d1, d5
	bcc.s validateOk
	moveq #0, d0
	move.w (a0), d0
	cmpi.l #runtime.TK_KIND_OP_LT, d0
	bgt.s validateInvalid
	move.l 4(a0), d6
	beq.s validateInvalid
	move.l d4, d7
	addq.l #1, d7
	cmp.l d7, d6
	bhi.s validateInvalid
	move.l 8(a0), d6
	beq.s validateInvalid
	cmp.l d7, d6
	bhi.s validateInvalid
	cmp.l 4(a0), d6
	blt.s validateInvalid
	move.l 12(a0), d6
	cmp.l d3, d6
	bhi.s validateInvalid
	move.l 16(a0), d7
	add.l d6, d7
	cmp.l d3, d7
	bhi.s validateInvalid
	adda.l #buffers.TOKEN_RECORD_SIZE, a0
	addq.l #1, d5
	bra.s validateLoop

validateOk
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0
	rts

validateInvalid
	moveq #1, d0
	movem.l (sp)+, d1-d7/a0
	rts
	.bend  ; validateResult

; Render token records into the line-oriented tkpkg tokenizer service output.
renderOutput	.block
	movem.l d2-d7/a2-a6, -(sp)
	suba.l #LOCAL_SIZE, sp
	lea 0(sp), a4
	move.l d1, d7
	clr.l LOCAL_OUTPUT_LEN(a4)
	clr.l LOCAL_OUTPUT_OVERFLOW(a4)
	move.l d6, LOCAL_RENDER_LINE(a4)
	clr.b buffers.LastErrorBuffer
	moveq #0, d6

renderLoop
	cmp.l d7, d6
	bcc.s renderDone
	move.l d6, d0
	bsr.w recordPtr
	movea.l a0, a5
	moveq #0, d0
	move.w (a5), d0
	move.l 12(a5), d2
	move.l 16(a5), d3
	lea buffers.TokenScratchBuffer, a6
	adda.l d2, a6
	bsr.w appendKindDebug
	bsr.w appendLiteralAt
	move.l LOCAL_RENDER_LINE(a4), d0
	bsr.w appendU32
	bsr.w appendLiteralColon
	move.l 4(a5), d0
	bsr.w appendU32
	bsr.w appendLiteralDash
	move.l 8(a5), d0
	bsr.w appendU32
	bsr.w appendLiteralNewline
	tst.l LOCAL_OUTPUT_OVERFLOW(a4)
	bne.s renderOverflow
	addq.l #1, d6
	bra.s renderLoop

renderDone
	move.l LOCAL_OUTPUT_LEN(a4), d1
	moveq #0, d0
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d2-d7/a2-a6
	rts

renderOverflow
	lea OutputOverflowText, a1
	moveq #OUTPUT_OVERFLOW_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; renderOutput

appendKindDebug	.block
	cmpi.w #runtime.TK_KIND_IDENTIFIER, d0
	beq.w appendIdentifier
	cmpi.w #runtime.TK_KIND_NUMBER, d0
	beq.w appendNumber
	cmpi.w #runtime.TK_KIND_STRING, d0
	beq.w appendString
	cmpi.w #runtime.TK_KIND_COMMA, d0
	beq.w appendBareComma
	cmpi.w #runtime.TK_KIND_COLON, d0
	beq.w appendBareColon
	cmpi.w #runtime.TK_KIND_DOLLAR, d0
	beq.w appendBareDollar
	cmpi.w #runtime.TK_KIND_DOT, d0
	beq.w appendBareDot
	cmpi.w #runtime.TK_KIND_HASH, d0
	beq.w appendBareHash
	cmpi.w #runtime.TK_KIND_QUESTION, d0
	beq.w appendBareQuestion
	cmpi.w #runtime.TK_KIND_OPEN_BRACKET, d0
	beq.w appendBareOpenBracket
	cmpi.w #runtime.TK_KIND_CLOSE_BRACKET, d0
	beq.w appendBareCloseBracket
	cmpi.w #runtime.TK_KIND_OPEN_BRACE, d0
	beq.w appendBareOpenBrace
	cmpi.w #runtime.TK_KIND_CLOSE_BRACE, d0
	beq.w appendBareCloseBrace
	cmpi.w #runtime.TK_KIND_OPEN_PAREN, d0
	beq.w appendBareOpenParen
	cmpi.w #runtime.TK_KIND_CLOSE_PAREN, d0
	beq.w appendBareCloseParen
	bra.w appendOperator

appendIdentifier
	lea IdentifierPrefix, a1
	moveq #11, d2
	bsr.w appendBytes
	movea.l a6, a1
	bsr.w appendQuoted
	bra.w appendLiteralCloseParen

appendNumber
	lea NumberPrefix, a1
	moveq #15, d2
	bsr.w appendBytes
	movea.l a6, a1
	bsr.w appendUpperQuoted
	lea NumberBasePrefix, a1
	moveq #8, d2
	bsr.w appendBytes
	moveq #10, d0
	tst.l d3
	beq.s appendNumberBaseDone
	moveq #0, d1
	move.b (a6), d1
	cmpi.b #'$', d1
	beq.s appendNumberHex
	cmpi.b #'%', d1
	beq.s appendNumberBin
	moveq #0, d2
	movea.l a6, a1
	adda.l d3, a1
	subq.l #1, a1
	move.b (a1), d2
	cmpi.b #'a', d2
	blo.s appendNumberSuffix
	cmpi.b #'z', d2
	bhi.s appendNumberSuffix
	andi.b #$DF, d2
appendNumberSuffix
	cmpi.b #'H', d2
	beq.s appendNumberHex
	cmpi.b #'B', d2
	beq.s appendNumberBin
	cmpi.b #'O', d2
	beq.s appendNumberOct
	cmpi.b #'Q', d2
	bne.s appendNumberBaseDone
appendNumberOct
	moveq #8, d0
	bra.s appendNumberBaseDone
appendNumberHex
	moveq #16, d0
	bra.s appendNumberBaseDone
appendNumberBin
	moveq #2, d0
appendNumberBaseDone
	bsr.w appendU32
	lea StringSuffix, a1
	addq.l #1, a1
	moveq #2, d2
	bsr.w appendBytes
	rts

appendString
	lea StringPrefix, a1
	moveq #14, d2
	bsr.w appendBytes
	movea.l a6, a1
	bsr.w appendStringRaw
	lea StringBytesPrefix, a1
	moveq #10, d2
	bsr.w appendBytes
	movea.l a6, a1
	bsr.w appendByteList
	lea StringSuffix, a1
	moveq #3, d2
	bsr.w appendBytes
	rts

appendBareComma
	lea KindCommaText, a1
	moveq #5, d2
	bra.w appendBytes
appendBareColon
	lea KindColonText, a1
	moveq #5, d2
	bra.w appendBytes
appendBareDollar
	lea KindDollarText, a1
	moveq #6, d2
	bra.w appendBytes
appendBareDot
	lea KindDotText, a1
	moveq #3, d2
	bra.w appendBytes
appendBareHash
	lea KindHashText, a1
	moveq #4, d2
	bra.w appendBytes
appendBareQuestion
	lea KindQuestionText, a1
	moveq #8, d2
	bra.w appendBytes
appendBareOpenBracket
	lea KindOpenBracketText, a1
	moveq #11, d2
	bra.w appendBytes
appendBareCloseBracket
	lea KindCloseBracketText, a1
	moveq #12, d2
	bra.w appendBytes
appendBareOpenBrace
	lea KindOpenBraceText, a1
	moveq #9, d2
	bra.w appendBytes
appendBareCloseBrace
	lea KindCloseBraceText, a1
	moveq #10, d2
	bra.w appendBytes
appendBareOpenParen
	lea KindOpenParenText, a1
	moveq #9, d2
	bra.w appendBytes
appendBareCloseParen
	lea KindCloseParenText, a1
	moveq #10, d2
	bra.w appendBytes

appendOperator
	move.l d0, -(sp)
	lea OperatorPrefix, a1
	moveq #9, d2
	bsr.w appendBytes
	move.l (sp)+, d0
	bsr.w appendOperatorName
	bra.w appendLiteralCloseParen

appendOperatorName
	cmpi.w #runtime.TK_KIND_OP_RANGE, d0
	beq.w opRange
	cmpi.w #runtime.TK_KIND_OP_RANGE_INCLUSIVE, d0
	beq.w opRangeInclusive
	cmpi.w #runtime.TK_KIND_OP_PLUS, d0
	beq.w opPlus
	cmpi.w #runtime.TK_KIND_OP_MINUS, d0
	beq.w opMinus
	cmpi.w #runtime.TK_KIND_OP_MULTIPLY, d0
	beq.w opMultiply
	cmpi.w #runtime.TK_KIND_OP_POWER, d0
	beq.w opPower
	cmpi.w #runtime.TK_KIND_OP_DIVIDE, d0
	beq.w opDivide
	cmpi.w #runtime.TK_KIND_OP_MOD, d0
	beq.w opMod
	cmpi.w #runtime.TK_KIND_OP_SHL, d0
	beq.w opShl
	cmpi.w #runtime.TK_KIND_OP_SHR, d0
	beq.w opShr
	cmpi.w #runtime.TK_KIND_OP_BIT_NOT, d0
	beq.w opBitNot
	cmpi.w #runtime.TK_KIND_OP_LOGIC_NOT, d0
	beq.w opLogicNot
	cmpi.w #runtime.TK_KIND_OP_BIT_AND, d0
	beq.w opBitAnd
	cmpi.w #runtime.TK_KIND_OP_BIT_OR, d0
	beq.w opBitOr
	cmpi.w #runtime.TK_KIND_OP_BIT_XOR, d0
	beq.w opBitXor
	cmpi.w #runtime.TK_KIND_OP_LOGIC_AND, d0
	beq.w opLogicAnd
	cmpi.w #runtime.TK_KIND_OP_LOGIC_OR, d0
	beq.w opLogicOr
	cmpi.w #runtime.TK_KIND_OP_LOGIC_XOR, d0
	beq.w opLogicXor
	cmpi.w #runtime.TK_KIND_OP_EQ, d0
	beq.w opEq
	cmpi.w #runtime.TK_KIND_OP_NE, d0
	beq.w opNe
	cmpi.w #runtime.TK_KIND_OP_GE, d0
	beq.w opGe
	cmpi.w #runtime.TK_KIND_OP_GT, d0
	beq.w opGt
	cmpi.w #runtime.TK_KIND_OP_LE, d0
	beq.w opLe
	lea OpLtText, a1
	moveq #2, d2
	bra.w appendBytes
opRange
	lea OpRangeText, a1
	moveq #5, d2
	bra.w appendBytes
opRangeInclusive
	lea OpRangeInclusiveText, a1
	moveq #14, d2
	bra.w appendBytes
opPlus
	lea OpPlusText, a1
	moveq #4, d2
	bra.w appendBytes
opMinus
	lea OpMinusText, a1
	moveq #5, d2
	bra.w appendBytes
opMultiply
	lea OpMultiplyText, a1
	moveq #8, d2
	bra.w appendBytes
opPower
	lea OpPowerText, a1
	moveq #5, d2
	bra.w appendBytes
opDivide
	lea OpDivideText, a1
	moveq #6, d2
	bra.w appendBytes
opMod
	lea OpModText, a1
	moveq #3, d2
	bra.w appendBytes
opShl
	lea OpShlText, a1
	moveq #3, d2
	bra.w appendBytes
opShr
	lea OpShrText, a1
	moveq #3, d2
	bra.w appendBytes
opBitNot
	lea OpBitNotText, a1
	moveq #6, d2
	bra.w appendBytes
opLogicNot
	lea OpLogicNotText, a1
	moveq #8, d2
	bra.w appendBytes
opBitAnd
	lea OpBitAndText, a1
	moveq #6, d2
	bra.w appendBytes
opBitOr
	lea OpBitOrText, a1
	moveq #5, d2
	bra.w appendBytes
opBitXor
	lea OpBitXorText, a1
	moveq #6, d2
	bra.w appendBytes
opLogicAnd
	lea OpLogicAndText, a1
	moveq #8, d2
	bra.w appendBytes
opLogicOr
	lea OpLogicOrText, a1
	moveq #7, d2
	bra.w appendBytes
opLogicXor
	lea OpLogicXorText, a1
	moveq #8, d2
	bra.w appendBytes
opEq
	lea OpEqText, a1
	moveq #2, d2
	bra.w appendBytes
opNe
	lea OpNeText, a1
	moveq #2, d2
	bra.w appendBytes
opGe
	lea OpGeText, a1
	moveq #2, d2
	bra.w appendBytes
opGt
	lea OpGtText, a1
	moveq #2, d2
	bra.w appendBytes
opLe
	lea OpLeText, a1
	moveq #2, d2
	bra.w appendBytes
	.bend  ; appendKindDebug

appendQuoted	.block
	movem.l d0-d1/d5, -(sp)
	moveq #'"', d0
	bsr.w appendChar
	move.l d3, d5
	beq.s quotedClose

quotedLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w appendEscapedChar
	subq.l #1, d5
	bne.s quotedLoop

quotedClose
	moveq #'"', d0
	bsr.w appendChar
	movem.l (sp)+, d0-d1/d5
	rts
	.bend  ; appendQuoted

appendUpperQuoted	.block
	movem.l d0-d1/d5, -(sp)
	moveq #'"', d0
	bsr.w appendChar
	move.l d3, d5
	beq.s upperQuotedClose

upperQuotedLoop
	moveq #0, d0
	move.b (a1)+, d0
	cmpi.b #'a', d0
	blo.s upperQuotedEmit
	cmpi.b #'z', d0
	bhi.s upperQuotedEmit
	andi.b #$DF, d0
upperQuotedEmit
	bsr.w appendEscapedChar
	subq.l #1, d5
	bne.s upperQuotedLoop

upperQuotedClose
	moveq #'"', d0
	bsr.w appendChar
	movem.l (sp)+, d0-d1/d5
	rts
	.bend  ; appendUpperQuoted

appendStringRaw	.block
	movem.l d0-d1/d5/a1, -(sp)
	moveq #'"', d0
	bsr.w appendChar
	moveq #'"', d0
	bsr.w appendEscapedChar
	move.l d3, d5
	beq.s stringRawClose

stringRawLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w appendEscapedChar
	subq.l #1, d5
	bne.s stringRawLoop

stringRawClose
	moveq #'"', d0
	bsr.w appendEscapedChar
	moveq #'"', d0
	bsr.w appendChar
	movem.l (sp)+, d0-d1/d5/a1
	rts
	.bend  ; appendStringRaw

appendByteList	.block
	movem.l d0-d1/d5/a1, -(sp)
	move.l d3, d5
	beq.s byteListDone

byteListLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w appendU32
	subq.l #1, d5
	beq.s byteListDone
	move.l a1, -(sp)
	lea CommaSpaceText, a1
	moveq #2, d2
	bsr.w appendBytes
	movea.l (sp)+, a1
	bra.s byteListLoop

byteListDone
	movem.l (sp)+, d0-d1/d5/a1
	rts
	.bend  ; appendByteList

appendEscapedChar	.block
	cmpi.b #'\\', d0
	beq.s escapeBackslash
	cmpi.b #'"', d0
	beq.s escapeQuote
	cmpi.b #10, d0
	beq.s escapeLf
	cmpi.b #13, d0
	beq.s escapeCr
	cmpi.b #9, d0
	beq.s escapeTab
	bra.w appendChar

escapeBackslash
	moveq #'\\', d0
	bsr.w appendChar
	moveq #'\\', d0
	bra.w appendChar

escapeQuote
	moveq #'\\', d0
	bsr.w appendChar
	moveq #'"', d0
	bra.w appendChar

escapeLf
	moveq #'\\', d0
	bsr.w appendChar
	moveq #'n', d0
	bra.w appendChar

escapeCr
	moveq #'\\', d0
	bsr.w appendChar
	moveq #'r', d0
	bra.w appendChar

escapeTab
	moveq #'\\', d0
	bsr.w appendChar
	moveq #'t', d0
	bra.w appendChar
	.bend  ; appendEscapedChar

appendU32	.block
	movem.l d1-d5/a1, -(sp)
	lea DecimalPowers, a1
	clr.l d4
	moveq #9, d5

appendU32Loop
	move.l (a1)+, d2
	moveq #0, d3

appendDigitCount
	cmp.l d2, d0
	blo.s appendDigitReady
	sub.l d2, d0
	addq.b #1, d3
	bra.s appendDigitCount

appendDigitReady
	tst.l d4
	bne.s appendDigitEmit
	tst.b d3
	bne.s appendDigitStart
	tst.w d5
	bne.s appendDigitSkip

appendDigitStart
	moveq #1, d4

appendDigitEmit
	move.l d0, -(sp)
	moveq #'0', d4
	add.b d3, d4
	move.l d4, d0
	bsr.w appendChar
	move.l (sp)+, d0

appendDigitSkip
	dbf d5, appendU32Loop
	movem.l (sp)+, d1-d5/a1
	rts
	.bend  ; appendU32

appendBytes	.block
	tst.w d2
	beq.s appendBytesDone

appendBytesLoop
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w appendChar
	subq.w #1, d2
	bne.s appendBytesLoop

appendBytesDone
	rts
	.bend  ; appendBytes

appendChar	.block
	move.l a1, -(sp)
	move.l LOCAL_OUTPUT_LEN(a4), d1
	cmpi.l #buffers.LAST_ERROR_BUFFER_CAPACITY - 1, d1
	bcs.s appendCharStore
	moveq #1, d1
	move.l d1, LOCAL_OUTPUT_OVERFLOW(a4)
	movea.l (sp)+, a1
	rts

appendCharStore
	lea buffers.LastErrorBuffer, a1
	move.b d0, 0(a1, d1.l)
	addq.l #1, d1
	move.l d1, LOCAL_OUTPUT_LEN(a4)
	clr.b 0(a1, d1.l)
	movea.l (sp)+, a1
	rts
	.bend  ; appendChar

numberBase	.block
	moveq #10, d0
	tst.l d3
	beq.s numberBaseDone
	moveq #0, d1
	move.b (a1), d1
	cmpi.b #'$', d1
	beq.s numberBaseHex
	cmpi.b #'%', d1
	beq.s numberBaseBin
	moveq #0, d2
	movea.l a1, a0
	adda.l d3, a0
	subq.l #1, a0
	move.b (a0), d2
	cmpi.b #'a', d2
	blo.s numberBaseSuffix
	cmpi.b #'z', d2
	bhi.s numberBaseSuffix
	andi.b #$DF, d2

numberBaseSuffix
	cmpi.b #'H', d2
	beq.s numberBaseHex
	cmpi.b #'B', d2
	beq.s numberBaseBin
	cmpi.b #'O', d2
	beq.s numberBaseOct
	cmpi.b #'Q', d2
	beq.s numberBaseOct
	bra.s numberBaseDone

numberBaseHex
	moveq #16, d0
	rts
numberBaseBin
	moveq #2, d0
	rts
numberBaseOct
	moveq #8, d0
numberBaseDone
	rts
	.bend  ; numberBase

recordPtr	.block
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
	lea buffers.TokenRecordBuffer, a1
	adda.l a0, a1
	movea.l a1, a0
	rts
	.bend  ; recordPtr

appendLiteralAt	.block
	lea AtDelimiterText, a1
	moveq #1, d2
	bra.w appendBytes
	.bend  ; appendLiteralAt

appendLiteralColon	.block
	lea ColonDelimiterText, a1
	moveq #1, d2
	bra.w appendBytes
	.bend  ; appendLiteralColon

appendLiteralDash	.block
	lea DashDelimiterText, a1
	moveq #1, d2
	bra.w appendBytes
	.bend  ; appendLiteralDash

appendLiteralNewline	.block
	lea NewlineDelimiterText, a1
	moveq #1, d2
	bra.w appendBytes
	.bend  ; appendLiteralNewline

appendLiteralCloseParen	.block
	lea CloseParenText, a1
	moveq #1, d2
	bra.w appendBytes
	.bend  ; appendLiteralCloseParen

; Skip one length-prefixed string at A2.
; Inputs: A2 = current package cursor; A6 = exclusive package end.
; Outputs: A2 advanced past the string; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/CCR.
; CCR: reflects D1 on return.
skipString	.block
	bsr.w readU32Le
	bne.s skipStringDone
	bsr.w requireBytes
	bne.s skipStringDone
	adda.l d0, a2
skipStringDone
	rts
	.bend  ; skipString

; Copy one length-prefixed string into the caller-owned diagnostic slot.
; Inputs: A1 = destination length byte; A3 = destination text buffer; A2/A6 = package cursor/end.
; Outputs: A2 advanced past the encoded string; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D2/A0/CCR.
; CCR: reflects D1 on return.
readStringIntoSlot	.block
	bsr.w readU32Le
	bne.s readStringDone
	bsr.w requireBytes
	bne.s readStringDone
	move.l d0, d2
	cmpi.l #31, d2
	bls.s readStringLenReady
	moveq #31, d2
readStringLenReady
	move.b d2, (a1)
	move.l d2, d1
	movea.l a2, a0
readStringCopyLoop
	tst.l d1
	beq.s readStringCopyDone
	move.b (a0)+, (a3)+
	subq.l #1, d1
	bra.s readStringCopyLoop
readStringCopyDone
	clr.b (a3)
	adda.l d0, a2
	moveq #0, d1
readStringDone
	rts
	.bend  ; readStringIntoSlot

; Expose one length-prefixed byte field from the package record.
; Inputs: A2/A6 = package cursor/end.
; Outputs: A3 = field bytes; D3 = field length; A2 advanced past the field; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/D3/A3/CCR.
; CCR: reflects D1 on return.
readBytesField	.block
	bsr.w readU32Le
	bne.s readBytesDone
	bsr.w requireBytes
	bne.s readBytesDone
	movea.l a2, a3
	move.l d0, d3
	adda.l d0, a2
	moveq #0, d1
readBytesDone
	rts
	.bend  ; readBytesField

; Read one little-endian u16 from the package record.
; Inputs: A2 = current package cursor; A6 = exclusive package end.
; Outputs: D0 = decoded value; A2 advanced by 2 on success; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/CCR.
; CCR: reflects D1 on return.
readU16Le	.block
	moveq #2, d0
	bsr.w requireBytes
	bne.s readU16Done
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
readU16Done
	rts
	.bend  ; readU16Le

; Read one little-endian u32 from the package record.
; Inputs: A2 = current package cursor; A6 = exclusive package end.
; Outputs: D0 = decoded value; A2 advanced by 4 on success; D1 = 0 on success, 1 on bounds failure.
; Clobbers: D0-D1/CCR.
; CCR: reflects D1 on return.
readU32Le	.block
	moveq #4, d0
	bsr.w requireBytes
	bne.s readU32Done
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
readU32Done
	rts
	.bend  ; readU32Le

; Verify that D0 bytes remain between A2 and the exclusive end pointer in A6.
; Inputs: D0 = required byte count; A2 = current read cursor; A6 = exclusive end.
; Outputs: D1 = 0 when enough bytes remain, 1 on bounds failure.
; Clobbers: D1/CCR.
; CCR: reflects D1 on return.
requireBytes	.block
	cmpa.l a6, a2
	bhi.s requireBytesFail
	move.l a6, d1
	sub.l a2, d1
	cmp.l d1, d0
	bhi.s requireBytesFail
	moveq #0, d1
	rts

requireBytesFail
	moveq #1, d1
	rts
	.bend  ; requireBytes

	.endsection
	.endmodule
