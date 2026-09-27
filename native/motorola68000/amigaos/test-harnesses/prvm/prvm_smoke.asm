; FS-UAE-friendly native smoke executable for the PRVM runtime.

	.module main
	.cpu 68020
	.use prvm.amigaos.abi as prvm_abi
	.use prvm.amigaos.runtime

SYS_BASE                        = 4
RETURN_OK                       = 0
RETURN_FAIL                     = 20

OPEN_LIBRARY                    = -552
CLOSE_LIBRARY                   = -414
PUT_STR                         = -948

PRVM_RESUME_STATE_SIZE          = runtime.PRVM_RESUME_STATE_SIZE
PRVM_RESUME_CHECKPOINT_DEPTH    = runtime.PRVM_RESUME_LOCAL_STATE + runtime.LOCAL_CHECKPOINT_DEPTH
PRVM_RESUME_CHECKPOINT_COUNT    = runtime.PRVM_RESUME_LOCAL_STATE + runtime.LOCAL_CHECKPOINT_STACK + 4

PRVM_RESUME_MAGIC               = $50525253
PRVM_NATIVE_EXPR_STATE_READY    = 1
PRVM_NATIVE_EXPR_KIND_IMM_DEC   = 1

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
PRVM_SMOKE_PROGRAM_LEN          = 59

	.section entry, kind=code
	.pub

start	.block
	moveq #RETURN_FAIL, d7

	lea DosName(PC), a1
	moveq #36, d0
	movea.l SYS_BASE.W, a6
	jsr OPEN_LIBRARY(a6)

	tst.l d0
	bne.s haveDos

	lea DosName(PC), a1
	moveq #0, d0
	movea.l SYS_BASE.W, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done

haveDos
	movea.l d0, a5
	lea StartedText(PC), a1
	move.l a1, d1
	bsr.w putStr

	bsr.w buildRequestFrame
	lea RequestFrame(PC), a0
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l

	lea SmokeStatus(PC), a0
	move.l d0, 0(a0)
	move.l d1, 4(a0)
	move.l d2, 8(a0)
	move.l d3, 12(a0)

	bsr.w validateExprRequest
	bne.s reportFailure

	bsr.w serviceExprRequest
	bne.s reportFailure

	lea RequestFrame(PC), a0
	move.w #prvm_abi.PRVM_CALL_MODE_RESUME, 8(a0)
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l

	lea SmokeStatus(PC), a0
	move.l d0, 0(a0)
	move.l d1, 4(a0)
	move.l d2, 8(a0)
	move.l d3, 12(a0)

	bsr.w validateResult
	bne.s reportFailure

	bsr.w runBoundaryCases
	bne.w reportFailure
	lea SuccessText(PC), a1
	move.l a1, d1
	bsr.w putStr
	moveq #RETURN_OK, d7
	bra.s closeDos

reportFailure
	move.l a1, d1
	bsr.w putStr

closeDos
	movea.l a5, a1
	movea.l SYS_BASE.W, a6
	jsr CLOSE_LIBRARY(a6)

done
	move.l d7, d0
	rts

putStr
	movea.l a5, a6
	jsr PUT_STR(a6)
	rts

buildRequestFrame
	lea RequestFrame(PC), a0
	move.l #prvm_abi.PRVM_MAGIC_OPRP, 0(a0)
	move.w #prvm_abi.PRVM_ABI_VERSION_V1, 4(a0)
	move.w #prvm_abi.PRVM_REQUEST_FRAME_SIZE, 6(a0)
	move.w #prvm_abi.PRVM_CALL_MODE_START, 8(a0)
	move.w #prvm_abi.PRVM_ENTRY_KIND_OPASM_STATEMENT, 10(a0)
	move.l #1, 12(a0)
	lea SourceLine(PC), a1
	move.l a1, 16(a0)
	move.l #14, 20(a0)
	lea TokenRecord(PC), a1
	move.l a1, 24(a0)
	move.l #4, 28(a0)
	move.w #prvm_abi.PRVM_TOKEN_RECORD_SIZE, 32(a0)
	clr.w 34(a0)
	lea LexemeBytes(PC), a1
	move.l a1, 36(a0)
	move.l #11, 40(a0)
	lea ParserProgram(PC), a1
	move.l a1, 44(a0)
	move.l #PRVM_SMOKE_PROGRAM_LEN, 48(a0)
	lea ResultBuffer(PC), a1
	move.l a1, 52(a0)
	move.l #256, 56(a0)
	lea DiagnosticBuffer(PC), a1
	move.l a1, 60(a0)
	move.l #32, 64(a0)
	lea ResumeBuffer(PC), a1
	move.l a1, 68(a0)
	move.l #PRVM_RESUME_STATE_SIZE, 72(a0)
	lea ExprRequestBuffer(PC), a1
	move.l a1, 76(a0)
	move.l #32, 80(a0)
	lea ExprResultBuffer(PC), a1
	move.l a1, 84(a0)
	move.l #1, 88(a0)
	move.l #prvm_abi.PRVM_PARSER_CONTRACT_VERSION_V2, 92(a0)
	move.l #64, 96(a0)
	clr.l 100(a0)
	clr.l 104(a0)
	clr.l 108(a0)
	rts

; Build the native expression result slot expected by the resume run.
; Inputs: ExprRequestBuffer = pending PRVM expression request.
; Outputs: D0.L = 0 on success, 1 on invalid request; NativeExprSlotTable/ExprResultBuffer updated on success.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0.L on return.
serviceExprRequest
	lea ExprRequestBuffer(PC), a2
	move.l 12(a2), d0
	move.l 16(a2), d1
	move.l d1, d2
	sub.l d0, d2
	cmpi.l #1, d2
	bne.w invalidExprService
	bsr.w tokenPtrByIndex
	bne.w invalidExprService
	move.l 16(a0), d4
	cmpi.l #2, d4
	blt.w invalidExprService
	move.l 12(a0), d2
	lea LexemeBytes(PC), a3
	adda.l d2, a3
	cmpi.b #"#", (a3)+
	bne.w invalidExprService
	subq.l #1, d4
	clr.l d5

parseDecimalLoop
	tst.l d4
	beq.s parsedImmediate
	moveq #0, d0
	move.b (a3)+, d0
	cmpi.b #"0", d0
	bcs.w invalidExprService
	cmpi.b #"9", d0
	bhi.w invalidExprService
	subi.b #"0", d0
	moveq #10, d6
	mulu.w d6, d5
	add.l d0, d5
	subq.l #1, d4
	bra.s parseDecimalLoop

parsedImmediate
	bsr.w writeNativeExprSlot
	lea ExprResultBuffer(PC), a1
	move.w #1, 0(a1)
	clr.w 2(a1)
	move.l 8(a2), 4(a1)
	move.l 20(a2), 8(a1)
	move.l 4(a0), 12(a1)
	move.l 8(a0), 16(a1)
	clr.l 20(a1)
	move.l #$FFFFFFFF, 24(a1)
	clr.l 28(a1)
	clr.l d0
	rts

tokenPtrByIndex
	cmpi.l #4, d0
	bcc.s tokenPtrInvalid
	move.l d0, d2
	lsl.l #4, d2
	move.l d0, d3
	lsl.l #2, d3
	add.l d3, d2
	lea TokenRecord(PC), a0
	adda.l d2, a0
	clr.l d0
	rts

tokenPtrInvalid
	moveq #1, d0
	rts

writeNativeExprSlot
	lea NativeExprSlotTable(PC), a1
	move.w #PRVM_NATIVE_EXPR_STATE_READY, 0(a1)
	move.w #PRVM_NATIVE_EXPR_KIND_IMM_DEC, 2(a1)
	move.l 8(a2), 4(a1)
	move.l d5, 8(a1)
	move.l 20(a2), 12(a1)
	move.l 4(a0), 16(a1)
	move.l 8(a0), 20(a1)
	move.l 12(a0), 24(a1)
	move.l 16(a0), 28(a1)
	rts

invalidExprService
	lea FailureExprServiceText(PC), a1
	moveq #1, d0
	rts

; Validate the first-run expression-request stop surface.
; Inputs: SmokeStatus = runtime status tuple; ExprRequestBuffer = emitted expression request.
; Outputs: D0.L = 0 on success, 1 on mismatch; A1 = failure text on mismatch.
; Clobbers: D0/A0-A1/CCR.
; CCR: reflects D0.L on return.
validateExprRequest
	lea SmokeStatus(PC), a1
	lea ExprRequestBuffer(PC), a0
	cmpi.l #prvm_abi.PRVM_STATUS_EXPR_REQUEST, 0(a1)
	bne.w invalidStatus
	tst.l 4(a1)
	bne.w invalidExprSlot
	cmpi.l #3, 8(a1)
	bne.w invalidExprCursor
	cmpi.l #PRVM_RESUME_STATE_SIZE, 12(a1)
	bne.w invalidExprResumeBytes
	cmpi.w #prvm_abi.PRVM_EXPR_REQUEST_VERSION_V2, 0(a0)
	bne.w invalidExprRequest
	cmpi.w #prvm_abi.PRVM_EXPR_REQUEST_MODE_DYNAMIC, 2(a0)
	bne.w invalidExprRequest
	tst.l 4(a0)
	bne.w invalidExprOperand
	tst.l 8(a0)
	bne.w invalidExprSlot
	cmpi.l #3, 12(a0)
	bne.w invalidExprStart
	cmpi.l #4, 16(a0)
	bne.w invalidExprEnd
	cmpi.l #1, 20(a0)
	bne.w invalidExprBoundary
	cmpi.l #12, 24(a0)
	bne.w invalidExprBoundary
	cmpi.l #15, 28(a0)
	bne.w invalidExprBoundary
	lea ResumeBuffer(PC), a0
	cmpi.l #PRVM_RESUME_MAGIC, 0(a0)
	bne.w invalidResume
	clr.l d0
	rts

validateResult
	lea SmokeStatus(PC), a1
	lea ResultBuffer(PC), a0
	cmpi.l #prvm_abi.PRVM_STATUS_OK, 0(a1)
	bne.w invalidStatus
	cmpi.l #6, 4(a1)
	bne.w invalidCount
	cmpi.l #3, 8(a1)
	bne.w invalidCursor
	cmpi.l #192, 12(a1)
	bne.w invalidBytes
	cmpi.w #prvm_abi.PRVM_RESULT_BEGIN_STATEMENT, 0(a0)
	bne.w invalidBegin
	cmpi.w #prvm_abi.PRVM_RESULT_LABEL_TEXT, 32(a0)
	bne.w invalidLabel
	cmpi.l #1, 40(a0)
	bne.w invalidLabel
	cmpi.l #6, 44(a0)
	bne.w invalidLabel
	tst.l 48(a0)
	bne.w invalidLabel
	cmpi.l #5, 52(a0)
	bne.w invalidLabel
	cmpi.w #prvm_abi.PRVM_RESULT_MNEMONIC_TEXT, 64(a0)
	bne.w invalidMnemonic
	cmpi.l #8, 72(a0)
	bne.w invalidColStart
	cmpi.l #11, 76(a0)
	bne.w invalidColEnd
	cmpi.l #5, 80(a0)
	bne.w invalidLexemeOffset
	cmpi.l #3, 84(a0)
	bne.w invalidLexemeLen
	cmpi.w #prvm_abi.PRVM_RESULT_OPERAND_TEXT, 96(a0)
	bne.w invalidOperand
	cmpi.l #12, 104(a0)
	bne.w invalidOperand
	cmpi.l #15, 108(a0)
	bne.w invalidOperand
	cmpi.l #3, 112(a0)
	bne.w invalidExprOperand
	cmpi.l #4, 116(a0)
	bne.w invalidExprEnd
	cmpi.w #prvm_abi.PRVM_RESULT_OPERAND_EXPR_SLOT, 128(a0)
	bne.w invalidOperand
	cmpi.l #12, 136(a0)
	bne.w invalidOperand
	cmpi.l #15, 140(a0)
	bne.w invalidOperand
	tst.l 144(a0)
	bne.w invalidExprOperand
	tst.l 148(a0)
	bne.w invalidExprSlot
	cmpi.l #3, 152(a0)
	bne.w invalidExprStart
	cmpi.l #4, 156(a0)
	bne.w invalidExprEnd
	cmpi.w #prvm_abi.PRVM_RESULT_FINISH_LINE, 160(a0)
	bne.w invalidFinish
	bsr.w validateNativeExprSlot
	bne.s validateResultReturn
	lea SuccessText(PC), a1
	clr.l d0

validateResultReturn
	rts

; Validate the native expression slot written by serviceExprRequest.
; Inputs: NativeExprSlotTable = expected decimal-immediate expression slot.
; Outputs: D0.L = 0 on success, 1 on mismatch; A1 = failure text on mismatch.
; Clobbers: D0/A0-A1/CCR.
; CCR: reflects D0.L on return.
validateNativeExprSlot
	lea NativeExprSlotTable(PC), a0
	cmpi.w #PRVM_NATIVE_EXPR_STATE_READY, 0(a0)
	bne.w invalidNativeExprSlot
	cmpi.w #PRVM_NATIVE_EXPR_KIND_IMM_DEC, 2(a0)
	bne.w invalidNativeExprKind
	tst.l 4(a0)
	bne.w invalidNativeExprSlot
	cmpi.l #42, 8(a0)
	bne.w invalidNativeExprValue
	cmpi.l #1, 12(a0)
	bne.w invalidNativeExprSlot
	cmpi.l #12, 16(a0)
	bne.w invalidNativeExprSlot
	cmpi.l #15, 20(a0)
	bne.w invalidNativeExprSlot
	cmpi.l #8, 24(a0)
	bne.w invalidNativeExprSlot
	cmpi.l #3, 28(a0)
	bne.w invalidNativeExprSlot
	clr.l d0
	rts

invalidStatus
	bsr.w formatStatus
	cmpi.l #prvm_abi.PRVM_STATUS_EXPR_REQUEST, 0(a1)
	beq.s invalidStatusExpr
	cmpi.l #prvm_abi.PRVM_STATUS_NEWLINE_UNSUPPORTED, 0(a1)
	beq.s invalidStatusNewline
	cmpi.l #prvm_abi.PRVM_STATUS_ENTRY_BOUNDARY, 0(a1)
	beq.s invalidStatusEntry
	cmpi.l #prvm_abi.PRVM_STATUS_INVALID_ARGUMENT, 0(a1)
	beq.s invalidStatusArgument
	cmpi.l #prvm_abi.PRVM_STATUS_INVALID_TOKEN, 0(a1)
	beq.s invalidStatusToken
	cmpi.l #prvm_abi.PRVM_STATUS_INVALID_PROGRAM, 0(a1)
	beq.s invalidStatusProgram
	cmpi.l #prvm_abi.PRVM_STATUS_OUTPUT_OVERFLOW, 0(a1)
	beq.s invalidStatusOverflow
	cmpi.l #prvm_abi.PRVM_STATUS_UNSUPPORTED_OPCODE, 0(a1)
	beq.s invalidStatusOpcode
	cmpi.l #prvm_abi.PRVM_STATUS_INVALID_RESUME, 0(a1)
	beq.s invalidStatusResume
	cmpi.l #prvm_abi.PRVM_STATUS_EXPR_RESULT_INVALID, 0(a1)
	beq.s invalidStatusExprResult
	cmpi.l #prvm_abi.PRVM_STATUS_BUDGET_EXCEEDED, 0(a1)
	beq.s invalidStatusBudget
	lea FailureStatusText(PC), a1
	bra.w invalid

invalidStatusExpr
	lea FailureStatusExprText(PC), a1
	bra.w invalid

invalidStatusNewline
	lea FailureStatusNewlineText(PC), a1
	bra.w invalid

invalidStatusEntry
	lea FailureStatusEntryText(PC), a1
	bra.w invalid

invalidStatusArgument
	lea FailureStatusArgumentText(PC), a1
	bra.w invalid

invalidStatusToken
	lea FailureStatusTokenText(PC), a1
	bra.w invalid

invalidStatusProgram
	lea FailureStatusProgramText(PC), a1
	bra.w invalid

invalidStatusOverflow
	lea FailureStatusOverflowText(PC), a1
	bra.w invalid

invalidStatusOpcode
	lea FailureStatusOpcodeText(PC), a1
	bra.w invalid

invalidStatusResume
	lea FailureStatusResumeText(PC), a1
	bra.w invalid

invalidStatusExprResult
	lea FailureStatusExprResultText(PC), a1
	bra.w invalid

invalidStatusBudget
	lea FailureStatusBudgetText(PC), a1
	bra.w invalid

invalidCount
	lea FailureCountText(PC), a1
	bra.w invalid

invalidCursor
	lea FailureCursorText(PC), a1
	bra.w invalid

invalidBytes
	lea FailureBytesText(PC), a1
	bra.w invalid

invalidBegin
	lea FailureBeginText(PC), a1
	bra.w invalid

invalidMnemonic
	lea FailureMnemonicText(PC), a1
	bra.w invalid

invalidLabel
	lea FailureLabelText(PC), a1
	bra.w invalid

invalidColStart
	lea FailureColStartText(PC), a1
	bra.w invalid

invalidColEnd
	lea FailureColEndText(PC), a1
	bra.w invalid

invalidLexemeOffset
	lea FailureLexemeOffsetText(PC), a1
	bra.w invalid

invalidLexemeLen
	lea FailureLexemeLenText(PC), a1
	bra.w invalid

invalidOperand
	lea FailureOperandText(PC), a1
	bra.w invalid

invalidExprRequest
	lea FailureExprRequestText(PC), a1
	bra.w invalid

invalidExprOperand
	lea FailureExprOperandText(PC), a1
	bra.w invalid

invalidExprSlot
	lea FailureExprSlotText(PC), a1
	bra.w invalid

invalidExprStart
	lea FailureExprStartText(PC), a1
	bra.w invalid

invalidExprEnd
	lea FailureExprEndText(PC), a1
	bra.w invalid

invalidNativeExprSlot
	lea FailureNativeExprSlotText(PC), a1
	bra.w invalid

invalidNativeExprKind
	lea FailureNativeExprKindText(PC), a1
	bra.w invalid

invalidNativeExprValue
	lea FailureNativeExprValueText(PC), a1
	bra.w invalid

invalidExprCursor
	lea FailureExprCursorText(PC), a1
	bra.s invalid

invalidExprResumeBytes
	lea FailureExprResumeBytesText(PC), a1
	bra.s invalid

invalidExprBoundary
	lea FailureExprBoundaryText(PC), a1
	bra.s invalid

invalidResume
	lea FailureResumeText(PC), a1
	bra.s invalid

invalidFinish
	lea FailureFinishText(PC), a1

invalid
	moveq #1, d0
	rts

formatStatus
	move.l 0(a1), d0
	lea FailureStatusHexDigits(PC), a0
	moveq #7, d2

formatStatusLoop
	rol.l #4, d0
	move.l d0, d3
	andi.b #$0F, d3
	cmpi.b #10, d3
	bcs.s formatStatusDigit
	addi.b #7, d3

formatStatusDigit
	addi.b #"0", d3
	move.b d3, (a0)+
	dbra d2, formatStatusLoop
	rts

; Exercise the boundary/resume contract with exact guest assertions. The table
; contains immutable token kinds and expected request ranges, never an oracle
; selected by a fixture name. Error slots must stop further expression requests.
; Outputs: D0 = 0 iff every case passes; A1 = failure text otherwise.
; Clobbers: D0-D6/A0-A4/CCR; preserves A5 (DOS library).
; CCR: reflects D0.
runBoundaryCases	.block
	lea BoundaryCases(PC), a4
nextCase
	tst.l 4(a4)
	beq success
	bsr.w buildRequestFrame
	lea RequestFrame(PC), a0
	lea BoundarySource(PC), a1
	move.l a1, 16(a0)
	move.l #64, 20(a0)
	lea BoundaryTokens(PC), a1
	move.l a1, 24(a0)
	move.l 12(a4), 28(a0)
	movea.l 0(a4), a1
	move.l a1, 44(a0)
	move.l 4(a4), 48(a0)
	lea BoundaryResults(PC), a1
	move.l a1, 52(a0)
	move.l #1024, 56(a0)
	lea BoundaryExprResults(PC), a1
	move.l a1, 84(a0)
	move.l #16, 88(a0)
	move.l #256, 96(a0)
	movea.l 8(a4), a1
	lea BoundaryTokens(PC), a0
	move.l 12(a4), d6
	moveq #1, d5
buildTokens
	tst.l d6
	beq tokensReady
	move.w (a1)+, (a0)+
	clr.w (a0)+
	move.l d5, (a0)+
	addq.l #1, d5
	move.l d5, (a0)+
	clr.l (a0)+
	move.l #1, (a0)+
	subq.l #1, d6
	bra buildTokens
tokensReady
	movea.l 16(a4), a2
	move.l a2, BoundaryExpected
	clr.l BoundaryRequestCount
run
	lea RequestFrame(PC), a0
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l
	cmpi.l #prvm_abi.PRVM_STATUS_EXPR_REQUEST, d0
	beq expressionRequest
	cmpi.l #prvm_abi.PRVM_STATUS_OK, d0
	bne fail
	cmp.l 24(a4), d2
	bne fail
	cmp.l 28(a4), d1
	bne fail
	move.l BoundaryRequestCount, d0
	cmp.l 20(a4), d0
	bne fail
	adda.l #36, a4
	bra nextCase
expressionRequest
	move.l BoundaryRequestCount, d0
	cmp.l 20(a4), d0
	bcc fail
	movea.l BoundaryExpected, a2
	lea ExprRequestBuffer(PC), a0
	cmpi.w #prvm_abi.PRVM_EXPR_REQUEST_VERSION_V2, 0(a0)
	bne fail
	cmpi.w #prvm_abi.PRVM_EXPR_REQUEST_MODE_DYNAMIC, 2(a0)
	bne fail
	move.l (a2)+, d0
	cmp.l 12(a0), d0
	bne fail
	move.l (a2)+, d0
	cmp.l 16(a0), d0
	bne fail
	move.l (a2)+, d0
	cmp.l 4(a0), d0
	bne fail
	move.l (a2)+, d0
	cmp.l 8(a0), d0
	bne fail
	move.l a2, BoundaryExpected
	addq.l #1, BoundaryRequestCount
	move.l 8(a0), d0
	cmpi.l #16, d0
	bcc fail
	lsl.l #5, d0
	lea BoundaryExprResults(PC), a1
	adda.l d0, a1
	move.w #1, 0(a1)
	move.l 12(a0), d0
	cmp.l 16(a0), d0
	bne ready
	tst.l 4(a0)
	beq emptyZero
	move.w #2, 0(a1)
	bra ready
emptyZero
	; The explicit dynamic request mode makes an initial empty range numeric
	; zero. Keep a concrete zero in the harness-owned semantic slot as proof.
	clr.l BoundaryZeroValue
	addq.l #1, BoundaryZeroCount
ready
	tst.l 32(a4)
	beq service
	move.w #2, 0(a1)
service
	clr.w 2(a1)
	move.l 8(a0), 4(a1)
	move.l 20(a0), 8(a1)
	move.l 24(a0), 12(a1)
	move.l 28(a0), 16(a1)
	move.l 8(a0), 20(a1)
	move.l #$FFFFFFFF, 24(a1)
	clr.l 28(a1)
	lea RequestFrame(PC), a0
	move.w #prvm_abi.PRVM_CALL_MODE_RESUME, 8(a0)
	bra run
success
	cmpi.l #5, BoundaryZeroCount
	bne fail
	tst.l BoundaryZeroValue
	bne fail
	bsr.w validateMalformedResumes
	rts
fail
	lea FailureBoundaryText(PC), a1
	moveq #1, d0
	rts
	.bend  ; runBoundaryCases

; Mutate independent fields of a fresh pending resume and require rejection.
; Outputs D0 = zero iff every malformed resume is rejected; A1 failure text.
; Clobbers D0-D6/A0-A4/CCR. Preserves A5 (DOS library).
validateMalformedResumes	.block
	bsr.w buildRequestFrame
	lea RequestFrame(PC), a0
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l
	cmpi.l #prvm_abi.PRVM_STATUS_EXPR_REQUEST, d0
	bne fail
	bsr.w serviceExprRequest
	bne fail
	lea RequestFrame(PC), a0
	move.w #prvm_abi.PRVM_CALL_MODE_RESUME, 8(a0)
	lea ResumeBuffer(PC), a4
	move.w 6(a4), d4
	addq.w #4, 6(a4)
	bsr.w reject
	bne fail
	move.w d4, 6(a4)
	move.l 16(a4), d4
	subq.l #1, 16(a4)
	bsr.w reject
	bne fail
	move.l d4, 16(a4)
	move.l 12(a4), d4
	addq.l #1, 12(a4)
	bsr.w reject
	bne fail
	move.l d4, 12(a4)
	move.l #5, PRVM_RESUME_CHECKPOINT_DEPTH(a4)
	bsr.w reject
	bne fail
	clr.l PRVM_RESUME_CHECKPOINT_DEPTH(a4)
	move.l 24(a4), d4
	move.l #$FFFFFFFF, 24(a4)
	bsr.w reject
	bne fail
	move.l d4, 24(a4)
	lea ExprResultBuffer(PC), a0
	move.w #1, 2(a0)
	bsr.w rejectExpression
	bne fail
	lea ExprResultBuffer(PC), a0
	clr.w 2(a0)
	move.l #1, 4(a0)
	bsr.w rejectExpression
	bne fail
	; A live two-level checkpoint snapshot must reject a saved result count
	; that would overflow result-record address arithmetic after rollback.
	bsr.w buildRequestFrame
	lea RequestFrame(PC), a0
	lea BoundaryProgramNestedCheckpoint(PC), a1
	move.l a1, 44(a0)
	move.l #23, 48(a0)
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l
	cmpi.l #prvm_abi.PRVM_STATUS_EXPR_REQUEST, d0
	bne fail
	lea RequestFrame(PC), a0
	move.w #prvm_abi.PRVM_CALL_MODE_RESUME, 8(a0)
	lea ResumeBuffer(PC), a4
	move.l #$FFFFFFFF, PRVM_RESUME_CHECKPOINT_COUNT(a4)
	bsr.w reject
	bne fail
	clr.l d0
	rts
rejectExpression
	lea RequestFrame(PC), a0
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l
	cmpi.l #prvm_abi.PRVM_STATUS_EXPR_RESULT_INVALID, d0
	bne fail
	clr.l d0
	rts
reject
	lea RequestFrame(PC), a0
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l
	cmpi.l #prvm_abi.PRVM_STATUS_INVALID_RESUME, d0
	bne fail
	clr.l d0
	rts
fail
	lea FailureResumeBoundsText(PC), a1
	moveq #1, d0
	rts
	.bend  ; validateMalformedResumes

DosName
	.byte "dos.library", 0
StartedText
	.byte "OPFORGE-PRVM smoke start", 10, 0
SuccessText
	.byte "OPFORGE-PRVM smoke OK", 10, 0
FailureText
	.byte "OPFORGE-PRVM smoke FAIL", 10, 0
FailureStatusText
	.byte "OPFORGE-PRVM smoke FAIL status $"
FailureStatusHexDigits
	.byte "00000000", 10, 0
FailureStatusExprText
	.byte "OPFORGE-PRVM smoke FAIL status expr", 10, 0
FailureStatusNewlineText
	.byte "OPFORGE-PRVM smoke FAIL status newline", 10, 0
FailureStatusEntryText
	.byte "OPFORGE-PRVM smoke FAIL status entry", 10, 0
FailureStatusArgumentText
	.byte "OPFORGE-PRVM smoke FAIL status argument", 10, 0
FailureStatusTokenText
	.byte "OPFORGE-PRVM smoke FAIL status token", 10, 0
FailureStatusProgramText
	.byte "OPFORGE-PRVM smoke FAIL status program", 10, 0
FailureStatusOverflowText
	.byte "OPFORGE-PRVM smoke FAIL status overflow", 10, 0
FailureStatusOpcodeText
	.byte "OPFORGE-PRVM smoke FAIL status opcode", 10, 0
FailureStatusResumeText
	.byte "OPFORGE-PRVM smoke FAIL status resume", 10, 0
FailureStatusExprResultText
	.byte "OPFORGE-PRVM smoke FAIL status expr-result", 10, 0
FailureStatusBudgetText
	.byte "OPFORGE-PRVM smoke FAIL status budget", 10, 0
FailureCountText
	.byte "OPFORGE-PRVM smoke FAIL count", 10, 0
FailureCursorText
	.byte "OPFORGE-PRVM smoke FAIL cursor", 10, 0
FailureBytesText
	.byte "OPFORGE-PRVM smoke FAIL bytes", 10, 0
FailureBeginText
	.byte "OPFORGE-PRVM smoke FAIL begin", 10, 0
FailureMnemonicText
	.byte "OPFORGE-PRVM smoke FAIL mnemonic", 10, 0
FailureLabelText
	.byte "OPFORGE-PRVM smoke FAIL label", 10, 0
FailureColStartText
	.byte "OPFORGE-PRVM smoke FAIL col-start", 10, 0
FailureColEndText
	.byte "OPFORGE-PRVM smoke FAIL col-end", 10, 0
FailureLexemeOffsetText
	.byte "OPFORGE-PRVM smoke FAIL lexeme-offset", 10, 0
FailureLexemeLenText
	.byte "OPFORGE-PRVM smoke FAIL lexeme-len", 10, 0
FailureOperandText
	.byte "OPFORGE-PRVM smoke FAIL operand", 10, 0
FailureExprRequestText
	.byte "OPFORGE-PRVM smoke FAIL expr-request", 10, 0
FailureExprOperandText
	.byte "OPFORGE-PRVM smoke FAIL expr-operand", 10, 0
FailureExprSlotText
	.byte "OPFORGE-PRVM smoke FAIL expr-slot", 10, 0
FailureExprStartText
	.byte "OPFORGE-PRVM smoke FAIL expr-start", 10, 0
FailureExprEndText
	.byte "OPFORGE-PRVM smoke FAIL expr-end", 10, 0
FailureExprServiceText
	.byte "OPFORGE-PRVM smoke FAIL expr-service", 10, 0
FailureNativeExprSlotText
	.byte "OPFORGE-PRVM smoke FAIL native-expr-slot", 10, 0
FailureNativeExprKindText
	.byte "OPFORGE-PRVM smoke FAIL native-expr-kind", 10, 0
FailureNativeExprValueText
	.byte "OPFORGE-PRVM smoke FAIL native-expr-value", 10, 0
FailureExprCursorText
	.byte "OPFORGE-PRVM smoke FAIL expr-cursor", 10, 0
FailureExprResumeBytesText
	.byte "OPFORGE-PRVM smoke FAIL expr-resume-bytes", 10, 0
FailureExprBoundaryText
	.byte "OPFORGE-PRVM smoke FAIL expr-boundary", 10, 0
FailureResumeText
	.byte "OPFORGE-PRVM smoke FAIL resume", 10, 0
FailureBoundaryText
	.byte "OPFORGE-PRVM smoke FAIL boundary", 10, 0
FailureResumeBoundsText
	.byte "OPFORGE-PRVM smoke FAIL resume bounds", 10, 0
FailureFinishText
	.byte "OPFORGE-PRVM smoke FAIL finish", 10, 0

SourceLine
	.byte "start: LDA #42"

LexemeBytes
	.byte "startLDA#42"

ParserProgram
	.byte $60, $40, $13, $03, $08, $00, $64, $00
	.byte $14, $03, $0E, $00, $66, $00
	.byte $15, $03, $24, $00
	.byte $33, $04, ".", "o", "r", "g", $62, $20, $22, $02, $41, $50
	.byte $FF, $FF, $FF, $FF, $64, $00
	.byte $10, $03, $03, $30, $00, $20, $30, $65, $20, $01, $33, $00
	.byte $30, $62, $20, $41, $50, $FF, $FF, $FF, $FF, $64, $00
ParserProgramEnd

TokenRecord
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
	.word 0
	.word 0
	.long 12
	.long 15
	.long 8
	.long 3

BoundaryCases
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsNested, 19
	.long BoundaryRanges0, 4
	.long 0, 10, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsClose, 4
	.long BoundaryRanges1, 1
	.long 0, 4, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsMixed, 4
	.long BoundaryRanges2, 1
	.long 0, 4, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsLeading, 2
	.long BoundaryRanges3, 2
	.long 0, 6, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsConsecutive, 4
	.long BoundaryRanges4, 2
	.long 0, 6, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsTrailing, 2
	.long BoundaryRanges5, 2
	.long 0, 6, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsComma, 1
	.long BoundaryRanges6, 2
	.long 0, 6, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsEmpty, 0
	.long BoundaryRanges7, 0
	.long 0, 2, 0
	.long BoundaryProgramNoScan, 8
	.long BoundaryKindsTwo, 3
	.long BoundaryRanges8, 0
	.long 0, 2, 0
	.long BoundaryProgramCursor, 10
	.long BoundaryKindsTwo, 3
	.long BoundaryRanges9, 2
	.long 1, 6, 0
	.long BoundaryProgramRescan, 11
	.long BoundaryKindsTwo, 3
	.long BoundaryRanges10, 2
	.long 1, 6, 0
	.long BoundaryProgramRollback, 13
	.long BoundaryKindsTwo, 3
	.long BoundaryRanges11, 2
	.long 0, 6, 0
	.long BoundaryProgramBegin, 9
	.long BoundaryKindsTwo, 3
	.long BoundaryRanges12, 2
	.long 0, 6, 0
	.long BoundaryProgramRepeat, 14
	.long BoundaryKindsLeading, 2
	.long BoundaryRanges13, 4
	.long 0, 10, 0
	.long BoundaryProgramNestedCheckpoint, 23
	.long BoundaryKindsTwo, 3
	.long BoundaryRanges14, 6
	.long 0, 6, 0
	.long BoundaryProgramLoadedCheckpoint, 16
	.long BoundaryKindsIdentifier, 1
	.long BoundaryRanges15, 1
	.long 0, 3, 0
	.long BoundaryProgramBasic, 9
	.long BoundaryKindsTwo, 3
	.long BoundaryRanges16, 1
	.long 0, 4, 1
	.long 0, 0
BoundaryProgramBasic
	.byte $60, $41, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramNoScan
	.byte $60, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramCursor
	.byte $60, $41, $20, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramRescan
	.byte $60, $41, $20, $41, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramRollback
	.byte $60, $41, $04, $20, $41, $05, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramBegin
	.byte $41, $60, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramRepeat
	.byte $60, $41, $50, $FF, $FF, $FF, $FF, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramNestedCheckpoint
	.byte $60, $41, $04, $04, $50, $FF, $FF, $FF, $FF, $05, $50, $FF, $FF, $FF, $FF, $05, $50, $FF, $FF, $FF, $FF, $64, $00
BoundaryProgramLoadedCheckpoint
	.byte $60, $30, $04, $04, $41, $50, $FF, $FF, $FF, $FF, $05, $62, $05, $62, $64, $00
	.align 2
BoundaryKindsNested
	.word 14, 2, 4, 2, 15, 4, 10, 2, 4, 2, 11, 4, 12, 2, 4, 2, 13, 4, 2
BoundaryKindsClose
	.word 15, 2, 4, 2
BoundaryKindsMixed
	.word 14, 11, 4, 2
BoundaryKindsLeading
	.word 4, 2
BoundaryKindsConsecutive
	.word 2, 4, 4, 2
BoundaryKindsTrailing
	.word 2, 4
BoundaryKindsComma
	.word 4
BoundaryKindsEmpty
BoundaryKindsTwo
	.word 2, 4, 2
BoundaryKindsIdentifier
	.word 0
BoundaryRanges0
	.long 0, 5, 0, 0
	.long 6, 11, 1, 1
	.long 12, 17, 2, 2
	.long 18, 19, 3, 3
BoundaryRanges1
	.long 0, 4, 0, 0
BoundaryRanges2
	.long 0, 4, 0, 0
BoundaryRanges3
	.long 0, 0, 0, 0
	.long 1, 2, 1, 1
BoundaryRanges4
	.long 0, 1, 0, 0
	.long 2, 2, 1, 1
BoundaryRanges5
	.long 0, 1, 0, 0
	.long 2, 2, 1, 1
BoundaryRanges6
	.long 0, 0, 0, 0
	.long 1, 1, 1, 1
BoundaryRanges7
BoundaryRanges8
BoundaryRanges9
	.long 0, 1, 0, 0
	.long 2, 3, 1, 1
BoundaryRanges10
	.long 1, 1, 0, 0
	.long 2, 3, 1, 1
BoundaryRanges11
	.long 0, 1, 0, 0
	.long 2, 3, 1, 1
BoundaryRanges12
	.long 0, 1, 0, 0
	.long 2, 3, 1, 1
BoundaryRanges13
	.long 0, 0, 0, 0
	.long 1, 2, 1, 1
	.long 0, 0, 0, 2
	.long 1, 2, 1, 3
BoundaryRanges14
	.long 0, 1, 0, 0
	.long 2, 3, 1, 1
	.long 0, 1, 0, 0
	.long 2, 3, 1, 1
	.long 0, 1, 0, 0
	.long 2, 3, 1, 1
BoundaryRanges15
	.long 0, 1, 0, 0
BoundaryRanges16
	.long 0, 1, 0, 0
BoundarySource
	.fill byte, 64, 32
	.align 4
BoundaryTokens
	.fill byte, 400, 0
BoundaryResults
	.fill byte, 1024, 0
BoundaryExprResults
	.fill byte, 512, 0
BoundaryExpected
	.long 0
BoundaryRequestCount
	.long 0
BoundaryZeroCount
	.long 0
BoundaryZeroValue
	.long 0

SmokeStatus
	.long 0
SmokeResultCount
	.long 0
SmokeCursor
	.long 0
SmokeResultBytes
	.long 0

RequestFrame
	.fill byte, 112, 0
ResultBuffer
	.fill byte, 256, 0
DiagnosticBuffer
	.fill byte, 32, 0
ResumeBuffer
	.fill byte, PRVM_RESUME_STATE_SIZE, 0
ExprRequestBuffer
	.fill byte, 32, 0
ExprResultBuffer
	.fill byte, 32, 0
NativeExprSlotTable
	.fill byte, 32, 0
	.bend  ; start
	.priv

	.endsection
	.output "build/prvm_smoke.hunk", format=hunk, sections=entry, code
	.endmodule
