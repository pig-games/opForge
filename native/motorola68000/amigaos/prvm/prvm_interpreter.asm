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
PRVM_RESULT_DIRECTIVE_TEXT          = 6
PRVM_RESULT_OPERAND_TEXT            = 7

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
PRVM_OPCODE_SET_DOT_MNEMONIC        = $65

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

AbiMarker
	.byte "OPFORGE-PRVM-ABI-V1", 0

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

prvmRun68000
	movem.l d4-d7/a4-a6, -(sp)
	move.l a0, d1  ; null-check the frame before touching any offset fields
	tst.l d1
	beq prvmInvalidArgument
	cmpi.l #PRVM_REQUEST_FRAME_SIZE, d0
	blt prvmInvalidArgument

	movea.l a0, a4  ; A4 is the stable request-frame base for the interpreter run
	suba.l #LOCAL_SIZE, sp  ; fixed native frame mirrors Rust parser VM execution state
	lea 0(sp), a3  ; A3 addresses LOCAL_* slots while opcodes consume A0-A2/D0-D3
	clr.l LOCAL_LOADED_FLAG(a3)
	clr.l LOCAL_FINISHED_FLAG(a3)
	clr.l LOCAL_STEP_COUNT(a3)
	clr.l LOCAL_OPERAND_COUNT(a3)
	clr.l LOCAL_LABEL_FLAG(a3)
	clr.l LOCAL_BOOL_VALUE(a3)
	clr.l LOCAL_CHECKPOINT_DEPTH(a3)

	cmpi.l #PRVM_MAGIC_OPRP, PRVM_FRAME_MAGIC(a4)  ; reject frames from another native ABI surface
	bne prvmInvalidArgumentWithLocals
	cmpi.w #PRVM_ABI_VERSION_V1, PRVM_FRAME_ABI_VERSION(a4)
	bne prvmInvalidArgumentWithLocals
	moveq #0, d0
	move.w PRVM_FRAME_FRAME_SIZE(a4), d0
	cmpi.l #PRVM_REQUEST_FRAME_SIZE, d0
	blt prvmInvalidArgumentWithLocals
	cmpi.w #PRVM_CALL_MODE_START, PRVM_FRAME_CALL_MODE(a4)
	beq prvmValidateEntryKind
	cmpi.w #PRVM_CALL_MODE_RESUME, PRVM_FRAME_CALL_MODE(a4)
	bne prvmInvalidArgumentWithLocals
prvmValidateEntryKind
	cmpi.w #PRVM_ENTRY_KIND_OPASM_STATEMENT, PRVM_FRAME_ENTRY_KIND(a4)  ; current PRVM slice routes opasm statements only
	bne prvmEntryBoundary
	cmpi.w #PRVM_TOKEN_RECORD_SIZE, PRVM_FRAME_TOKEN_RECORD_SIZE(a4)
	bne prvmInvalidArgumentWithLocals
	cmpi.l #PRVM_PARSER_CONTRACT_VERSION_V2, PRVM_FRAME_PARSER_CONTRACT_VERSION(a4)  ; keep parser bytecode/result contract explicit
	bne prvmInvalidProgramAtCursor
	tst.l PRVM_FRAME_FLAGS(a4)
	bne prvmInvalidArgumentWithLocals

	move.l PRVM_FRAME_SOURCE_LEN(a4), d6
	bmi prvmInvalidArgumentWithLocals
	beq prvmValidateTokenBuffer
	move.l PRVM_FRAME_SOURCE_PTR(a4), d0
	tst.l d0
	beq prvmInvalidArgumentWithLocals

prvmValidateTokenBuffer
	move.l PRVM_FRAME_TOKEN_COUNT(a4), d4
	bmi prvmInvalidArgumentWithLocals
	beq prvmValidateLexemeBuffer
	move.l PRVM_FRAME_TOKEN_PTR(a4), d0
	tst.l d0
	beq prvmInvalidArgumentWithLocals

prvmValidateLexemeBuffer
	move.l PRVM_FRAME_LEXEME_LEN(a4), d0
	bmi prvmInvalidArgumentWithLocals
	beq prvmValidateProgramBuffer
	move.l PRVM_FRAME_LEXEME_PTR(a4), d7
	tst.l d7
	beq prvmInvalidArgumentWithLocals

prvmValidateProgramBuffer
	move.l PRVM_FRAME_PROGRAM_LEN(a4), d6
	ble prvmInvalidProgramAtCursor
	move.l PRVM_FRAME_PROGRAM_PTR(a4), d0
	tst.l d0
	beq prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_RESULT_PTR(a4), d0
	tst.l d0
	beq prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_RESULT_CAPACITY(a4), d0
	bmi prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_DIAGNOSTIC_PTR(a4), d0
	tst.l d0
	beq prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_RESUME_PTR(a4), d0
	tst.l d0
	beq prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_RESUME_CAPACITY(a4), d0
	cmpi.l #PRVM_RESUME_STATE_SIZE, d0
	blt prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_EXPR_REQUEST_PTR(a4), d0
	tst.l d0
	beq prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_EXPR_REQUEST_SIZE(a4), d0
	cmpi.l #PRVM_EXPR_REQUEST_RECORD_SIZE, d0
	blt prvmInvalidArgumentWithLocals
	move.l PRVM_FRAME_EXPR_RESULT_COUNT(a4), d0
	bmi prvmInvalidArgumentWithLocals
	beq prvmValidateExpressionResultBufferDone
	move.l PRVM_FRAME_EXPR_RESULT_PTR(a4), d7
	tst.l d7
	beq prvmInvalidArgumentWithLocals
prvmValidateExpressionResultBufferDone

	movea.l PRVM_FRAME_SOURCE_PTR(a4), a0  ; PRVM consumes one logical line; iterator/router split newlines first
	move.l PRVM_FRAME_SOURCE_LEN(a4), d6
	clr.l d0
prvmNewlineScanLoop
	cmp.l d6, d0
	bcc prvmNewlineScanDone
	cmpi.b #10, 0(a0, d0.l)
	beq prvmNewlineUnsupported
	cmpi.b #13, 0(a0, d0.l)
	beq prvmNewlineUnsupported
	addq.l #1, d0
	bra prvmNewlineScanLoop

prvmNewlineUnsupported
	clr.l d1
	move.l d0, d2
	clr.l d3
	moveq #PRVM_STATUS_NEWLINE_UNSUPPORTED, d0
	bra prvmReturnWithLocals

prvmNewlineScanDone
	movea.l PRVM_FRAME_PROGRAM_PTR(a4), a5
	move.l PRVM_FRAME_PROGRAM_LEN(a4), d6
	lea 0(a5, d6.l), a6
	move.l PRVM_FRAME_STEP_BUDGET(a4), d6
	bgt prvmStartProgram
	move.l #PRVM_DEFAULT_STEP_BUDGET, d6

prvmStartProgram
	cmpi.w #PRVM_CALL_MODE_RESUME, PRVM_FRAME_CALL_MODE(a4)
	beq prvmResumeFromExpression
	clr.l d1
	clr.l d2
	clr.l d3

prvmProgramLoop
	move.l LOCAL_STEP_COUNT(a3), d0
	addq.l #1, d0
	move.l d0, LOCAL_STEP_COUNT(a3)
	cmp.l d6, d0
	bhi prvmBudgetExceeded
	cmpa.l a6, a5
	bcc prvmInvalidProgramAtCursor

	moveq #0, d7
	move.b (a5)+, d7
	cmpi.b #PRVM_OPCODE_END, d7
	beq prvmOpcodeEnd
	cmpi.b #PRVM_OPCODE_JUMP, d7
	beq prvmOpcodeJump
	cmpi.b #PRVM_OPCODE_JUMP_IF_FALSE, d7
	beq prvmOpcodeJumpIfFalse
	cmpi.b #PRVM_OPCODE_CHECKPOINT, d7
	beq prvmOpcodeCheckpoint
	cmpi.b #PRVM_OPCODE_ROLLBACK, d7
	beq prvmOpcodeRollback
	cmpi.b #PRVM_OPCODE_COMMIT, d7
	beq prvmOpcodeCommit
	cmpi.b #PRVM_OPCODE_PEEK_KIND, d7
	beq prvmOpcodePeekKind
	cmpi.b #PRVM_OPCODE_IS_EOL, d7
	beq prvmOpcodeIsEol
	cmpi.b #PRVM_OPCODE_PEEK_ASSIGNMENT, d7
	beq prvmOpcodePeekAssignment
	cmpi.b #PRVM_OPCODE_PEEK_STAR_ORG, d7
	beq prvmOpcodePeekStarOrg
	cmpi.b #PRVM_OPCODE_ADVANCE, d7
	beq prvmOpcodeAdvance
	cmpi.b #PRVM_OPCODE_LOAD_IDENTIFIER, d7
	beq prvmOpcodeLoadIdentifier
	cmpi.b #PRVM_OPCODE_PARSE_OPTIONAL_LABEL, d7
	beq prvmOpcodeParseOptionalLabel
	cmpi.b #PRVM_OPCODE_SCAN_COMMA_BOUNDARIES, d7
	beq prvmProgramLoop
	cmpi.b #PRVM_OPCODE_PARSE_OPERAND_EXPR, d7
	beq prvmOpcodeParseOperandExpr
	cmpi.b #PRVM_OPCODE_BEGIN_STATEMENT, d7
	beq prvmOpcodeBeginStatement
	cmpi.b #PRVM_OPCODE_SET_MNEMONIC, d7
	beq prvmOpcodeSetMnemonic
	cmpi.b #PRVM_OPCODE_FINISH_LINE, d7
	beq prvmOpcodeFinishLine
	cmpi.b #PRVM_OPCODE_SET_DOT_MNEMONIC, d7
	beq prvmOpcodeSetDotMnemonic
	bra prvmUnsupportedOpcode

prvmOpcodeEnd
	tst.l LOCAL_FINISHED_FLAG(a3)
	beq prvmInvalidProgramAtCursor
	moveq #PRVM_STATUS_OK, d0
	bra prvmReturnWithLocals

prvmOpcodeJump
	bsr.w prvmReadProgramTarget
	tst.l d0
	bne prvmReturnWithLocals
	movea.l d5, a5
	bra prvmProgramLoop

prvmOpcodeJumpIfFalse
	bsr.w prvmReadProgramTarget
	tst.l d0
	bne prvmReturnWithLocals
	tst.l LOCAL_BOOL_VALUE(a3)
	bne prvmProgramLoop
	movea.l d5, a5
	bra prvmProgramLoop

prvmOpcodeCheckpoint
	bsr.w prvmPushCheckpoint
	tst.l d0
	bne prvmReturnWithLocals
	bra prvmProgramLoop

prvmOpcodeRollback
	bsr.w prvmPopCheckpointAddress
	tst.l d0
	bne prvmReturnWithLocals
	move.l (a0)+, d2
	move.l (a0)+, d1
	move.l (a0)+, d3
	move.l (a0)+, LOCAL_OPERAND_COUNT(a3)
	move.l (a0)+, LOCAL_FINISHED_FLAG(a3)
	move.l (a0)+, LOCAL_LABEL_FLAG(a3)
	move.l (a0)+, LOCAL_BOOL_VALUE(a3)
	bra prvmProgramLoop

prvmOpcodeCommit
	bsr.w prvmPopCheckpointAddress
	tst.l d0
	bne prvmReturnWithLocals
	bra prvmProgramLoop

prvmOpcodePeekKind
	cmpa.l a6, a5
	bcc prvmInvalidProgramAtCursor
	moveq #0, d0
	move.b (a5)+, d0
	bsr.w prvmPeekKind
	move.l d0, LOCAL_BOOL_VALUE(a3)
	bra prvmProgramLoop

prvmOpcodeIsEol
	clr.l LOCAL_BOOL_VALUE(a3)
	cmp.l d4, d2
	bcs prvmProgramLoop
	move.l #1, LOCAL_BOOL_VALUE(a3)
	bra prvmProgramLoop

prvmOpcodePeekAssignment
	clr.l LOCAL_BOOL_VALUE(a3)
	bra prvmProgramLoop

prvmOpcodePeekStarOrg
	clr.l LOCAL_BOOL_VALUE(a3)
	bra prvmProgramLoop

prvmOpcodeAdvance
	cmp.l d4, d2
	bcc prvmProgramLoop
	addq.l #1, d2
	bra prvmProgramLoop

prvmOpcodeLoadIdentifier
	bsr.w prvmCurrentTokenPtr
	tst.l d0
	bne prvmReturnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_IDENTIFIER, 0(a1)
	bne prvmInvalidTokenAtCursor
	move.l 4(a1), d0
	beq prvmInvalidTokenAtCursor
	move.l 8(a1), d7
	cmp.l d0, d7
	bcs prvmInvalidTokenAtCursor
	move.l 12(a1), d0
	move.l 16(a1), d7
	beq prvmInvalidTokenAtCursor
	move.l d0, d5
	add.l d7, d5
	bcs prvmInvalidTokenAtCursor
	cmp.l PRVM_FRAME_LEXEME_LEN(a4), d5
	bhi prvmInvalidTokenAtCursor
	move.l 4(a1), LOCAL_LOADED_COL_START(a3)
	move.l 8(a1), LOCAL_LOADED_COL_END(a3)
	move.l 12(a1), LOCAL_LOADED_LEXEME_OFFSET(a3)
	move.l 16(a1), LOCAL_LOADED_LEXEME_LEN(a3)
	move.l #1, LOCAL_LOADED_FLAG(a3)
	bra prvmProgramLoop

prvmOpcodeParseOptionalLabel
	tst.l d2
	bne prvmProgramLoop
	tst.l d4
	beq prvmProgramLoop
	clr.l d0
	bsr.w prvmTokenPtrByIndex
	tst.l d0
	bne prvmReturnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_IDENTIFIER, 0(a1)
	bne prvmProgramLoop
	cmpi.l #1, 4(a1)
	bne prvmProgramLoop
	move.l 12(a1), d0
	move.l 16(a1), d7
	beq prvmInvalidTokenAtCursor
	move.l d0, d5
	add.l d7, d5
	bcs prvmInvalidTokenAtCursor
	cmp.l PRVM_FRAME_LEXEME_LEN(a4), d5
	bhi prvmInvalidTokenAtCursor
	move.l 4(a1), LOCAL_LABEL_COL_START(a3)
	move.l 8(a1), LOCAL_LABEL_COL_END(a3)
	move.l 12(a1), LOCAL_LABEL_LEXEME_OFFSET(a3)
	move.l 16(a1), LOCAL_LABEL_LEXEME_LEN(a3)
	move.l #1, LOCAL_LABEL_FLAG(a3)
	moveq #1, d2
	cmpi.l #2, d4
	bcs prvmEmitOptionalLabel
	moveq #1, d0
	bsr.w prvmTokenPtrByIndex
	tst.l d0
	bne prvmReturnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_COLON, 0(a1)
	bne prvmEmitOptionalLabel
	move.l 4(a1), d0
	cmp.l LOCAL_LABEL_COL_END(a3), d0
	bne prvmEmitOptionalLabel
	moveq #2, d2

prvmEmitOptionalLabel
	bsr.w prvmEmitLabelText
	tst.l d0
	bne prvmReturnWithLocals
	bra prvmProgramLoop

prvmOpcodeBeginStatement
	clr.l LOCAL_LOADED_FLAG(a3)
	clr.l LOCAL_FINISHED_FLAG(a3)
	clr.l LOCAL_OPERAND_COUNT(a3)
	clr.l LOCAL_LABEL_FLAG(a3)
	bsr.w prvmEmitBeginStatement
	tst.l d0
	bne prvmReturnWithLocals
	bra prvmProgramLoop

prvmOpcodeSetMnemonic
	tst.l LOCAL_LOADED_FLAG(a3)
	beq prvmInvalidProgramAtCursor
	bsr.w prvmEmitMnemonicText
	tst.l d0
	bne prvmReturnWithLocals
	clr.l LOCAL_LOADED_FLAG(a3)
	bra prvmProgramLoop

prvmOpcodeSetDotMnemonic
	tst.l LOCAL_LOADED_FLAG(a3)
	beq prvmInvalidProgramAtCursor
	bsr.w prvmEmitDirectiveText
	tst.l d0
	bne prvmReturnWithLocals
	clr.l LOCAL_LOADED_FLAG(a3)
	bra prvmProgramLoop

prvmOpcodeFinishLine
	bsr.w prvmEmitFinishLine
	tst.l d0
	bne prvmReturnWithLocals
	move.l #1, LOCAL_FINISHED_FLAG(a3)
	bra prvmProgramLoop

prvmOpcodeParseOperandExpr
	movea.l a5, a0
	adda.l #4, a0
	cmpa.l a6, a0
	bhi prvmInvalidProgramAtCursor
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne prvmUnsupportedOpcode
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne prvmUnsupportedOpcode
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne prvmUnsupportedOpcode
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne prvmUnsupportedOpcode
	cmp.l d4, d2
	bcc prvmProgramLoop
	bra prvmRequestOperandAtCursor

prvmRequestOperandAtCursor
	move.l d2, LOCAL_EXPR_START_TOKEN(a3)
	move.l d2, d5
prvmFindOperandEndLoop
	cmp.l d4, d5
	bcc prvmOperandEndFound
	move.l d5, d0
	bsr.w prvmTokenPtrByIndex
	tst.l d0
	bne prvmReturnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_COMMA, 0(a1)
	beq prvmOperandEndFound
	addq.l #1, d5
	bra prvmFindOperandEndLoop

prvmOperandEndFound
	move.l d5, LOCAL_EXPR_END_TOKEN(a3)
	move.l LOCAL_OPERAND_COUNT(a3), d0
	move.l d0, LOCAL_EXPR_SLOT_INDEX(a3)
	bsr.w prvmEmitOperandTextSpan
	tst.l d0
	bne prvmReturnWithLocals
	bsr.w prvmWriteExpressionRequest
	tst.l d0
	bne prvmReturnWithLocals
	bsr.w prvmWriteResumeState
	tst.l d0
	bne prvmReturnWithLocals
	move.l LOCAL_EXPR_SLOT_INDEX(a3), d1
	move.l LOCAL_EXPR_START_TOKEN(a3), d2
	move.l #PRVM_RESUME_STATE_SIZE, d3
	moveq #PRVM_STATUS_EXPR_REQUEST, d0
	bra prvmReturnWithLocals

prvmResumeFromExpression
	movea.l PRVM_FRAME_RESUME_PTR(a4), a2
	cmpi.l #PRVM_RESUME_MAGIC, 0(a2)
	bne prvmInvalidResume
	cmpi.w #PRVM_RESUME_VERSION, 4(a2)
	bne prvmInvalidResume
	cmpi.w #PRVM_RESUME_STATE_SIZE, 6(a2)
	blt prvmInvalidResume
	cmpi.l #PRVM_CONTINUATION_PARSE_OPERAND, 8(a2)
	bne prvmInvalidResume
	move.l 12(a2), LOCAL_EXPR_SLOT_INDEX(a3)
	move.l 20(a2), d2
	move.l 24(a2), d1
	move.l 28(a2), LOCAL_OPERAND_COUNT(a3)
	move.l 32(a2), LOCAL_EXPR_START_TOKEN(a3)
	move.l 36(a2), LOCAL_EXPR_END_TOKEN(a3)
	move.l PRVM_FRAME_PROGRAM_PTR(a4), d0
	add.l 16(a2), d0
	movea.l d0, a5
	cmpa.l a6, a5
	bhi prvmInvalidResume
	bsr.w prvmValidateExpressionResultSlot
	tst.l d0
	bne prvmReturnWithLocals
	bsr.w prvmEmitOperandExprSlot
	tst.l d0
	bne prvmReturnWithLocals
	move.l LOCAL_OPERAND_COUNT(a3), d0
	addq.l #1, d0
	move.l d0, LOCAL_OPERAND_COUNT(a3)
	cmp.l d4, d2
	bcs prvmRequestOperandAtCursor
	bra prvmProgramLoop

prvmCurrentTokenPtr
	move.l d2, d0
	bra prvmTokenPtrByIndex

prvmTokenPtrByIndex
	cmp.l d4, d0
	bcc prvmCurrentTokenInvalid
	lsl.l #4, d0
	move.l d0, d7
	lsr.l #4, d7
	lsl.l #2, d7
	add.l d7, d0
	movea.l PRVM_FRAME_TOKEN_PTR(a4), a1
	adda.l d0, a1
	clr.l d0
	rts

prvmReadProgramTarget
	movea.l a5, a0
	adda.l #2, a0
	cmpa.l a6, a0
	bhi prvmInvalidProgramAtCursor
	moveq #0, d5
	move.b (a5)+, d5
	moveq #0, d7
	move.b (a5)+, d7
	lsl.l #8, d7
	or.l d7, d5
	move.l PRVM_FRAME_PROGRAM_PTR(a4), d0
	add.l d5, d0
	movea.l d0, a0
	cmpa.l a6, a0
	bhi prvmInvalidProgramAtCursor
	move.l a0, d5
	clr.l d0
	rts

prvmPushCheckpoint
	move.l LOCAL_CHECKPOINT_DEPTH(a3), d0
	cmpi.l #LOCAL_CHECKPOINT_MAX_DEPTH, d0
	bcc prvmInvalidProgramAtCursor
	bsr.w prvmCheckpointAddressForDepth
	move.l d2, (a0)+
	move.l d1, (a0)+
	move.l d3, (a0)+
	move.l LOCAL_OPERAND_COUNT(a3), (a0)+
	move.l LOCAL_FINISHED_FLAG(a3), (a0)+
	move.l LOCAL_LABEL_FLAG(a3), (a0)+
	move.l LOCAL_BOOL_VALUE(a3), (a0)+
	addq.l #1, LOCAL_CHECKPOINT_DEPTH(a3)
	clr.l d0
	rts

prvmPopCheckpointAddress
	move.l LOCAL_CHECKPOINT_DEPTH(a3), d0
	beq prvmInvalidProgramAtCursor
	subq.l #1, d0
	move.l d0, LOCAL_CHECKPOINT_DEPTH(a3)
	bsr.w prvmCheckpointAddressForDepth
	clr.l d0
	rts

prvmCheckpointAddressForDepth
	move.l d0, d5
	lsl.l #5, d5
	move.l d0, d7
	lsl.l #2, d7
	sub.l d7, d5
	lea LOCAL_CHECKPOINT_STACK(a3), a0
	adda.l d5, a0
	rts

prvmPeekKind
	cmp.l d4, d2
	bcc prvmPeekKindFalse
	move.l d2, d5
	lsl.l #4, d5
	move.l d2, d7
	lsl.l #2, d7
	add.l d7, d5
	movea.l PRVM_FRAME_TOKEN_PTR(a4), a1
	adda.l d5, a1
	cmpi.b #$03, d0
	beq prvmPeekKindDot
	bra prvmPeekKindFalse

prvmPeekKindDot
	cmpi.w #PRVM_TOKEN_KIND_DOT, 0(a1)
	bne prvmPeekKindFalse
	moveq #1, d0
	rts

prvmPeekKindFalse
	clr.l d0
	rts

prvmCurrentTokenInvalid
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_TOKEN, d0
	rts

prvmResultRecordPtr
	move.l d1, d0
	lsl.l #5, d0
	move.l d0, d7
	addi.l #PRVM_RESULT_RECORD_SIZE, d7
	cmp.l PRVM_FRAME_RESULT_CAPACITY(a4), d7
	bhi prvmOutputOverflow
	movea.l PRVM_FRAME_RESULT_PTR(a4), a2
	adda.l d0, a2
	clr.l d0
	rts

prvmCommitResultRecord
	addq.l #1, d1
	move.l d1, d3
	lsl.l #5, d3
	clr.l d0
	rts

prvmEmitBeginStatement
	bsr.w prvmResultRecordPtr
	tst.l d0
	bne prvmEmitRecordReturn
	move.w #PRVM_RESULT_BEGIN_STATEMENT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	clr.l 8(a2)
	clr.l 12(a2)
	clr.l 16(a2)
	clr.l 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra prvmCommitResultRecord

prvmEmitLabelText
	tst.l LOCAL_LABEL_FLAG(a3)
	beq prvmEmitRecordReturn
	bsr.w prvmResultRecordPtr
	tst.l d0
	bne prvmEmitRecordReturn
	move.w #PRVM_RESULT_LABEL_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l LOCAL_LABEL_COL_START(a3), 8(a2)
	move.l LOCAL_LABEL_COL_END(a3), 12(a2)
	move.l LOCAL_LABEL_LEXEME_OFFSET(a3), 16(a2)
	move.l LOCAL_LABEL_LEXEME_LEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra prvmCommitResultRecord

prvmEmitMnemonicText
	bsr.w prvmResultRecordPtr
	tst.l d0
	bne prvmEmitRecordReturn
	move.w #PRVM_RESULT_MNEMONIC_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l LOCAL_LOADED_COL_START(a3), 8(a2)
	move.l LOCAL_LOADED_COL_END(a3), 12(a2)
	move.l LOCAL_LOADED_LEXEME_OFFSET(a3), 16(a2)
	move.l LOCAL_LOADED_LEXEME_LEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra prvmCommitResultRecord

prvmEmitDirectiveText
	bsr.w prvmResultRecordPtr
	tst.l d0
	bne prvmEmitRecordReturn
	move.w #PRVM_RESULT_DIRECTIVE_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l LOCAL_LOADED_COL_START(a3), 8(a2)
	move.l LOCAL_LOADED_COL_END(a3), 12(a2)
	move.l LOCAL_LOADED_LEXEME_OFFSET(a3), 16(a2)
	move.l LOCAL_LOADED_LEXEME_LEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra prvmCommitResultRecord

prvmEmitOperandTextSpan
	move.l LOCAL_EXPR_START_TOKEN(a3), d0
	cmp.l LOCAL_EXPR_END_TOKEN(a3), d0
	bcc prvmEmitOperandTextSpanNone
	bsr.w prvmTokenPtrByIndex
	tst.l d0
	bne prvmEmitRecordReturn
	move.l 4(a1), d5
	move.l LOCAL_EXPR_END_TOKEN(a3), d0
	subq.l #1, d0
	bsr.w prvmTokenPtrByIndex
	tst.l d0
	bne prvmEmitRecordReturn
	move.l 8(a1), d7
	move.l d5, -(sp)
	move.l d7, -(sp)
	bsr.w prvmResultRecordPtr
	move.l (sp)+, d7
	move.l (sp)+, d5
	tst.l d0
	bne prvmEmitRecordReturn
	move.w #PRVM_RESULT_OPERAND_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l d5, 8(a2)
	move.l d7, 12(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 16(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra prvmCommitResultRecord

prvmEmitOperandTextSpanNone
	clr.l d0
	rts

prvmEmitOperandExprSlot
	bsr.w prvmResultRecordPtr
	tst.l d0
	bne prvmEmitRecordReturn
	move.w #PRVM_RESULT_OPERAND_EXPR_SLOT, 0(a2)
	clr.w 2(a2)
	move.l 8(a1), 4(a2)
	move.l 12(a1), 8(a2)
	move.l 16(a1), 12(a2)
	move.l LOCAL_OPERAND_COUNT(a3), 16(a2)
	move.l LOCAL_EXPR_SLOT_INDEX(a3), 20(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 24(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 28(a2)
	bra prvmCommitResultRecord

prvmEmitFinishLine
	bsr.w prvmResultRecordPtr
	tst.l d0
	bne prvmEmitRecordReturn
	move.w #PRVM_RESULT_FINISH_LINE, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	clr.l 8(a2)
	clr.l 12(a2)
	clr.l 16(a2)
	clr.l 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra prvmCommitResultRecord

prvmEmitRecordReturn
	rts

prvmWriteExpressionRequest
	movea.l PRVM_FRAME_EXPR_REQUEST_PTR(a4), a2
	move.w #1, 0(a2)
	clr.w 2(a2)
	move.l LOCAL_OPERAND_COUNT(a3), 4(a2)
	move.l LOCAL_EXPR_SLOT_INDEX(a3), 8(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 12(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 16(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), d0
	cmp.l d4, d0
	bcc prvmWriteExpressionRequestEndSpan
	bsr.w prvmTokenPtrByIndex
	tst.l d0
	bne prvmWriteExpressionRequestReturn
	move.l PRVM_FRAME_LINE_NUM(a4), 20(a2)
	move.l 4(a1), 24(a2)
	move.l 8(a1), 28(a2)
	clr.l d0
	rts

prvmWriteExpressionRequestEndSpan
	move.l PRVM_FRAME_LINE_NUM(a4), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	clr.l d0
prvmWriteExpressionRequestReturn
	rts

prvmWriteResumeState
	movea.l PRVM_FRAME_RESUME_PTR(a4), a2
	move.l #PRVM_RESUME_MAGIC, 0(a2)
	move.w #PRVM_RESUME_VERSION, 4(a2)
	move.w #PRVM_RESUME_STATE_SIZE, 6(a2)
	move.l #PRVM_CONTINUATION_PARSE_OPERAND, 8(a2)
	move.l LOCAL_EXPR_SLOT_INDEX(a3), 12(a2)
	move.l a5, d0
	sub.l PRVM_FRAME_PROGRAM_PTR(a4), d0
	move.l d0, 16(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), d0
	cmp.l d4, d0
	bcc prvmWriteResumeCursor
	addq.l #1, d0
prvmWriteResumeCursor
	move.l d0, 20(a2)
	move.l d1, 24(a2)
	move.l LOCAL_OPERAND_COUNT(a3), 28(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 32(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 36(a2)
	clr.l d0
	rts

prvmValidateExpressionResultSlot
	move.l LOCAL_EXPR_SLOT_INDEX(a3), d0
	cmp.l PRVM_FRAME_EXPR_RESULT_COUNT(a4), d0
	bcc prvmExpressionResultInvalid
	lsl.l #5, d0
	movea.l PRVM_FRAME_EXPR_RESULT_PTR(a4), a1
	adda.l d0, a1
	move.w 0(a1), d0
	cmpi.w #PRVM_EXPR_SLOT_READY, d0
	beq prvmValidateExpressionResultReady
	cmpi.w #PRVM_EXPR_SLOT_READY_ERROR, d0
	bne prvmExpressionResultInvalid
prvmValidateExpressionResultReady
	tst.w 2(a1)
	bne prvmExpressionResultInvalid
	move.l 4(a1), d0
	cmp.l LOCAL_EXPR_SLOT_INDEX(a3), d0
	bne prvmExpressionResultInvalid
	cmpi.l #$FFFFFFFF, 24(a1)
	bne prvmExpressionResultInvalid
	tst.l 28(a1)
	bne prvmExpressionResultInvalid
	clr.l d0
	rts

prvmEntryBoundary
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #PRVM_STATUS_ENTRY_BOUNDARY, d0
	bra prvmReturnWithLocals

prvmInvalidTokenAtCursor
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_TOKEN, d0
	bra prvmReturnWithLocals

prvmInvalidProgramAtCursor
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_PROGRAM, d0
	bra prvmReturnWithLocals

prvmOutputOverflow
	moveq #PRVM_STATUS_OUTPUT_OVERFLOW, d0
	rts

prvmUnsupportedOpcode
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_UNSUPPORTED_OPCODE, d0
	bra prvmReturnWithLocals

prvmInvalidResume
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_RESUME, d0
	bra prvmReturnWithLocals

prvmExpressionResultInvalid
	move.l LOCAL_EXPR_SLOT_INDEX(a3), d1
	clr.l d3
	moveq #PRVM_STATUS_EXPR_RESULT_INVALID, d0
	bra prvmReturnWithLocals

prvmBudgetExceeded
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_BUDGET_EXCEEDED, d0
	bra prvmReturnWithLocals

prvmInvalidArgumentWithLocals
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #PRVM_STATUS_INVALID_ARGUMENT, d0
	bra prvmReturnWithLocals

prvmReturnWithLocals
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d4-d7/a4-a6
	rts

prvmInvalidArgument
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #PRVM_STATUS_INVALID_ARGUMENT, d0
	movem.l (sp)+, d4-d7/a4-a6
	rts

	.endsection
	.endmodule
