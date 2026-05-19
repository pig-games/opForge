; Native parser VM module for the AmigaOS PRVM runtime.

	.module prvm.amigaos.runtime
	.cpu 68020
	.pub

PRVM_MAGIC_OPRP                     = $4F505250
PRVM_REQUEST_FRAME_SIZE             = 112
PRVM_TOKEN_RECORD_SIZE              = 20
PRVM_RESULT_RECORD_SIZE             = 32
PRVM_DEFAULT_STEP_BUDGET            = 256
PRVM_LEXEME_SCRATCH_CAPACITY        = 256

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
PRVM_TOKEN_KIND_OP_PLUS             = 18
PRVM_TOKEN_KIND_OP_MULTIPLY         = 20
PRVM_TOKEN_KIND_OP_EQ               = 34

PRVM_PARSER_KIND_IDENTIFIER         = 1
PRVM_PARSER_KIND_DOT                = 3
PRVM_PARSER_KIND_COLON              = 4
PRVM_PARSER_KIND_OPERATOR           = 5
PRVM_PARSER_KIND_QUESTION           = 6
PRVM_PARSER_KIND_COMMA              = 7

PRVM_OPERATOR_PLUS                  = 1
PRVM_OPERATOR_EQ                    = 2
PRVM_OPERATOR_MULTIPLY              = 3

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
PRVM_OPCODE_CONSUME_OPERATOR        = $22
PRVM_OPCODE_LOAD_IDENTIFIER         = $30
PRVM_OPCODE_LOAD_INLINE_TEXT        = $33
PRVM_OPCODE_PARSE_OPTIONAL_LABEL    = $40
PRVM_OPCODE_SCAN_COMMA_BOUNDARIES   = $41
PRVM_OPCODE_PARSE_OPERAND_EXPR      = $50
PRVM_OPCODE_BEGIN_STATEMENT         = $60
PRVM_OPCODE_SET_MNEMONIC            = $62
PRVM_OPCODE_FINISH_LINE             = $64
PRVM_OPCODE_SET_DOT_MNEMONIC        = $65
PRVM_OPCODE_FINISH_ASSIGNMENT       = $66

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

	.pub

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
prvmRun68000	.block
	movem.l d4-d7/a4-a6, -(sp)
	move.l a0, d1  ; null-check the frame before touching any offset fields
	tst.l d1
	beq.w invalidArgument
	cmpi.l #PRVM_REQUEST_FRAME_SIZE, d0
	blt invalidArgument

	movea.l a0, a4  ; A4 is the stable request-frame base for the runtime run
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
	bne invalidArgumentWithLocals
	cmpi.w #PRVM_ABI_VERSION_V1, PRVM_FRAME_ABI_VERSION(a4)
	bne invalidArgumentWithLocals
	moveq #0, d0
	move.w PRVM_FRAME_FRAME_SIZE(a4), d0
	cmpi.l #PRVM_REQUEST_FRAME_SIZE, d0
	blt invalidArgumentWithLocals
	cmpi.w #PRVM_CALL_MODE_START, PRVM_FRAME_CALL_MODE(a4)
	beq validateEntryKind
	cmpi.w #PRVM_CALL_MODE_RESUME, PRVM_FRAME_CALL_MODE(a4)
	bne invalidArgumentWithLocals
validateEntryKind
	cmpi.w #PRVM_ENTRY_KIND_OPASM_STATEMENT, PRVM_FRAME_ENTRY_KIND(a4)  ; current PRVM slice routes opasm statements only
	bne entryBoundary
	cmpi.w #PRVM_TOKEN_RECORD_SIZE, PRVM_FRAME_TOKEN_RECORD_SIZE(a4)
	bne invalidArgumentWithLocals
	cmpi.l #PRVM_PARSER_CONTRACT_VERSION_V2, PRVM_FRAME_PARSER_CONTRACT_VERSION(a4)  ; keep parser bytecode/result contract explicit
	bne invalidProgramAtCursor
	tst.l PRVM_FRAME_FLAGS(a4)
	bne invalidArgumentWithLocals

	move.l PRVM_FRAME_SOURCE_LEN(a4), d6
	bmi invalidArgumentWithLocals
	beq validateTokenBuffer
	move.l PRVM_FRAME_SOURCE_PTR(a4), d0
	tst.l d0
	beq invalidArgumentWithLocals

validateTokenBuffer
	move.l PRVM_FRAME_TOKEN_COUNT(a4), d4
	bmi invalidArgumentWithLocals
	beq validateLexemeBuffer
	move.l PRVM_FRAME_TOKEN_PTR(a4), d0
	tst.l d0
	beq invalidArgumentWithLocals

validateLexemeBuffer
	move.l PRVM_FRAME_LEXEME_LEN(a4), d0
	bmi invalidArgumentWithLocals
	beq validateProgramBuffer
	move.l PRVM_FRAME_LEXEME_PTR(a4), d7
	tst.l d7
	beq invalidArgumentWithLocals

validateProgramBuffer
	move.l PRVM_FRAME_PROGRAM_LEN(a4), d6
	ble invalidProgramAtCursor
	move.l PRVM_FRAME_PROGRAM_PTR(a4), d0
	tst.l d0
	beq invalidArgumentWithLocals
	move.l PRVM_FRAME_RESULT_PTR(a4), d0
	tst.l d0
	beq invalidArgumentWithLocals
	move.l PRVM_FRAME_RESULT_CAPACITY(a4), d0
	bmi invalidArgumentWithLocals
	move.l PRVM_FRAME_DIAGNOSTIC_PTR(a4), d0
	tst.l d0
	beq invalidArgumentWithLocals
	move.l PRVM_FRAME_RESUME_PTR(a4), d0
	tst.l d0
	beq invalidArgumentWithLocals
	move.l PRVM_FRAME_RESUME_CAPACITY(a4), d0
	cmpi.l #PRVM_RESUME_STATE_SIZE, d0
	blt invalidArgumentWithLocals
	move.l PRVM_FRAME_EXPR_REQUEST_PTR(a4), d0
	tst.l d0
	beq invalidArgumentWithLocals
	move.l PRVM_FRAME_EXPR_REQUEST_SIZE(a4), d0
	cmpi.l #PRVM_EXPR_REQUEST_RECORD_SIZE, d0
	blt invalidArgumentWithLocals
	move.l PRVM_FRAME_EXPR_RESULT_COUNT(a4), d0
	bmi invalidArgumentWithLocals
	beq validateExpressionResultBufferDone
	move.l PRVM_FRAME_EXPR_RESULT_PTR(a4), d7
	tst.l d7
	beq invalidArgumentWithLocals
validateExpressionResultBufferDone

	movea.l PRVM_FRAME_SOURCE_PTR(a4), a0  ; PRVM consumes one logical line; iterator/router split newlines first
	move.l PRVM_FRAME_SOURCE_LEN(a4), d6
	clr.l d0
newlineScanLoop
	cmp.l d6, d0
	bcc newlineScanDone
	cmpi.b #10, 0(a0, d0.l)
	beq newlineUnsupported
	cmpi.b #13, 0(a0, d0.l)
	beq newlineUnsupported
	addq.l #1, d0
	bra newlineScanLoop

newlineUnsupported
	clr.l d1
	move.l d0, d2
	clr.l d3
	moveq #PRVM_STATUS_NEWLINE_UNSUPPORTED, d0
	bra returnWithLocals

newlineScanDone
	movea.l PRVM_FRAME_PROGRAM_PTR(a4), a5
	move.l PRVM_FRAME_PROGRAM_LEN(a4), d6
	lea 0(a5, d6.l), a6
	move.l PRVM_FRAME_STEP_BUDGET(a4), d6
	bgt startProgram
	move.l #PRVM_DEFAULT_STEP_BUDGET, d6

startProgram
	cmpi.w #PRVM_CALL_MODE_RESUME, PRVM_FRAME_CALL_MODE(a4)
	beq resumeFromExpression
	clr.l d1
	clr.l d2
	clr.l d3

programLoop
	move.l LOCAL_STEP_COUNT(a3), d0
	addq.l #1, d0
	move.l d0, LOCAL_STEP_COUNT(a3)
	cmp.l d6, d0
	bhi budgetExceeded
	cmpa.l a6, a5
	bcc invalidProgramAtCursor

	moveq #0, d7
	move.b (a5)+, d7
	cmpi.b #PRVM_OPCODE_END, d7
	beq opcodeEnd
	cmpi.b #PRVM_OPCODE_JUMP, d7
	beq opcodeJump
	cmpi.b #PRVM_OPCODE_JUMP_IF_FALSE, d7
	beq opcodeJumpIfFalse
	cmpi.b #PRVM_OPCODE_CHECKPOINT, d7
	beq opcodeCheckpoint
	cmpi.b #PRVM_OPCODE_ROLLBACK, d7
	beq opcodeRollback
	cmpi.b #PRVM_OPCODE_COMMIT, d7
	beq opcodeCommit
	cmpi.b #PRVM_OPCODE_PEEK_KIND, d7
	beq opcodePeekKind
	cmpi.b #PRVM_OPCODE_IS_EOL, d7
	beq opcodeIsEol
	cmpi.b #PRVM_OPCODE_PEEK_ASSIGNMENT, d7
	beq opcodePeekAssignment
	cmpi.b #PRVM_OPCODE_PEEK_STAR_ORG, d7
	beq opcodePeekStarOrg
	cmpi.b #PRVM_OPCODE_ADVANCE, d7
	beq opcodeAdvance
	cmpi.b #PRVM_OPCODE_CONSUME_OPERATOR, d7
	beq opcodeConsumeOperator
	cmpi.b #PRVM_OPCODE_LOAD_IDENTIFIER, d7
	beq opcodeLoadIdentifier
	cmpi.b #PRVM_OPCODE_LOAD_INLINE_TEXT, d7
	beq opcodeLoadInlineText
	cmpi.b #PRVM_OPCODE_PARSE_OPTIONAL_LABEL, d7
	beq opcodeParseOptionalLabel
	cmpi.b #PRVM_OPCODE_SCAN_COMMA_BOUNDARIES, d7
	beq programLoop
	cmpi.b #PRVM_OPCODE_PARSE_OPERAND_EXPR, d7
	beq opcodeParseOperandExpr
	cmpi.b #PRVM_OPCODE_BEGIN_STATEMENT, d7
	beq opcodeBeginStatement
	cmpi.b #PRVM_OPCODE_SET_MNEMONIC, d7
	beq opcodeSetMnemonic
	cmpi.b #PRVM_OPCODE_FINISH_LINE, d7
	beq opcodeFinishLine
	cmpi.b #PRVM_OPCODE_SET_DOT_MNEMONIC, d7
	beq opcodeSetDotMnemonic
	cmpi.b #PRVM_OPCODE_FINISH_ASSIGNMENT, d7
	beq opcodeFinishAssignment
	bra unsupportedOpcode

opcodeEnd
	tst.l LOCAL_FINISHED_FLAG(a3)
	beq invalidProgramAtCursor
	moveq #PRVM_STATUS_OK, d0
	bra returnWithLocals

opcodeJump
	bsr.w readProgramTarget
	tst.l d0
	bne returnWithLocals
	movea.l d5, a5
	bra programLoop

opcodeJumpIfFalse
	bsr.w readProgramTarget
	tst.l d0
	bne returnWithLocals
	tst.l LOCAL_BOOL_VALUE(a3)
	bne programLoop
	movea.l d5, a5
	bra programLoop

opcodeCheckpoint
	bsr.w pushCheckpoint
	tst.l d0
	bne returnWithLocals
	bra programLoop

opcodeRollback
	bsr.w popCheckpointAddress
	tst.l d0
	bne returnWithLocals
	move.l (a0)+, d2
	move.l (a0)+, d1
	move.l (a0)+, d3
	move.l (a0)+, LOCAL_OPERAND_COUNT(a3)
	move.l (a0)+, LOCAL_FINISHED_FLAG(a3)
	move.l (a0)+, LOCAL_LABEL_FLAG(a3)
	move.l (a0)+, LOCAL_BOOL_VALUE(a3)
	bra programLoop

opcodeCommit
	bsr.w popCheckpointAddress
	tst.l d0
	bne returnWithLocals
	bra programLoop

opcodePeekKind
	cmpa.l a6, a5
	bcc invalidProgramAtCursor
	moveq #0, d0
	move.b (a5)+, d0
	bsr.w peekKind
	move.l d0, LOCAL_BOOL_VALUE(a3)
	bra programLoop

opcodeIsEol
	clr.l LOCAL_BOOL_VALUE(a3)
	cmp.l d4, d2
	bcs programLoop
	move.l #1, LOCAL_BOOL_VALUE(a3)
	bra programLoop

opcodePeekAssignment
	clr.l LOCAL_BOOL_VALUE(a3)
	tst.l LOCAL_LABEL_FLAG(a3)
	beq programLoop
	cmp.l d4, d2
	bcc programLoop
	bsr.w currentTokenPtr
	tst.l d0
	bne returnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_OP_EQ, 0(a1)
	bne programLoop
	move.l #1, LOCAL_BOOL_VALUE(a3)
	bra programLoop

opcodePeekStarOrg
	clr.l LOCAL_BOOL_VALUE(a3)
	tst.l LOCAL_LABEL_FLAG(a3)
	bne programLoop
	move.l d2, d0
	bsr.w tokenPtrByIndex
	tst.l d0
	bne returnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_OP_MULTIPLY, 0(a1)
	bne programLoop
	move.l d2, d0
	addq.l #1, d0
	cmp.l d4, d0
	bcc programLoop
	bsr.w tokenPtrByIndex
	tst.l d0
	bne returnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_OP_EQ, 0(a1)
	bne programLoop
	move.l #1, LOCAL_BOOL_VALUE(a3)
	bra programLoop

opcodeAdvance
	cmp.l d4, d2
	bcc programLoop
	addq.l #1, d2
	bra programLoop

opcodeConsumeOperator
	cmpa.l a6, a5
	bcc invalidProgramAtCursor
	moveq #0, d0
	move.b (a5)+, d0
	bsr.w currentTokenPtr
	tst.l d0
	bne returnWithLocals
	move.w 0(a1), d7
	cmpi.b #PRVM_OPERATOR_PLUS, -1(a5)
	beq consumePlus
	cmpi.b #PRVM_OPERATOR_EQ, -1(a5)
	beq consumeEq
	cmpi.b #PRVM_OPERATOR_MULTIPLY, -1(a5)
	beq consumeMultiply
	bra invalidTokenAtCursor

consumePlus
	cmpi.w #PRVM_TOKEN_KIND_OP_PLUS, d7
	bne invalidTokenAtCursor
	addq.l #1, d2
	bra programLoop

consumeEq
	cmpi.w #PRVM_TOKEN_KIND_OP_EQ, d7
	bne invalidTokenAtCursor
	addq.l #1, d2
	bra programLoop

consumeMultiply
	cmpi.w #PRVM_TOKEN_KIND_OP_MULTIPLY, d7
	bne invalidTokenAtCursor
	addq.l #1, d2
	bra programLoop

opcodeLoadIdentifier
	bsr.w currentTokenPtr
	tst.l d0
	bne returnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_IDENTIFIER, 0(a1)
	bne invalidTokenAtCursor
	move.l 4(a1), d0
	beq invalidTokenAtCursor
	move.l 8(a1), d7
	cmp.l d0, d7
	bcs invalidTokenAtCursor
	move.l 12(a1), d0
	move.l 16(a1), d7
	beq invalidTokenAtCursor
	move.l d0, d5
	add.l d7, d5
	bcs invalidTokenAtCursor
	cmp.l PRVM_FRAME_LEXEME_LEN(a4), d5
	bhi invalidTokenAtCursor
	move.l 4(a1), LOCAL_LOADED_COL_START(a3)
	move.l 8(a1), LOCAL_LOADED_COL_END(a3)
	move.l 12(a1), LOCAL_LOADED_LEXEME_OFFSET(a3)
	move.l 16(a1), LOCAL_LOADED_LEXEME_LEN(a3)
	move.l #1, LOCAL_LOADED_FLAG(a3)
	bra programLoop

opcodeLoadInlineText
	cmpa.l a6, a5
	bcc invalidProgramAtCursor
	moveq #0, d0
	move.b (a5)+, d0
	move.l d0, d7
	movea.l a5, a0
	adda.l d7, a0
	cmpa.l a6, a0
	bhi invalidProgramAtCursor
	move.l PRVM_FRAME_LEXEME_LEN(a4), d5
	move.l d5, d0
	add.l d7, d0
	bcs invalidProgramAtCursor
	cmpi.l #PRVM_LEXEME_SCRATCH_CAPACITY, d0
	bhi invalidProgramAtCursor
	move.l d5, LOCAL_LOADED_LEXEME_OFFSET(a3)
	move.l d7, LOCAL_LOADED_LEXEME_LEN(a3)
	clr.l LOCAL_LOADED_COL_START(a3)
	move.l d7, LOCAL_LOADED_COL_END(a3)
	move.l d0, PRVM_FRAME_LEXEME_LEN(a4)
	movea.l PRVM_FRAME_LEXEME_PTR(a4), a0
	adda.l d5, a0
	tst.l d7
	beq loadInlineDone

loadInlineCopyLoop
	move.b (a5)+, (a0)+
	subq.l #1, d7
	bne loadInlineCopyLoop

loadInlineDone
	move.l #1, LOCAL_LOADED_FLAG(a3)
	bra programLoop

opcodeParseOptionalLabel
	tst.l d2
	bne programLoop
	tst.l d4
	beq programLoop
	clr.l d0
	bsr.w tokenPtrByIndex
	tst.l d0
	bne returnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_IDENTIFIER, 0(a1)
	bne programLoop
	cmpi.l #1, 4(a1)
	bne programLoop
	move.l 12(a1), d0
	move.l 16(a1), d7
	beq invalidTokenAtCursor
	move.l d0, d5
	add.l d7, d5
	bcs invalidTokenAtCursor
	cmp.l PRVM_FRAME_LEXEME_LEN(a4), d5
	bhi invalidTokenAtCursor
	move.l 4(a1), LOCAL_LABEL_COL_START(a3)
	move.l 8(a1), LOCAL_LABEL_COL_END(a3)
	move.l 12(a1), LOCAL_LABEL_LEXEME_OFFSET(a3)
	move.l 16(a1), LOCAL_LABEL_LEXEME_LEN(a3)
	move.l #1, LOCAL_LABEL_FLAG(a3)
	moveq #1, d2
	cmpi.l #2, d4
	bcs emitOptionalLabel
	moveq #1, d0
	bsr.w tokenPtrByIndex
	tst.l d0
	bne returnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_COLON, 0(a1)
	bne emitOptionalLabel
	move.l 4(a1), d0
	cmp.l LOCAL_LABEL_COL_END(a3), d0
	bne emitOptionalLabel
	moveq #2, d2

emitOptionalLabel
	bsr.w emitLabelText
	tst.l d0
	bne returnWithLocals
	bra programLoop

opcodeBeginStatement
	clr.l LOCAL_LOADED_FLAG(a3)
	clr.l LOCAL_FINISHED_FLAG(a3)
	clr.l LOCAL_OPERAND_COUNT(a3)
	clr.l LOCAL_LABEL_FLAG(a3)
	bsr.w emitBeginStatement
	tst.l d0
	bne returnWithLocals
	bra programLoop

opcodeSetMnemonic
	tst.l LOCAL_LOADED_FLAG(a3)
	beq invalidProgramAtCursor
	bsr.w emitMnemonicText
	tst.l d0
	bne returnWithLocals
	clr.l LOCAL_LOADED_FLAG(a3)
	bra programLoop

opcodeSetDotMnemonic
	tst.l LOCAL_LOADED_FLAG(a3)
	beq invalidProgramAtCursor
	bsr.w emitDirectiveText
	tst.l d0
	bne returnWithLocals
	clr.l LOCAL_LOADED_FLAG(a3)
	bra programLoop

opcodeFinishLine
	bsr.w emitFinishLine
	tst.l d0
	bne returnWithLocals
	move.l #1, LOCAL_FINISHED_FLAG(a3)
	bra programLoop

opcodeFinishAssignment
	tst.l LOCAL_LABEL_FLAG(a3)
	beq invalidProgramAtCursor
	move.l d4, d2
	bsr.w emitFinishLine
	tst.l d0
	bne returnWithLocals
	move.l #1, LOCAL_FINISHED_FLAG(a3)
	bra programLoop

opcodeParseOperandExpr
	movea.l a5, a0
	adda.l #4, a0
	cmpa.l a6, a0
	bhi invalidProgramAtCursor
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne unsupportedOpcode
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne unsupportedOpcode
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne unsupportedOpcode
	move.b (a5)+, d0
	cmpi.b #$FF, d0
	bne unsupportedOpcode
	cmp.l d4, d2
	bcc programLoop
	bra requestOperandAtCursor

requestOperandAtCursor
	move.l d2, LOCAL_EXPR_START_TOKEN(a3)
	move.l d2, d5
findOperandEndLoop
	cmp.l d4, d5
	bcc operandEndFound
	move.l d5, d0
	bsr.w tokenPtrByIndex
	tst.l d0
	bne returnWithLocals
	cmpi.w #PRVM_TOKEN_KIND_COMMA, 0(a1)
	beq operandEndFound
	addq.l #1, d5
	bra findOperandEndLoop

operandEndFound
	move.l d5, LOCAL_EXPR_END_TOKEN(a3)
	move.l LOCAL_OPERAND_COUNT(a3), d0
	move.l d0, LOCAL_EXPR_SLOT_INDEX(a3)
	bsr.w emitOperandTextSpan
	tst.l d0
	bne returnWithLocals
	bsr.w writeExpressionRequest
	tst.l d0
	bne returnWithLocals
	bsr.w writeResumeState
	tst.l d0
	bne returnWithLocals
	move.l LOCAL_EXPR_SLOT_INDEX(a3), d1
	move.l LOCAL_EXPR_START_TOKEN(a3), d2
	move.l #PRVM_RESUME_STATE_SIZE, d3
	moveq #PRVM_STATUS_EXPR_REQUEST, d0
	bra returnWithLocals

resumeFromExpression
	movea.l PRVM_FRAME_RESUME_PTR(a4), a2
	cmpi.l #PRVM_RESUME_MAGIC, 0(a2)
	bne invalidResume
	cmpi.w #PRVM_RESUME_VERSION, 4(a2)
	bne invalidResume
	cmpi.w #PRVM_RESUME_STATE_SIZE, 6(a2)
	blt invalidResume
	cmpi.l #PRVM_CONTINUATION_PARSE_OPERAND, 8(a2)
	bne invalidResume
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
	bhi invalidResume
	bsr.w validateExpressionResultSlot
	tst.l d0
	bne returnWithLocals
	bsr.w emitOperandExprSlot
	tst.l d0
	bne returnWithLocals
	move.l LOCAL_OPERAND_COUNT(a3), d0
	addq.l #1, d0
	move.l d0, LOCAL_OPERAND_COUNT(a3)
	cmp.l d4, d2
	bcs requestOperandAtCursor
	bra programLoop

entryBoundary
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #PRVM_STATUS_ENTRY_BOUNDARY, d0
	bra returnWithLocals

invalidTokenAtCursor
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_TOKEN, d0
	bra returnWithLocals

invalidProgramAtCursor
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_PROGRAM, d0
	bra returnWithLocals

outputOverflow
	moveq #PRVM_STATUS_OUTPUT_OVERFLOW, d0
	rts

unsupportedOpcode
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_UNSUPPORTED_OPCODE, d0
	bra returnWithLocals

invalidResume
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_RESUME, d0
	bra returnWithLocals

expressionResultInvalid
	move.l LOCAL_EXPR_SLOT_INDEX(a3), d1
	clr.l d3
	moveq #PRVM_STATUS_EXPR_RESULT_INVALID, d0
	bra returnWithLocals

budgetExceeded
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_BUDGET_EXCEEDED, d0
	bra returnWithLocals

invalidArgumentWithLocals
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #PRVM_STATUS_INVALID_ARGUMENT, d0
	bra returnWithLocals

returnWithLocals
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d4-d7/a4-a6
	rts

invalidArgument
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #PRVM_STATUS_INVALID_ARGUMENT, d0
	movem.l (sp)+, d4-d7/a4-a6
	rts
	.bend  ; prvmRun68000
	
	.priv

currentTokenPtr	.block
	move.l d2, d0
	bra tokenPtrByIndex
	.bend  ; currentTokenPtr

tokenPtrByIndex	.block
	cmp.l d4, d0
	bcc invalidToken
	lsl.l #4, d0
	move.l d0, d7
	lsr.l #4, d7
	lsl.l #2, d7
	add.l d7, d0
	movea.l PRVM_FRAME_TOKEN_PTR(a4), a1
	adda.l d0, a1
	clr.l d0
	rts

invalidToken
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_TOKEN, d0
	rts
	.bend  ; tokenPtrByIndex

readProgramTarget	.block
	movea.l a5, a0
	adda.l #2, a0
	cmpa.l a6, a0
	bhi invalidProgram
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
	bhi invalidProgram
	move.l a0, d5
	clr.l d0
	rts

invalidProgram
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; readProgramTarget

pushCheckpoint	.block
	move.l LOCAL_CHECKPOINT_DEPTH(a3), d0
	cmpi.l #LOCAL_CHECKPOINT_MAX_DEPTH, d0
	bcc invalidProgram
	bsr.w checkpointAddressForDepth
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

invalidProgram
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; pushCheckpoint

popCheckpointAddress	.block
	move.l LOCAL_CHECKPOINT_DEPTH(a3), d0
	beq invalidProgram
	subq.l #1, d0
	move.l d0, LOCAL_CHECKPOINT_DEPTH(a3)
	bsr.w checkpointAddressForDepth
	clr.l d0
	rts

invalidProgram
	clr.l d1
	clr.l d3
	moveq #PRVM_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; popCheckpointAddress

checkpointAddressForDepth	.block
	move.l d0, d5
	lsl.l #5, d5
	move.l d0, d7
	lsl.l #2, d7
	sub.l d7, d5
	lea LOCAL_CHECKPOINT_STACK(a3), a0
	adda.l d5, a0
	rts
	.bend  ; checkpointAddressForDepth

peekKind	.block
	cmp.l d4, d2
	bcc false
	move.l d2, d5
	lsl.l #4, d5
	move.l d2, d7
	lsl.l #2, d7
	add.l d7, d5
	movea.l PRVM_FRAME_TOKEN_PTR(a4), a1
	adda.l d5, a1
	cmpi.b #PRVM_PARSER_KIND_IDENTIFIER, d0
	beq identifier
	cmpi.b #PRVM_PARSER_KIND_DOT, d0
	beq dot
	cmpi.b #PRVM_PARSER_KIND_COLON, d0
	beq colon
	cmpi.b #PRVM_PARSER_KIND_OPERATOR, d0
	beq operator
	cmpi.b #PRVM_PARSER_KIND_QUESTION, d0
	beq question
	cmpi.b #PRVM_PARSER_KIND_COMMA, d0
	beq comma
	bra false

identifier
	cmpi.w #PRVM_TOKEN_KIND_IDENTIFIER, 0(a1)
	bne false
	moveq #1, d0
	rts

dot
	cmpi.w #PRVM_TOKEN_KIND_DOT, 0(a1)
	bne false
	moveq #1, d0
	rts

colon
	cmpi.w #PRVM_TOKEN_KIND_COLON, 0(a1)
	bne false
	moveq #1, d0
	rts

operator
	cmpi.w #PRVM_TOKEN_KIND_OP_PLUS, 0(a1)
	blt false
	cmpi.w #PRVM_TOKEN_KIND_OP_EQ, 0(a1)
	bgt false
	moveq #1, d0
	rts

question
	clr.l d0
	rts

comma
	cmpi.w #PRVM_TOKEN_KIND_COMMA, 0(a1)
	bne false
	moveq #1, d0
	rts

false
	clr.l d0
	rts
	.bend  ; peekKind

resultRecordPtr	.block
	move.l d1, d0
	lsl.l #5, d0
	move.l d0, d7
	addi.l #PRVM_RESULT_RECORD_SIZE, d7
	cmp.l PRVM_FRAME_RESULT_CAPACITY(a4), d7
	bhi overflow
	movea.l PRVM_FRAME_RESULT_PTR(a4), a2
	adda.l d0, a2
	clr.l d0
	rts

overflow
	moveq #PRVM_STATUS_OUTPUT_OVERFLOW, d0
	rts
	.bend  ; resultRecordPtr

commitResultRecord	.block
	addq.l #1, d1
	move.l d1, d3
	lsl.l #5, d3
	clr.l d0
	rts
	.bend  ; commitResultRecord

emitBeginStatement	.block
	bsr.w resultRecordPtr
	tst.l d0
	bne return
	move.w #PRVM_RESULT_BEGIN_STATEMENT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	clr.l 8(a2)
	clr.l 12(a2)
	clr.l 16(a2)
	clr.l 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra commitResultRecord

return
	rts
	.bend  ; emitBeginStatement

emitLabelText	.block
	tst.l LOCAL_LABEL_FLAG(a3)
	beq return
	bsr.w resultRecordPtr
	tst.l d0
	bne return
	move.w #PRVM_RESULT_LABEL_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l LOCAL_LABEL_COL_START(a3), 8(a2)
	move.l LOCAL_LABEL_COL_END(a3), 12(a2)
	move.l LOCAL_LABEL_LEXEME_OFFSET(a3), 16(a2)
	move.l LOCAL_LABEL_LEXEME_LEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra commitResultRecord

return
	rts
	.bend  ; emitLabelText

emitMnemonicText	.block
	bsr.w resultRecordPtr
	tst.l d0
	bne return
	move.w #PRVM_RESULT_MNEMONIC_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l LOCAL_LOADED_COL_START(a3), 8(a2)
	move.l LOCAL_LOADED_COL_END(a3), 12(a2)
	move.l LOCAL_LOADED_LEXEME_OFFSET(a3), 16(a2)
	move.l LOCAL_LOADED_LEXEME_LEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra commitResultRecord

return
	rts
	.bend  ; emitMnemonicText

emitDirectiveText	.block
	bsr.w resultRecordPtr
	tst.l d0
	bne return
	move.w #PRVM_RESULT_DIRECTIVE_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l LOCAL_LOADED_COL_START(a3), 8(a2)
	move.l LOCAL_LOADED_COL_END(a3), 12(a2)
	move.l LOCAL_LOADED_LEXEME_OFFSET(a3), 16(a2)
	move.l LOCAL_LOADED_LEXEME_LEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra commitResultRecord

return
	rts
	.bend  ; emitDirectiveText

emitOperandTextSpan	.block
	move.l LOCAL_EXPR_START_TOKEN(a3), d0
	cmp.l LOCAL_EXPR_END_TOKEN(a3), d0
	bcc none
	bsr.w tokenPtrByIndex
	tst.l d0
	bne return
	move.l 4(a1), d5
	move.l LOCAL_EXPR_END_TOKEN(a3), d0
	subq.l #1, d0
	bsr.w tokenPtrByIndex
	tst.l d0
	bne return
	move.l 8(a1), d7
	move.l d5, -(sp)
	move.l d7, -(sp)
	bsr.w resultRecordPtr
	move.l (sp)+, d7
	move.l (sp)+, d5
	tst.l d0
	bne return
	move.w #PRVM_RESULT_OPERAND_TEXT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l d5, 8(a2)
	move.l d7, 12(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 16(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra commitResultRecord

none
	clr.l d0
return
	rts
	.bend  ; emitOperandTextSpan

emitOperandExprSlot	.block
	move.l LOCAL_EXPR_START_TOKEN(a3), d0
	cmp.l LOCAL_EXPR_END_TOKEN(a3), d0
	bcc return
	bsr.w tokenPtrByIndex
	tst.l d0
	bne return
	move.l 4(a1), d5
	move.l LOCAL_EXPR_END_TOKEN(a3), d0
	subq.l #1, d0
	bsr.w tokenPtrByIndex
	tst.l d0
	bne return
	move.l 8(a1), d7
	move.l d5, -(sp)
	move.l d7, -(sp)
	bsr.w resultRecordPtr
	move.l (sp)+, d7
	move.l (sp)+, d5
	tst.l d0
	bne return
	move.w #PRVM_RESULT_OPERAND_EXPR_SLOT, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	move.l d5, 8(a2)
	move.l d7, 12(a2)
	move.l LOCAL_OPERAND_COUNT(a3), 16(a2)
	move.l LOCAL_EXPR_SLOT_INDEX(a3), 20(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 24(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 28(a2)
	bra commitResultRecord

return
	rts
	.bend  ; emitOperandExprSlot

emitFinishLine	.block
	bsr.w resultRecordPtr
	tst.l d0
	bne return
	move.w #PRVM_RESULT_FINISH_LINE, 0(a2)
	clr.w 2(a2)
	move.l PRVM_FRAME_LINE_NUM(a4), 4(a2)
	clr.l 8(a2)
	clr.l 12(a2)
	clr.l 16(a2)
	clr.l 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	bra commitResultRecord

return
	rts
	.bend  ; emitFinishLine

writeExpressionRequest	.block
	movea.l PRVM_FRAME_EXPR_REQUEST_PTR(a4), a2
	move.w #1, 0(a2)
	clr.w 2(a2)
	move.l LOCAL_OPERAND_COUNT(a3), 4(a2)
	move.l LOCAL_EXPR_SLOT_INDEX(a3), 8(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 12(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 16(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), d0
	cmp.l d4, d0
	bcc endSpan
	bsr.w tokenPtrByIndex
	tst.l d0
	bne return
	move.l PRVM_FRAME_LINE_NUM(a4), 20(a2)
	move.l 4(a1), 24(a2)
	move.l 8(a1), 28(a2)
	clr.l d0
	rts

endSpan
	move.l PRVM_FRAME_LINE_NUM(a4), 20(a2)
	clr.l 24(a2)
	clr.l 28(a2)
	clr.l d0
return
	rts
	.bend  ; writeExpressionRequest

writeResumeState	.block
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
	bcc writeResumeCursor
	addq.l #1, d0
writeResumeCursor
	move.l d0, 20(a2)
	move.l d1, 24(a2)
	move.l LOCAL_OPERAND_COUNT(a3), 28(a2)
	move.l LOCAL_EXPR_START_TOKEN(a3), 32(a2)
	move.l LOCAL_EXPR_END_TOKEN(a3), 36(a2)
	clr.l d0
	rts
	.bend  ; writeResumeState

validateExpressionResultSlot	.block
	move.l LOCAL_EXPR_SLOT_INDEX(a3), d0
	cmp.l PRVM_FRAME_EXPR_RESULT_COUNT(a4), d0
	bcc invalid
	lsl.l #5, d0
	movea.l PRVM_FRAME_EXPR_RESULT_PTR(a4), a1
	adda.l d0, a1
	move.w 0(a1), d0
	cmpi.w #PRVM_EXPR_SLOT_READY, d0
	beq ready
	cmpi.w #PRVM_EXPR_SLOT_READY_ERROR, d0
	bne invalid
ready
	tst.w 2(a1)
	bne invalid
	move.l 4(a1), d0
	cmp.l LOCAL_EXPR_SLOT_INDEX(a3), d0
	bne invalid
	cmpi.l #$FFFFFFFF, 24(a1)
	bne invalid
	tst.l 28(a1)
	bne invalid
	clr.l d0
	rts

invalid
	move.l LOCAL_EXPR_SLOT_INDEX(a3), d1
	clr.l d3
	moveq #PRVM_STATUS_EXPR_RESULT_INVALID, d0
	rts
	.bend  ; validateExpressionResultSlot

	.endsection
	.endmodule
