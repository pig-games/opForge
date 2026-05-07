; Native tokenizer VM module for the AmigaOS tokvm example.
;
; Owns the runtime-model constants, interpreter loop, demo bytecode, and
; token-shape metadata that the CLI harness imports through `.use`.

	.module tokvm.amigaos.tokenizer_vm
	.cpu 68020
	.pub

; Positive VM statuses mirror the native tokenization result contract.
TK_STATUS_SUCCESS               = 0
TK_STATUS_NEWLINE_UNSUPPORTED   = 1
TK_STATUS_TOKEN_OVERFLOW        = 2
TK_STATUS_LEXEME_OVERFLOW       = 3
TK_STATUS_VM_FAILURE            = 4
TK_STATUS_INVALID_ARGUMENT      = 5
TK_STATUS_INVALID_PROGRAM       = 6
TK_STATUS_STEP_LIMIT_EXCEEDED   = 7

TK_VM_FAILURE_KIND_NONE         = 0
TK_VM_FAILURE_KIND_FAIL         = 1
TK_VM_FAILURE_KIND_EMIT_DIAG    = 2

; Opcode values intentionally match crate::package::TokenizerVmOpcode in
; crates/opforge-package/src/package.rs.
TK_OPCODE_END                   = 0
TK_OPCODE_READ_CHAR             = 1
TK_OPCODE_ADVANCE               = 2
TK_OPCODE_START_LEXEME          = 3
TK_OPCODE_PUSH_CHAR             = 4
TK_OPCODE_EMIT_TOKEN            = 5
TK_OPCODE_SET_STATE             = 6
TK_OPCODE_JUMP                  = 7
TK_OPCODE_JUMP_IF_EOL           = 8
TK_OPCODE_JUMP_IF_BYTE_EQ       = 9
TK_OPCODE_JUMP_IF_CLASS         = 10
TK_OPCODE_FAIL                  = 11
TK_OPCODE_EMIT_DIAG             = 12
TK_OPCODE_SCAN_CORE             = 14
TK_OPCODE_SCAN_IDENTIFIER       = 15
TK_OPCODE_SCAN_NUMBER           = 16
TK_OPCODE_SCAN_STRING           = 17
TK_OPCODE_SCAN_SYMBOL           = 18

; Token kind values are the compact on-wire/native record encoding used by the
; report writer and matched back to PortableTokenKind on the Rust side.
TK_KIND_IDENTIFIER              = 0
TK_KIND_NUMBER                  = 2
TK_KIND_STRING                  = 3
TK_KIND_COMMA                   = 4
TK_KIND_COLON                   = 5
TK_KIND_DOLLAR                  = 6
TK_KIND_DOT                     = 7
TK_KIND_HASH                    = 8
TK_KIND_QUESTION                = 9
TK_KIND_OPEN_BRACKET            = 10
TK_KIND_CLOSE_BRACKET           = 11
TK_KIND_OPEN_BRACE              = 12
TK_KIND_CLOSE_BRACE             = 13
TK_KIND_OPEN_PAREN              = 14
TK_KIND_CLOSE_PAREN             = 15
TK_KIND_OP_RANGE                = 16
TK_KIND_OP_RANGE_INCLUSIVE      = 17
TK_KIND_OP_PLUS                 = 18
TK_KIND_OP_MINUS                = 19
TK_KIND_OP_MULTIPLY             = 20
TK_KIND_OP_POWER                = 21
TK_KIND_OP_DIVIDE               = 22
TK_KIND_OP_MOD                  = 23
TK_KIND_OP_SHL                  = 24
TK_KIND_OP_SHR                  = 25
TK_KIND_OP_BIT_NOT              = 26
TK_KIND_OP_LOGIC_NOT            = 27
TK_KIND_OP_BIT_AND              = 28
TK_KIND_OP_BIT_OR               = 29
TK_KIND_OP_BIT_XOR              = 30
TK_KIND_OP_LOGIC_AND            = 31
TK_KIND_OP_LOGIC_OR             = 32
TK_KIND_OP_LOGIC_XOR            = 33
TK_KIND_OP_EQ                   = 34
TK_KIND_OP_NE                   = 35
TK_KIND_OP_GE                   = 36
TK_KIND_OP_GT                   = 37
TK_KIND_OP_LE                   = 38
TK_KIND_OP_LT                   = 39

; The default demo program currently uses only the same first five character
; classes consumed by vm_char_class_matches in tokenizer_runtime_utils.rs.
TK_CLASS_WHITESPACE             = 1
TK_CLASS_IDENTIFIER_START       = 2
TK_CLASS_IDENTIFIER_CONTINUE    = 3
TK_CLASS_DIGIT                  = 4
TK_CLASS_QUOTE                  = 5

; Program-counter labels for demoProgram. These offsets intentionally match the
; little-endian jump targets emitted by the Rust builder default loop.
DEMO_PC_READ_CHAR               = 0
DEMO_PC_SCAN_SYMBOL             = 36
DEMO_PC_SKIP_WHITESPACE         = 42
DEMO_PC_SCAN_IDENTIFIER         = 48
DEMO_PC_SCAN_NUMBER             = 54
DEMO_PC_SCAN_STRING             = 60
DEMO_PC_FINISH                  = 66

TOKEN_RECORD_SIZE               = 20
SOURCE_BUFFER_CAPACITY          = 1024
TOKEN_BUFFER_CAPACITY           = 64
SCRATCH_BUFFER_CAPACITY         = 1024
TOKVM_DEFAULT_MAX_STEPS_PER_LINE = 2048

; The fixed capacities above intentionally match the AmigaOS host harness so the
; native VM and the CLI/report layer agree on how much source, token, and scratch
; state can be exchanged without any additional negotiation structure.

; tokvm_run_68000 local frame layout.
;
; Call ABI:
; - A0 / D0: source buffer pointer and source byte length
; - A1 / D1: token buffer pointer and token capacity in records
; - A2 / D2: scratch buffer pointer and scratch capacity in bytes
; - A3 / D3: tokenizer bytecode pointer and program length in bytes
;
; Return ABI:
; - D0: TK_STATUS_*
; - D1: emitted token count
; - D2: final source cursor
; - D3: scratch bytes committed into lexemeScratch
LOCAL_CURRENT_BYTE              = 0
LOCAL_PENDING_KIND              = 4
LOCAL_PENDING_START             = 8
LOCAL_PENDING_END               = 12
LOCAL_PENDING_LEX_LEN           = 16
LOCAL_TEMP_U32                  = 20
LOCAL_PROGRAM_COUNTER           = 24
LOCAL_STEP_COUNT                = 28
LOCAL_STEP_LIMIT                = 32
LOCAL_SIZE                      = 36

	.section data, kind=data

TokvmStepBudget
	.long TOKVM_DEFAULT_MAX_STEPS_PER_LINE

DemoStateEntryOffsets
	.long DEMO_PC_READ_CHAR

TokvmProgramStateTablePtr
	.long DemoStateEntryOffsets

TokvmProgramStateCount
	.long 1

TokvmProgramStartState
	.word 0

TokvmLastFailureKind
	.word 0

TokvmLastFailureOperand
	.word 0

	.endsection

	.section code, kind=code

; ---------------------------------------------------------------------------
; Native tokenizer VM interpreter.
;
; This interpreter executes the tokenizer bytecode against one line-buffer-backed
; input stream. Its control flow mirrors the Rust tokenizer VM loop in
; crates/opforge-vm/src/runtime_model_core.rs:
; - validate arguments and stream constraints
; - keep source cursor in D2 and program counter in A0
; - decode opcodes from the bytecode stream in A3..A3+D7
; - dispatch scan/predicate helpers that mirror tokenizer_runtime_utils.rs
; - emit compact native token records into the caller-provided token buffer
; ---------------------------------------------------------------------------

; Override the tokenizer VM step budget for the next runs; nonpositive restores default.
tokvmSetStepBudget68000
	tst.l d0
	bgt.s tokvmSetStepBudgetStore
	move.l #TOKVM_DEFAULT_MAX_STEPS_PER_LINE, d0
tokvmSetStepBudgetStore
	move.l d0, TokvmStepBudget
	rts

; Install a package-provided state table; invalid counts fall back to demo state 0.
tokvmSetProgramStateTable68000
	tst.l d0
	bgt.s tokvmSetProgramStateStore
	lea DemoStateEntryOffsets, a0
	moveq #1, d0
	moveq #0, d1
tokvmSetProgramStateStore
	move.l a0, TokvmProgramStateTablePtr
	move.l d0, TokvmProgramStateCount
	move.w d1, TokvmProgramStartState
	rts

; Return the last explicit VM failure kind/operand captured by tokvm_run_68000.
tokvmReadLastFailure68000
	moveq #0, d0
	move.w TokvmLastFailureKind, d0
	moveq #0, d1
	move.w TokvmLastFailureOperand, d1
	rts

tokvmRun68000
	movem.l d4-d7/a4-a6, -(sp)
	movea.l a2, a6  ; preserve scratch base separately so A2 can become the interpreter-local frame pointer
	movea.l a0, a4  ; source bytes base, equivalent to VmTokenizerInputStream.bytes
	movea.l a1, a5  ; token record output base
	move.l d0, d4  ; source byte length / maximum cursor
	move.l d1, d5  ; token capacity in 20-byte records
	move.l d2, d6  ; scratch capacity in bytes
	move.l d3, d7  ; bytecode length in bytes
	suba.l #LOCAL_SIZE, sp  ; local spill area for pending token metadata and saved PC
	lea 0(sp), a2  ; A2 now points at LOCAL_* slots for the duration of interpretation

	clr.l d1  ; emitted token count starts at 0
	clr.l d2  ; source cursor starts at column 1 / byte 0
	clr.l d3  ; scratch bytes committed starts at 0
	clr.l LOCAL_STEP_COUNT(a2)
	move.l TokvmStepBudget, d0
	move.l d0, LOCAL_STEP_LIMIT(a2)
	moveq #-1, d0  ; sentinel current byte = EOF until ReadChar runs
	move.l d0, LOCAL_CURRENT_BYTE(a2)
	clr.w TokvmLastFailureKind
	clr.w TokvmLastFailureOperand

	tst.l d4  ; reject negative lengths/capacities before dereferencing any caller pointers
	bmi tokvmInvalidArgument
	tst.l d5
	bmi tokvmInvalidArgument
	tst.l d6
	bmi tokvmInvalidArgument
	tst.l d7
	bmi tokvmInvalidArgument

	tst.l d4  ; non-empty source requires a non-null source pointer
	beq tokvmCheckTokenPointer
	move.l a4, d0
	tst.l d0
	beq tokvmInvalidArgument

tokvmCheckTokenPointer
	tst.l d5  ; non-zero token capacity requires a writable token buffer
	beq tokvmCheckScratchPointer
	move.l a5, d0
	tst.l d0
	beq tokvmInvalidArgument

tokvmCheckScratchPointer
	tst.l d6  ; non-zero scratch capacity requires a writable scratch buffer
	beq tokvmCheckProgramPointer
	move.l a6, d0
	tst.l d0
	beq tokvmInvalidArgument

tokvmCheckProgramPointer
	tst.l d7  ; bytecode length 0 cannot encode a valid tokenizer program
	beq tokvmInvalidProgramAtCursor
	move.l a3, d0
	tst.l d0
	beq tokvmInvalidArgument

	moveq #0, d0  ; proactively reject CR/LF because this slice models one line-input stream only
tokvmNewlineScanLoop
	cmp.l d4, d0
	bcc tokvmNewlineScanDone
	cmpi.b #10, 0(a4, d0.l)
	beq tokvmNewlineUnsupported
	cmpi.b #13, 0(a4, d0.l)
	beq tokvmNewlineUnsupported
	addq.l #1, d0
	bra tokvmNewlineScanLoop

tokvmNewlineUnsupported
	move.l d0, d2
	moveq #TK_STATUS_NEWLINE_UNSUPPORTED, d0
	bra tokvmReturn

tokvmNewlineScanDone
	moveq #0, d0
	move.w TokvmProgramStartState, d0
	cmp.l TokvmProgramStateCount, d0
	bcc tokvmInvalidProgramAtCursor
	move.l TokvmProgramStateTablePtr, d1
	tst.l d1
	beq tokvmInvalidProgramAtCursor
	movea.l d1, a1
	move.l 0.W(a1, d0.l*4), d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	lea 0(a3, d0.l), a0
	lea 0(a3, d7.l), a1
	clr.l d1  ; token count must still enter the first loop iteration as 0

; Main bytecode dispatch loop.
; A0 is the native program counter, A1 is the bytecode end pointer, D2 is the
; source cursor, and LOCAL_CURRENT_BYTE stores the last ReadChar result. This
; corresponds directly to the Rust match over TokenizerVmOpcode.
tokvmProgramLoop
	move.l LOCAL_STEP_COUNT(a2), d0
	addq.l #1, d0
	move.l d0, LOCAL_STEP_COUNT(a2)
	cmp.l LOCAL_STEP_LIMIT(a2), d0
	bhi tokvmStepLimitExceeded
	lea 0(a3, d7.l), a1
	cmp.l d4, d2
	bhi tokvmInvalidProgramAtCursor
	cmpa.l a1, a0
	bcc tokvmInvalidProgramAtCursor

tokvmProgramLoopDispatchOpcode
	moveq #0, d0
	move.b (a0)+, d0

	; The native slice only implements opcode values 0..18.
	; Unsupported shared VM slots still get explicit table entries so the
	; opcode-to-handler mapping stays visible and future additions stay local.
	cmpi.b #TK_OPCODE_SCAN_SYMBOL, d0
	bhi tokvmInvalidProgramAtCursor
	add.w d0, d0
	add.w d0, d0
	lea TokvmOpcodeDispatchTable(PC), a1
	movea.l 0(a1, d0.W), a1
	jmp (a1)

TokvmOpcodeDispatchTable
	.long tokvmOpcodeEnd
	.long tokvmOpcodeReadChar
	.long tokvmOpcodeAdvance
	.long tokvmOpcodeStartLexeme
	.long tokvmOpcodePushChar
	.long tokvmOpcodeEmitToken
	.long tokvmOpcodeSetState
	.long tokvmOpcodeJump
	.long tokvmOpcodeJumpIfEol
	.long tokvmOpcodeJumpIfByteEq
	.long tokvmOpcodeJumpIfClass
	.long tokvmOpcodeFail
	.long tokvmOpcodeEmitDiag
	.long tokvmInvalidProgramAtCursor
	.long tokvmInvalidProgramAtCursor
	.long tokvmOpcodeScanIdentifier
	.long tokvmOpcodeScanNumber
	.long tokvmOpcodeScanString
	.long tokvmOpcodeScanSymbol

tokvmOpcodeEnd
	cmp.l d4, d2  ; Rust runtime also only accepts END when the source cursor is at EOL
	bne tokvmInvalidProgramAtCursor
	moveq #TK_STATUS_SUCCESS, d0
	bra tokvmReturn

	; ReadChar mirrors VmTokenizerInputStream.current_byte(): live bytes are zero-extended
	; into D0 and only EOF uses the -1 sentinel stored in LOCAL_CURRENT_BYTE.
tokvmOpcodeReadChar
	moveq #0, d0
	cmp.l d4, d2
	bcc tokvmStoreEofByte
	move.b 0(a4, d2.l), d0
	bra tokvmStoreCurrentByte
tokvmStoreEofByte
	moveq #-1, d0
tokvmStoreCurrentByte
	move.l d0, LOCAL_CURRENT_BYTE(a2)

	bra tokvmProgramLoop

tokvmOpcodeAdvance
	cmp.l d4, d2  ; advance saturates at EOL, same as VmTokenizerInputStream.advance()
	bcc tokvmProgramLoop
	addq.l #1, d2
	bra tokvmProgramLoop

tokvmOpcodeStartLexeme
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	move.l d2, LOCAL_PENDING_START(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	bra tokvmProgramLoop

tokvmOpcodePushChar
	move.l LOCAL_CURRENT_BYTE(a2), d0
	tst.l d0
	bmi tokvmInvalidProgramAtCursor
	move.l d1, LOCAL_TEMP_U32(a2)
	move.l d3, d1
	add.l LOCAL_PENDING_LEX_LEN(a2), d1
	cmp.l d6, d1
	bcc tokvmPendingLexemeOverflow
	movea.l a6, a1
	adda.l d1, a1
	move.b d0, (a1)
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	move.l d2, d1
	addq.l #1, d1
	move.l d1, LOCAL_PENDING_END(a2)
	move.l LOCAL_TEMP_U32(a2), d1
	bra tokvmProgramLoop

tokvmOpcodeEmitToken
	lea 0(a3, d7.l), a1
	cmpa.l a1, a0
	bcc tokvmInvalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, LOCAL_PENDING_KIND(a2)
	jsr tokvmCommitPendingToken
	tst.l d0
	bne tokvmReturn
	bra tokvmProgramLoop

tokvmOpcodeSetState
	move.l a0, d0
	sub.l a3, d0
	addq.l #2, d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	move.l d1, LOCAL_TEMP_U32(a2)
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.w #8, d1
	or.w d1, d0
	cmp.l TokvmProgramStateCount, d0
	bcc tokvmInvalidProgramAtCursor
	move.l TokvmProgramStateTablePtr, d1
	tst.l d1
	beq tokvmInvalidProgramAtCursor
	movea.l d1, a1
	move.l 0.W(a1, d0.l*4), d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	lea 0(a3, d0.l), a0
	move.l LOCAL_TEMP_U32(a2), d1
	bra tokvmProgramLoop

tokvmOpcodeFail
	move.l a0, d0
	sub.l a3, d0
	addq.l #1, d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0
	move.w #TK_VM_FAILURE_KIND_FAIL, TokvmLastFailureKind
	move.w d0, TokvmLastFailureOperand
	bra tokvmVmFailureAtCursor

tokvmOpcodeEmitDiag
	move.l a0, d0
	sub.l a3, d0
	addq.l #1, d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0
	move.w #TK_VM_FAILURE_KIND_EMIT_DIAG, TokvmLastFailureKind
	move.w d0, TokvmLastFailureOperand
	bra tokvmVmFailureAtCursor

tokvmOpcodeJump
	move.l a0, d0
	sub.l a3, d0
	addq.l #4, d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	move.l (a0), d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	adda.l #4, a0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra tokvmProgramLoop

tokvmOpcodeJumpIfEol
	move.l a0, d0
	sub.l a3, d0
	addq.l #4, d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	move.l (a0), d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	adda.l #4, a0
	cmp.l d4, d2
	bne tokvmProgramLoop
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra tokvmProgramLoop

tokvmOpcodeJumpIfByteEq
	move.l a0, d0
	sub.l a3, d0
	addq.l #5, d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0  ; operand 0 = byte literal to compare against LOCAL_CURRENT_BYTE
	move.w d0, LOCAL_PENDING_KIND(a2)
	move.l (a0), d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	adda.l #4, a0
	move.l d0, LOCAL_TEMP_U32(a2)
	move.l LOCAL_CURRENT_BYTE(a2), d0  ; no jump fires at EOF, matching Rust's Option<u8>-based predicate path
	tst.l d0
	bmi tokvmProgramLoop
	cmp.w LOCAL_PENDING_KIND(a2), d0
	bne tokvmProgramLoop
	move.l LOCAL_TEMP_U32(a2), d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra tokvmProgramLoop

tokvmOpcodeJumpIfClass
	move.l a0, d0
	sub.l a3, d0
	addq.l #5, d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0  ; operand 0 = compact character-class id from builder.rs default demo loop
	move.w d0, LOCAL_PENDING_KIND(a2)
	move.l (a0), d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	adda.l #4, a0
	move.l d0, LOCAL_TEMP_U32(a2)
	move.l LOCAL_CURRENT_BYTE(a2), d0  ; EOF never matches a class, same as vm_char_class_matches(None, ...)
	tst.l d0
	bmi tokvmProgramLoop
	moveq #0, d0
	move.w LOCAL_PENDING_KIND(a2), d0
	cmpi.b #1, d0
	beq.w tokvmClassWhitespace
	cmpi.b #2, d0
	beq.w tokvmClassIdentStart
	cmpi.b #3, d0
	beq.w tokvmClassIdentContinue
	cmpi.b #4, d0
	beq.w tokvmClassDigit
	cmpi.b #5, d0
	beq.w tokvmClassQuote
	bra tokvmProgramLoop

tokvmClassWhitespace
	; Class 1 is intentionally tiny in this first slice: only inline space
	; and tab are skipped by the demo loop because CR/LF are rejected up front.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tokvmIsWhitespace
	tst.l d0
	beq tokvmProgramLoop
	bra tokvmApplyClassJump

tokvmClassIdentStart
	; Class 2 mirrors the Rust identifier-start mask used by the default
	; tokenizer VM policy for ASCII letters, underscore, and dot.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tokvmIsIdentifierStart
	tst.l d0
	beq tokvmProgramLoop
	bra tokvmApplyClassJump

tokvmClassIdentContinue
	; Class 3 is wider than the start class so identifiers can continue with
	; digits and assembler-flavored suffix bytes such as '$' and '@'.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tokvmIsIdentifierContinue
	tst.l d0
	beq tokvmProgramLoop
	bra tokvmApplyClassJump

tokvmClassDigit
	; Class 4 is kept inline because the Rust helper ultimately reduces to an
	; ASCII digit check for the default family tokenizer program.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	cmpi.b #'0', d0
	blo tokvmProgramLoop
	cmpi.b #'9', d0
	bhi tokvmProgramLoop
	bra tokvmApplyClassJump

tokvmClassQuote
	; Class 5 delegates to the same quote-set logic reused by string scanning.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tokvmIsQuoteChar
	tst.l d0
	beq tokvmProgramLoop

tokvmApplyClassJump
	move.l LOCAL_TEMP_U32(a2), d0
	cmp.l d7, d0
	bhi tokvmInvalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra tokvmProgramLoop

; The scan helpers below mirror vm_scan_identifier_token,
; vm_scan_number_token, vm_scan_string_token, and vm_scan_symbol_token in
; tokenizer_runtime_utils.rs. The helper bodies reuse A0, so the interpreter
; saves and restores the native program counter around each call.
tokvmOpcodeScanIdentifier
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr tokvmScanIdentifierToken
	tst.l d0
	bne tokvmReturn
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra tokvmProgramLoop

tokvmOpcodeScanNumber
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr tokvmScanNumberToken
	tst.l d0
	bne tokvmReturn
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra tokvmProgramLoop

tokvmOpcodeScanString
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr tokvmScanStringToken
	tst.l d0
	bne tokvmReturn
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra tokvmProgramLoop

tokvmOpcodeScanSymbol
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr tokvmScanSymbolToken
	tst.l d0
	bne tokvmReturn
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra tokvmProgramLoop

tokvmInvalidArgument
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #TK_STATUS_INVALID_ARGUMENT, d0
	bra tokvmReturn

tokvmVmFailureAtCursor
	moveq #TK_STATUS_VM_FAILURE, d0
	bra tokvmReturn

tokvmStepLimitExceeded
	moveq #TK_STATUS_STEP_LIMIT_EXCEEDED, d0
	bra tokvmReturn

; Invalid program is reserved for truncated bytecode, bad jump targets, or
; opcode/operand combinations that the native interpreter refuses to execute.
tokvmInvalidProgramAtCursor
	moveq #TK_STATUS_INVALID_PROGRAM, d0

tokvmReturn
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d4-d7/a4-a6
	rts

; ---------------------------------------------------------------------------
; Native token record staging and commit.
;
; Each committed record is 20 bytes in tokenBuffer:
; - word 0: token kind code
; - long 4: 1-based start column
; - long 8: 1-based end column
; - long 12: lexemeScratch offset
; - long 16: lexeme length in bytes
;
; This is the compact native surface that the asm tests decode back into the
; OPFORGE-TOKVM 1 report format.
; ---------------------------------------------------------------------------

tokvmCommitPendingToken
	cmp.l d5, d1  ; token_count < token_capacity
	bcc tokvmPendingTokenOverflow
	move.l d3, d0  ; scratch_used + pending_len must stay within scratch_capacity
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bhi tokvmPendingLexemeOverflow
	move.l d1, d0  ; compute record_index * TOKEN_RECORD_SIZE without a MUL dependency
	add.l d0, d0  ; *2
	movea.l d0, a0
	add.l d0, d0  ; *4
	add.l d0, d0  ; *8
	add.l d0, d0  ; *16
	adda.l d0, a0  ; *18
	move.l d1, d0
	add.l d0, d0  ; +2 => *20 byte stride
	adda.l d0, a0
	movea.l a5, a1
	adda.l a0, a1
	move.w LOCAL_PENDING_KIND(a2), (a1)  ; field 0: token kind code
	clr.w 2(a1)
	move.l LOCAL_PENDING_START(a2), d0  ; field 4: 1-based start column
	addq.l #1, d0
	move.l d0, 4(a1)
	move.l LOCAL_PENDING_END(a2), d0  ; field 8: 1-based end column
	addq.l #1, d0
	move.l d0, 8(a1)
	move.l d3, 12(a1)  ; field 12: lexeme offset into scratch
	move.l LOCAL_PENDING_LEX_LEN(a2), 16(a1)  ; field 16: lexeme length in bytes
	addq.l #1, d1
	add.l LOCAL_PENDING_LEX_LEN(a2), d3
	moveq #TK_STATUS_SUCCESS, d0
	rts

; Overflow exits report the start column of the token that could not be fully
; materialized. This matches the Rust-side behavior of attributing capacity
; failures to the token currently being scanned rather than the following byte.
tokvmPendingTokenOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_TOKEN_OVERFLOW, d0
	rts

tokvmPendingLexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts

; Stage a fixed lexeme literal from the static data table into scratch.
; This is used for punctuation and operator tokens whose lexeme spelling is
; known upfront and does not need to be copied from the source buffer.
tokvmStageFixedLexeme
	move.l d0, LOCAL_PENDING_LEX_LEN(a2)  ; fixed operator/punctuation lexeme length from the inline template string
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bhi tokvmPendingLexemeOverflow
	movea.l a6, a1
	adda.l d3, a1
	move.l LOCAL_PENDING_LEX_LEN(a2), d0
tokvmStageFixedLexemeLoop
	tst.l d0
	beq tokvmStageFixedLexemeDone
	move.b (a0)+, (a1)+  ; copy the canonical lexeme bytes that Rust would expose in PortableToken text/raw
	subq.l #1, d0
	bra tokvmStageFixedLexemeLoop

tokvmStageFixedLexemeDone
	moveq #TK_STATUS_SUCCESS, d0
	rts

; ---------------------------------------------------------------------------
; Scanner helpers.
;
; These are the native counterparts of the Rust tokenizer helper routines in
; tokenizer_runtime_utils.rs. Each helper advances D2 as the live source cursor,
; populates LOCAL_PENDING_* metadata, stages lexeme bytes into the scratch
; buffer, then commits a token record.
; ---------------------------------------------------------------------------

tokvmScanIdentifierToken
	; Identifier scan is the native mirror of vm_scan_identifier_token():
	; walk identifier-continue bytes, lowercase ASCII letters for the demo
	; policy, then emit one identifier record backed by scratch bytes.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	movea.l a6, a0
	adda.l d3, a0

tokvmScanIdentifierLoop
	; D2 stays as the source cursor, A0 walks the next free scratch byte,
	; and LOCAL_PENDING_LEX_LEN grows in lockstep so commit can later write
	; both the source span and scratch payload length into the token record.
	cmp.l d4, d2
	bcc tokvmScanIdentifierDone
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tokvmIsIdentifierContinue  ; mirrors vm_matches_identifier_continue_class()
	tst.l d0
	beq tokvmScanIdentifierDone
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc tokvmPendingLexemeOverflowFromScan
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'A', d0
	blo tokvmCopyIdentifierByte
	cmpi.b #'Z', d0
	bhi tokvmCopyIdentifierByte
	ori.b #$20, d0  ; native demo bakes in ASCII-lower identifier normalization used by the Rust bridge tests
tokvmCopyIdentifierByte
	move.b d0, (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	bra tokvmScanIdentifierLoop

tokvmScanIdentifierDone
	; Match vm_scan_identifier_token(): a trailing prime belongs to the
	; identifier/register lexeme for Z80 alternate-register spellings like AF'.
	cmp.l d4, d2
	bcc tokvmScanIdentifierCommit
	cmpi.b #39, 0(a4, d2.l)
	bne tokvmScanIdentifierCommit
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc tokvmPendingLexemeOverflowFromScan
	move.b #39, (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)

tokvmScanIdentifierCommit
	; Identifier spans are half-open in cursor space and become 1-based only
	; when tokvmCommitPendingToken serializes them into the native record.
	move.w #TK_KIND_IDENTIFIER, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr tokvmCommitPendingToken
	rts

tokvmScanNumberToken
	; Number scan accepts the same permissive body bytes as the Rust helper,
	; leaving base interpretation to downstream token consumers/report logic.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	movea.l a6, a0
	adda.l d3, a0

tokvmScanNumberLoop
	; Number scanning intentionally keeps the raw source spelling, including
	; prefixes/suffixes/underscores, so later consumers can decide how to
	; interpret base markers just like the Rust runtime does.
	cmp.l d4, d2
	bcc tokvmScanNumberDone
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'%', d0
	bne tokvmScanNumberCheckBody
	cmp.l LOCAL_PENDING_START(a2), d2
	beq tokvmScanNumberAcceptByte
tokvmScanNumberCheckBody
	jsr tokvmIsNumberBody  ; same permissive number-body walk as vm_scan_number_token()
	tst.l d0
	beq tokvmScanNumberDone
tokvmScanNumberAcceptByte
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc tokvmPendingLexemeOverflowFromScan
	move.b 0(a4, d2.l), (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	bra tokvmScanNumberLoop

tokvmScanNumberDone
	move.w #TK_KIND_NUMBER, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr tokvmCommitPendingToken
	rts

tokvmScanStringToken
	; Strings keep their raw delimiter choice for closing rules, but only the
	; decoded payload bytes are staged into scratch and exposed in LEXHEX.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	moveq #0, d0
	move.b 0(a4, d2.l), d0  ; remember whether the string opened with ' or " so we can require the same closer
	move.l d0, LOCAL_CURRENT_BYTE(a2)
	addq.l #1, d2
	movea.l a6, a0
	adda.l d3, a0

tokvmScanStringLoop
	; Strings advance one payload unit at a time. Plain bytes copy through,
	; while escape sequences normalize into their decoded payload bytes so
	; LEXHEX reflects runtime string contents rather than source spelling.
	cmp.l d4, d2
	bcc tokvmScanStringFailure
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmp.l LOCAL_CURRENT_BYTE(a2), d0
	beq tokvmScanStringClose
	cmpi.b #'\\', d0  ; decode the same escape surface exercised by vm_scan_string_token()
	bne tokvmScanStringCopyLiteral
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmVmFailureAtCursor
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'n', d0
	beq tokvmStringEscapeNewline
	cmpi.b #'r', d0
	beq tokvmStringEscapeReturn
	cmpi.b #'t', d0
	beq tokvmStringEscapeTab
	cmpi.b #'x', d0  ; \xHH is decoded into one payload byte, just like the Rust helper
	beq tokvmStringEscapeHex
	bra tokvmScanStringEmitDecoded

tokvmStringEscapeNewline
	; The decoded escape value remains in D0 and falls through the shared
	; emit path that appends one payload byte to scratch.
	moveq #10, d0
	bra tokvmScanStringEmitDecoded

tokvmStringEscapeReturn
	moveq #13, d0
	bra tokvmScanStringEmitDecoded

tokvmStringEscapeTab
	moveq #9, d0
	bra tokvmScanStringEmitDecoded

tokvmStringEscapeHex
	; Parse exactly two hex digits after \x and combine them into one byte,
	; mirroring tokenizer_runtime_utils::vm_scan_string_token().
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmScanStringFailure
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tokvmHexDigitValue
	tst.l d0
	bmi tokvmScanStringFailure
	move.l d0, LOCAL_TEMP_U32(a2)
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmScanStringFailure
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tokvmHexDigitValue
	tst.l d0
	bmi tokvmScanStringFailure
	move.l d1, -(sp)
	move.l LOCAL_TEMP_U32(a2), d1
	lsl.l #4, d1
	or.l d1, d0
	move.l (sp)+, d1

	bra tokvmScanStringEmitDecoded

tokvmScanStringCopyLiteral
	; Literal non-escape bytes use the same capacity accounting as decoded
	; escapes so both paths feed one consistent scratch payload stream.
	move.l d1, -(sp)
	move.l d3, d1
	add.l LOCAL_PENDING_LEX_LEN(a2), d1
	cmp.l d6, d1
	bcc tokvmScanStringLiteralOverflow
	move.l (sp)+, d1
	bra tokvmScanStringEmitDecoded

tokvmScanStringLiteralOverflow
	move.l (sp)+, d1
	bra tokvmPendingLexemeOverflowFromScan

tokvmScanStringEmitDecoded
	move.b d0, (a0)+
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	addq.l #1, d2
	bra tokvmScanStringLoop

tokvmScanStringClose
	; D2 is advanced past the closing delimiter before commit so the source
	; span matches the Rust token span semantics for quoted strings.
	addq.l #1, d2
	move.w #TK_KIND_STRING, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr tokvmCommitPendingToken
	rts

; Unterminated strings and malformed escape sequences both collapse to the same
; VM failure status in this first native slice.
tokvmScanStringFailure
	moveq #TK_STATUS_VM_FAILURE, d0
	rts

; Symbol scan covers punctuation, operators, comments, and prefixed numeric
; forms. The structure intentionally parallels vm_scan_symbol_token() in Rust:
; dispatch by lead byte, optionally consume a longer form, then commit the
; canonical lexeme bytes through tokvmStageAndCommitSymbol.
tokvmScanSymbolToken
	; The dispatch order matters. More syntactically specific lead bytes are
	; tested before generic operator fallbacks so multi-byte forms get the
	; same precedence as in the Rust helper.
	move.l d2, LOCAL_PENDING_START(a2)
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #';', d0
	beq tokvmScanCommentToEol
	cmpi.b #'.', d0
	beq tokvmScanDotLike
	cmpi.b #'$', d0
	beq tokvmScanDollarOrPrefixedNumber
	cmpi.b #'%', d0
	beq tokvmScanPercentOrPrefixedNumber
	cmpi.b #'#', d0
	beq tokvmStageHash
	cmpi.b #'?', d0
	beq tokvmStageQuestion
	cmpi.b #'[', d0
	beq tokvmStageOpenBracket
	cmpi.b #']', d0
	beq tokvmStageCloseBracket
	cmpi.b #'{', d0
	beq tokvmStageOpenBrace
	cmpi.b #'}', d0
	beq tokvmStageCloseBrace
	cmpi.b #',', d0
	beq tokvmStageComma
	cmpi.b #':', d0
	beq tokvmStageColon
	cmpi.b #'(', d0
	beq tokvmStageOpenParen
	cmpi.b #')', d0
	beq tokvmStageCloseParen
	cmpi.b #'+', d0
	beq tokvmStagePlus
	cmpi.b #'-', d0
	beq tokvmStageMinus
	cmpi.b #'*', d0
	beq tokvmScanStarLike
	cmpi.b #'/', d0
	beq tokvmStageDivide
	cmpi.b #'~', d0
	beq tokvmStageBitNot
	cmpi.b #'=', d0
	beq tokvmScanEqualLike
	cmpi.b #'!', d0
	beq tokvmScanBangLike
	cmpi.b #'&', d0
	beq tokvmScanAndLike
	cmpi.b #'|', d0
	beq tokvmScanOrLike
	cmpi.b #'^', d0
	beq tokvmScanCaretLike
	cmpi.b #'<', d0
	beq tokvmScanLessLike
	cmpi.b #'>', d0
	beq tokvmScanGreaterLike
	move.w #TK_VM_FAILURE_KIND_FAIL, TokvmLastFailureKind
	move.w d0, TokvmLastFailureOperand
	moveq #TK_STATUS_VM_FAILURE, d0
	rts

tokvmScanCommentToEol
	move.l d4, d2  ; comments consume the rest of the line and emit no token, matching vm_scan_symbol_token()
	moveq #TK_STATUS_SUCCESS, d0
	rts

tokvmScanDotLike
	; '.', '..', and '..=' are grouped together so the operator family stays
	; adjacent in both the native and Rust scanner implementations.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageDot
	cmpi.b #'.', 0(a4, d2.l)
	bne tokvmStageDot
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageRange
	cmpi.b #'=', 0(a4, d2.l)
	bne tokvmStageRange
	addq.l #1, d2
	move.w #TK_KIND_OP_RANGE_INCLUSIVE, LOCAL_PENDING_KIND(a2)
	lea LexRangeInclusive, a0
	moveq #3, d0
	bra tokvmStageAndCommitSymbol

tokvmStageRange
	; '..' and '..=' share the same entry path so the inclusive form only
	; needs one extra lookahead byte and a different fixed lexeme template.
	move.w #TK_KIND_OP_RANGE, LOCAL_PENDING_KIND(a2)
	lea LexRange, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageDot
	move.w #TK_KIND_DOT, LOCAL_PENDING_KIND(a2)
	lea LexDot, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanDollarOrPrefixedNumber
	; '$' is ambiguous by design: either a standalone dollar token or the
	; prefix for a hex-like number literal if a valid body byte follows.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageDollar
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tokvmIsHexDigitOrUnderscore  ; '$' starts either a hex literal or a standalone dollar token
	tst.l d0
	beq tokvmStageDollar
	move.l LOCAL_PENDING_START(a2), d2
	jsr tokvmScanNumberToken
	rts

tokvmStageDollar
	move.w #TK_KIND_DOLLAR, LOCAL_PENDING_KIND(a2)
	lea LexDollar, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanPercentOrPrefixedNumber
	; '%' is likewise split between modulo and binary-prefixed number forms.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStagePercent
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'0', d0
	beq tokvmScanPercentAsNumber
	cmpi.b #'1', d0
	bne tokvmStagePercent

tokvmScanPercentAsNumber
	jsr tokvmPercentHasPrefixContext
	tst.l d0
	beq tokvmStagePercent
	move.l LOCAL_PENDING_START(a2), d2  ; rewind so the number scanner sees the leading '%', like Rust prefixed-number handling
	jsr tokvmScanNumberToken
	rts

tokvmStagePercent
	move.w #TK_KIND_OP_MOD, LOCAL_PENDING_KIND(a2)
	lea LexMod, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageHash
	addq.l #1, d2
	move.w #TK_KIND_HASH, LOCAL_PENDING_KIND(a2)
	lea LexHash, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageQuestion
	; The single-byte staging labels below follow one repeated pattern:
	; advance the source cursor, choose a token kind, point at the canonical
	; lexeme bytes, set the lexeme length, then funnel through the shared
	; stage-and-commit tail.
	addq.l #1, d2
	move.w #TK_KIND_QUESTION, LOCAL_PENDING_KIND(a2)
	lea LexQuestion, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageOpenBracket
	addq.l #1, d2
	move.w #TK_KIND_OPEN_BRACKET, LOCAL_PENDING_KIND(a2)
	lea LexOpenBracket, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageCloseBracket
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_BRACKET, LOCAL_PENDING_KIND(a2)
	lea LexCloseBracket, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageOpenBrace
	addq.l #1, d2
	move.w #TK_KIND_OPEN_BRACE, LOCAL_PENDING_KIND(a2)
	lea LexOpenBrace, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageCloseBrace
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_BRACE, LOCAL_PENDING_KIND(a2)
	lea LexCloseBrace, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageComma
	addq.l #1, d2
	move.w #TK_KIND_COMMA, LOCAL_PENDING_KIND(a2)
	lea LexComma, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageColon
	addq.l #1, d2
	move.w #TK_KIND_COLON, LOCAL_PENDING_KIND(a2)
	lea LexColon, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageOpenParen
	addq.l #1, d2
	move.w #TK_KIND_OPEN_PAREN, LOCAL_PENDING_KIND(a2)
	lea LexOpenParen, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageCloseParen
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_PAREN, LOCAL_PENDING_KIND(a2)
	lea LexCloseParen, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStagePlus
	addq.l #1, d2
	move.w #TK_KIND_OP_PLUS, LOCAL_PENDING_KIND(a2)
	lea LexPlus, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageMinus
	addq.l #1, d2
	move.w #TK_KIND_OP_MINUS, LOCAL_PENDING_KIND(a2)
	lea LexMinus, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanStarLike
	; '*' and '**' match the Rust tokenizer's multiply/power split.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageMultiply
	cmpi.b #'*', 0(a4, d2.l)
	beq tokvmStagePower

tokvmStageMultiply
	move.w #TK_KIND_OP_MULTIPLY, LOCAL_PENDING_KIND(a2)
	lea LexMultiply, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStagePower
	addq.l #1, d2
	move.w #TK_KIND_OP_POWER, LOCAL_PENDING_KIND(a2)
	lea LexPower, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageDivide
	addq.l #1, d2
	move.w #TK_KIND_OP_DIVIDE, LOCAL_PENDING_KIND(a2)
	lea LexDivide, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmStageBitNot
	addq.l #1, d2
	move.w #TK_KIND_OP_BIT_NOT, LOCAL_PENDING_KIND(a2)
	lea LexBitNot, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanEqualLike
	; '=' and '==' both normalize to the same equality token kind, matching
	; the Rust tokenizer helper's forgiving equality parsing.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageEq
	cmpi.b #'=', 0(a4, d2.l)
	bne tokvmStageEq
	addq.l #1, d2

tokvmStageEq
	; The canonical fixed lexeme is always "==" for equality so report output
	; normalizes '=' and '==' into one operator surface.
	move.w #TK_KIND_OP_EQ, LOCAL_PENDING_KIND(a2)
	lea LexEq, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmScanBangLike
	; '!' stands for logical-not, while '!=' upgrades to not-equal.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageLogicNot
	cmpi.b #'=', 0(a4, d2.l)
	bne tokvmStageLogicNot
	addq.l #1, d2
	move.w #TK_KIND_OP_NE, LOCAL_PENDING_KIND(a2)
	lea LexNe, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageLogicNot
	; Unlike equality, logical-not preserves its single-byte spelling in the
	; report/output surface because '!' and '!=' are distinct token kinds.
	move.w #TK_KIND_OP_LOGIC_NOT, LOCAL_PENDING_KIND(a2)
	lea LexLogicNot, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanAndLike
	; '&' / '&&' map to bitwise and logical forms respectively.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageBitAnd
	cmpi.b #'&', 0(a4, d2.l)
	bne tokvmStageBitAnd
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_AND, LOCAL_PENDING_KIND(a2)
	lea LexLogicAnd, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageBitAnd
	move.w #TK_KIND_OP_BIT_AND, LOCAL_PENDING_KIND(a2)
	lea LexBitAnd, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanOrLike
	; '|' / '||' map to bitwise and logical forms respectively.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageBitOr
	cmpi.b #'|', 0(a4, d2.l)
	bne tokvmStageBitOr
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_OR, LOCAL_PENDING_KIND(a2)
	lea LexLogicOr, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageBitOr
	move.w #TK_KIND_OP_BIT_OR, LOCAL_PENDING_KIND(a2)
	lea LexBitOr, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanCaretLike
	; '^' is bitwise xor, while '^^' is promoted to the logical xor
	; token used by the native report-name table.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageBitXor
	cmpi.b #'^', 0(a4, d2.l)
	bne tokvmStageBitXor
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_XOR, LOCAL_PENDING_KIND(a2)
	lea LexLogicXor, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageBitXor
	move.w #TK_KIND_OP_BIT_XOR, LOCAL_PENDING_KIND(a2)
	lea LexBitXor, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanLessLike
	; '<' expands into four related operators: <, <<, <=, and <>/!=.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageLt
	cmpi.b #'<', 0(a4, d2.l)
	beq tokvmStageShl
	cmpi.b #'=', 0(a4, d2.l)
	beq tokvmStageLe
	cmpi.b #'>', 0(a4, d2.l)
	beq tokvmStageAltNe
	bra tokvmStageLt

tokvmStageShl
	addq.l #1, d2
	move.w #TK_KIND_OP_SHL, LOCAL_PENDING_KIND(a2)
	lea LexShl, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageLe
	addq.l #1, d2
	move.w #TK_KIND_OP_LE, LOCAL_PENDING_KIND(a2)
	lea LexLe, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageAltNe
	addq.l #1, d2
	move.w #TK_KIND_OP_NE, LOCAL_PENDING_KIND(a2)
	lea LexNe, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageLt
	; The family labels above all converge here with a fully-chosen token
	; kind and lexeme template, so the commit tail can stay generic.
	move.w #TK_KIND_OP_LT, LOCAL_PENDING_KIND(a2)
	lea LexLt, a0
	moveq #1, d0
	bra tokvmStageAndCommitSymbol

tokvmScanGreaterLike
	; '>' expands into >, >>, and >=.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tokvmStageGt
	cmpi.b #'>', 0(a4, d2.l)
	beq tokvmStageShr
	cmpi.b #'=', 0(a4, d2.l)
	beq tokvmStageGe
	bra tokvmStageGt

tokvmStageShr
	addq.l #1, d2
	move.w #TK_KIND_OP_SHR, LOCAL_PENDING_KIND(a2)
	lea LexShr, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageGe
	addq.l #1, d2
	move.w #TK_KIND_OP_GE, LOCAL_PENDING_KIND(a2)
	lea LexGe, a0
	moveq #2, d0
	bra tokvmStageAndCommitSymbol

tokvmStageGt
	move.w #TK_KIND_OP_GT, LOCAL_PENDING_KIND(a2)
	lea LexGt, a0
	moveq #1, d0

tokvmStageAndCommitSymbol
	; At this point LOCAL_PENDING_START already marks the source span start,
	; D2 already points just past the consumed source bytes, and A0/D0 name
	; the canonical lexeme bytes to materialize into scratch.
	jsr tokvmStageFixedLexeme  ; stage the canonical lexeme bytes before committing the token metadata
	tst.l d0
	bne tokvmStageAndCommitSymbolDone
	move.l d2, LOCAL_PENDING_END(a2)
	jsr tokvmCommitPendingToken
tokvmStageAndCommitSymbolDone
	rts

; Used when symbol lookahead discovers a shape the native bytecode contract does
; not allow, such as the unsupported '**' power operator.
tokvmScanSymbolInvalidProgram
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_INVALID_PROGRAM, d0
	rts

; Shared overflow exit for any scanner that would exceed scratch capacity.
tokvmPendingLexemeOverflowFromScan
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts

; ---------------------------------------------------------------------------
; Character-class and byte-shape predicates.
;
; These intentionally parallel vm_char_class_matches, vm_matches_identifier_*,
; and related helper logic in tokenizer_runtime_utils.rs. The native demo loop
; calls them through JumpIfClass and the scan helpers reuse them while walking
; identifiers, number bodies, strings, and prefixed constants.
; ---------------------------------------------------------------------------

tokvmIsWhitespace
	cmpi.b #' ', d0  ; this line-input slice only treats space and tab as intra-line whitespace
	beq tokvmPredicateTrue
	cmpi.b #9, d0
	beq tokvmPredicateTrue
	moveq #0, d0
	rts

tokvmIsIdentifierStart
	; These predicate chains intentionally avoid lookup tables so the native
	; implementation stays easy to audit against the Rust helper masks.
	cmpi.b #'A', d0
	blo tokvmCheckIdentStartLower
	cmpi.b #'Z', d0
	bls tokvmPredicateTrue
tokvmCheckIdentStartLower
	cmpi.b #'a', d0
	blo tokvmCheckIdentStartPunct
	cmpi.b #'z', d0
	bls tokvmPredicateTrue
tokvmCheckIdentStartPunct
	cmpi.b #'_', d0
	beq tokvmPredicateTrue
	cmpi.b #'.', d0  ; '.' remains identifier-start-capable because the runtime class mask includes it
	beq tokvmPredicateTrue
	moveq #0, d0
	rts

tokvmIsIdentifierContinue
	cmpi.b #'A', d0
	blo tokvmCheckIdentContinueLower
	cmpi.b #'Z', d0
	bls tokvmPredicateTrue
tokvmCheckIdentContinueLower
	cmpi.b #'a', d0
	blo tokvmCheckIdentContinueDigit
	cmpi.b #'z', d0
	bls tokvmPredicateTrue
tokvmCheckIdentContinueDigit
	cmpi.b #'0', d0
	blo tokvmCheckIdentExtra
	cmpi.b #'9', d0
	bls tokvmPredicateTrue
tokvmCheckIdentExtra
	cmpi.b #'_', d0
	beq tokvmPredicateTrue
	cmpi.b #'.', d0
	beq tokvmPredicateTrue
	cmpi.b #'$', d0  ; '$' and '@' stay valid continue bytes per tokenizer_runtime_utils.rs masks
	beq tokvmPredicateTrue
	cmpi.b #'@', d0
	beq tokvmPredicateTrue
	moveq #0, d0
	rts

tokvmIsQuoteChar
	cmpi.b #'"', d0  ; demo program accepts both quote styles, matching the Rust helper's quote-char set
	beq tokvmPredicateTrue
	cmpi.b #39, d0
	beq tokvmPredicateTrue
	moveq #0, d0
	rts

tokvmIsNumberBody
	; Number bodies are deliberately permissive at scan time. Validation of
	; bases and suffix meaning is deferred to later consumers, matching the
	; Rust tokenizer helper contract.
	cmpi.b #'0', d0
	blo tokvmCheckNumberLetters
	cmpi.b #'9', d0
	bls tokvmPredicateTrue
tokvmCheckNumberLetters
	cmpi.b #'A', d0
	blo tokvmCheckNumberLower
	cmpi.b #'Z', d0
	bls tokvmPredicateTrue
tokvmCheckNumberLower
	cmpi.b #'a', d0
	blo tokvmCheckNumberExtra
	cmpi.b #'z', d0
	bls tokvmPredicateTrue
tokvmCheckNumberExtra
	cmpi.b #'_', d0
	beq tokvmPredicateTrue
	cmpi.b #'$', d0
	beq tokvmPredicateTrue
	cmpi.b #'%', d0
	beq tokvmPredicateTrue
	cmpi.b #'@', d0
	beq tokvmPredicateTrue
	moveq #0, d0
	rts

tokvmIsHexDigitOrUnderscore
	; Used only as a fast probe for deciding whether '$' begins a number or
	; remains a standalone token.
	cmpi.b #'_', d0
	beq tokvmPredicateTrue
	jsr tokvmHexDigitValue
	tst.l d0
	bmi tokvmPredicateFalse
tokvmPredicateTrue
	moveq #1, d0
	rts

tokvmPredicateFalse
	moveq #0, d0
	rts

; Shared hex nibble decoder for both string escape parsing and '$'-prefixed
; number probing. Returns -1 for non-hex input so callers can branch cleanly.
tokvmHexDigitValue
	cmpi.b #'0', d0  ; shared nibble decoder for \xHH strings and '$'-prefixed number probing
	blo tokvmHexUpper
	cmpi.b #'9', d0
	bhi tokvmHexUpper
	subi.b #'0', d0
	andi.l #$FF, d0
	rts

tokvmHexUpper
	cmpi.b #'A', d0
	blo tokvmHexLower
	cmpi.b #'F', d0
	bhi tokvmHexLower
	subi.b #'A', d0
	addi.b #10, d0
	andi.l #$FF, d0
	rts

tokvmHexLower
	cmpi.b #'a', d0
	blo tokvmHexInvalid
	cmpi.b #'f', d0
	bhi tokvmHexInvalid
	subi.b #'a', d0
	addi.b #10, d0
	andi.l #$FF, d0
	rts

tokvmHexInvalid
	moveq #-1, d0
	rts

; Rust treats '%' as a binary-number prefix only when the byte appears where an
; expression can start. Without that context, % remains the modulo operator.
tokvmPercentHasPrefixContext
	move.l LOCAL_PENDING_START(a2), d0
	beq tokvmPercentPrefixTrue

	clr.w LOCAL_PENDING_KIND(a2)
	clr.l LOCAL_TEMP_U32(a2)
	subq.l #1, d0
	move.l d0, LOCAL_TEMP_U32(a2)
	lea 0(a4, d0.l), a1
	moveq #0, d0
	move.b (a1), d0
	cmpi.b #' ', d0
	beq tokvmPercentMarkLeadingSpace
	cmpi.b #9, d0
	bne tokvmPercentCheckPrevNonSpaceByte

tokvmPercentMarkLeadingSpace
	moveq #1, d0
	move.w d0, LOCAL_PENDING_KIND(a2)

tokvmPercentPrevNonSpaceLoop
	move.l LOCAL_TEMP_U32(a2), d0
	tst.l d0
	beq tokvmPercentPrefixTrue
	subq.l #1, d0
	move.l d0, LOCAL_TEMP_U32(a2)
	lea 0(a4, d0.l), a1
	moveq #0, d0
	move.b (a1), d0

tokvmPercentCheckPrevNonSpaceByte
	cmpi.b #' ', d0
	beq tokvmPercentPrevNonSpaceLoop
	cmpi.b #9, d0
	beq tokvmPercentPrevNonSpaceLoop
	cmpi.b #'(', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #',', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'+', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'-', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'*', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'/', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'%', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'&', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'|', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'^', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'~', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'!', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'<', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'>', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'=', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #'?', d0
	beq tokvmPercentPrefixTrue
	cmpi.b #':', d0
	beq tokvmPercentPrefixTrue

	tst.w LOCAL_PENDING_KIND(a2)
	beq tokvmPercentPrefixFalse
	jsr tokvmIsIdentifierContinue
	tst.l d0
	bne tokvmPercentPrefixTrue

tokvmPercentPrefixFalse
	moveq #0, d0
	rts

tokvmPercentPrefixTrue
	moveq #1, d0
	rts

	.endsection
	.section data, kind=data

; Data section: ABI marker, bytecode macros, demo bytecode, and fixed lexeme
; templates used by the symbol scanner.
AbiMarker
	.byte "OPFORGE-TOKVM-ABI-V1", 0

; The emit* helpers encode the same little-endian jump-target format produced by
; Rust's default_family_tokenizer_vm_program_bytes(). Keep demoProgram and the
; DEMO_PC_* offsets together so the control-flow map stays readable.
emitLe32	.macro value
	; Keep jump targets readable in source while still emitting the same
	; little-endian u32 layout as builder.rs and the Rust VM loader expect.
	.byte (.value) & $ff
	.byte ((.value) >> 8) & $ff
	.byte ((.value) >> 16) & $ff
	.byte ((.value) >> 24) & $ff
.endmacro

emitJumpTarget	.macro opcode, target
	; Macros keep the demo bytecode readable without obscuring the exact byte
	; sequence that the Rust builder would emit.
	.byte .opcode
	.emitLe32 .target
.endmacro

emitClassJump	.macro class_id, target
	.byte TK_OPCODE_JUMP_IF_CLASS, .class_id
	.emitLe32 .target
.endmacro

emitByteJump	.macro byte_value, target
	.byte TK_OPCODE_JUMP_IF_BYTE_EQ, .byte_value
	.emitLe32 .target
.endmacro

; Default tokenizer VM loop for this native example.
; This intentionally mirrors crates/opforge-vm/src/builder.rs:
; - ReadChar
; - if EOL -> End
; - if whitespace -> Advance and loop
; - if '.' or symbol-leading punctuation -> ScanSymbol
; - if identifier-start -> ScanIdentifier
; - if digit -> ScanNumber
; - if quote -> ScanString
;
; The bytecode below is the readable assembler mirror of builder.rs
; default_family_tokenizer_vm_program_bytes(): read char, branch by class, then loop.
DemoProgram
DemoReadChar
	.byte TK_OPCODE_READ_CHAR
	.emitJumpTarget TK_OPCODE_JUMP_IF_EOL, DEMO_PC_FINISH
	.emitClassJump TK_CLASS_WHITESPACE, DEMO_PC_SKIP_WHITESPACE
	.emitByteJump '.', DEMO_PC_SCAN_SYMBOL
	.emitClassJump TK_CLASS_IDENTIFIER_START, DEMO_PC_SCAN_IDENTIFIER
	.emitClassJump TK_CLASS_DIGIT, DEMO_PC_SCAN_NUMBER
	.emitClassJump TK_CLASS_QUOTE, DEMO_PC_SCAN_STRING

DemoScanSymbol
	; Every scan arm jumps back to DEMO_PC_READ_CHAR so the program behaves
	; as a tight read-dispatch-scan loop until EOL.
	.byte TK_OPCODE_SCAN_SYMBOL
	.emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

DemoSkipWhitespace
	; Whitespace is the only class that does not emit a token. It simply
	; advances one byte and loops back to the next ReadChar.
	.byte TK_OPCODE_ADVANCE
	.emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

DemoScanIdentifier
	.byte TK_OPCODE_SCAN_IDENTIFIER
	.emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

DemoScanNumber
	.byte TK_OPCODE_SCAN_NUMBER
	.emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

DemoScanString
	.byte TK_OPCODE_SCAN_STRING
	.emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

DemoFinish
	.byte TK_OPCODE_END

; Canonical lexeme spellings used by tokvmStageFixedLexeme.
; Keeping them in one contiguous table makes the symbol scanner readable and
; ensures the report's LEXHEX field stays stable across all operator forms.
; Grouping also makes it obvious which operators are implemented natively in
; this slice: if there is no fixed lexeme entry here, the scanner cannot emit it.
LexDot
	.byte "."
LexDollar
	.byte "$"
LexHash
	.byte "#"
LexQuestion
	.byte "?"
LexOpenBracket
	.byte "["
LexCloseBracket
	.byte "]"
LexOpenBrace
	.byte "{"
LexCloseBrace
	.byte "}"
LexComma
	.byte ","
LexColon
	.byte ":"
LexOpenParen
	.byte "("
LexCloseParen
	.byte ")"
LexPlus
	.byte "+"
LexMinus
	.byte "-"
LexMultiply
	.byte "*"
LexPower
	.byte "**"
LexDivide
	.byte "/"
LexBitNot
	.byte "~"
LexEq
	.byte "=="
LexNe
	.byte "!="
LexLogicNot
	.byte "!"
LexBitAnd
	.byte "&"
LexBitOr
	.byte "|"
LexLogicAnd
	.byte "&&"
LexLogicOr
	.byte "||"
LexBitXor
	.byte "^"
LexLogicXor
	.byte "^^"
LexLt
	.byte "<"
LexLe
	.byte "<="
LexGt
	.byte ">"
LexGe
	.byte ">="
LexShl
	.byte "<<"
LexShr
	.byte ">>"
LexMod
	.byte "%"
LexRange
	.byte ".."
LexRangeInclusive
	.byte "..="

; 67 bytes is the assembled size of demoProgram and must stay aligned with the
; symbolic DEMO_PC_* offsets above as well as the Rust builder's default loop.
DemoProgramLen
	.long 67

	.endsection
	.endmodule
