; Native tokenizer VM runtime used by tkpkg and test harnesses.
;
; Owns the runtime-model constants, interpreter loop, demo bytecode, and
; token-shape metadata that the CLI harness imports through `.use`.

	.module tkvm.amigaos.runtime
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
TKVM_DEFAULT_MAX_STEPS_PER_LINE = 2048

; The fixed capacities above intentionally match the AmigaOS host harness so the
; native VM and the CLI/report layer agree on how much source, token, and scratch
; state can be exchanged without any additional negotiation structure.

; tkvm_run_68000 local frame layout.
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

TkvmStepBudget
	.long TKVM_DEFAULT_MAX_STEPS_PER_LINE

DemoStateEntryOffsets
	.long DEMO_PC_READ_CHAR

TkvmProgramStateTablePtr
	.long DemoStateEntryOffsets

TkvmProgramStateCount
	.long 1

TkvmProgramStartState
	.word 0

TkvmLastFailureKind
	.word 0

TkvmLastFailureOperand
	.word 0

	.endsection

	.section code, kind=code

	.pub

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
tkvmSetStepBudget68000	.block
	tst.l d0
	bgt.s store
	move.l #TKVM_DEFAULT_MAX_STEPS_PER_LINE, d0
store
	move.l d0, TkvmStepBudget
	rts
	.bend  ; tkvmSetStepBudget68000

; Install a package-provided state table; invalid counts fall back to demo state 0.
tkvmSetProgramStateTable68000	.block
	tst.l d0
	bgt.s store
	lea DemoStateEntryOffsets, a0
	moveq #1, d0
	moveq #0, d1
store
	move.l a0, TkvmProgramStateTablePtr
	move.l d0, TkvmProgramStateCount
	move.w d1, TkvmProgramStartState
	rts
	.bend  ; tkvmSetProgramStateTable68000

; Return the last explicit VM failure kind/operand captured by tkvm_run_68000.
tkvmReadLastFailure68000	.block
	moveq #0, d0
	move.w TkvmLastFailureKind, d0
	moveq #0, d1
	move.w TkvmLastFailureOperand, d1
	rts
	.bend  ; tkvmReadLastFailure68000

tkvmRun68000	.block
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
	move.l TkvmStepBudget, d0
	move.l d0, LOCAL_STEP_LIMIT(a2)
	moveq #-1, d0  ; sentinel current byte = EOF until ReadChar runs
	move.l d0, LOCAL_CURRENT_BYTE(a2)
	clr.w TkvmLastFailureKind
	clr.w TkvmLastFailureOperand

	tst.l d4  ; reject negative lengths/capacities before dereferencing any caller pointers
	bmi invalidArgument
	tst.l d5
	bmi invalidArgument
	tst.l d6
	bmi invalidArgument
	tst.l d7
	bmi invalidArgument

	tst.l d4  ; non-empty source requires a non-null source pointer
	beq checkTokenPointer
	move.l a4, d0
	tst.l d0
	beq invalidArgument

checkTokenPointer
	tst.l d5  ; non-zero token capacity requires a writable token buffer
	beq checkScratchPointer
	move.l a5, d0
	tst.l d0
	beq invalidArgument

checkScratchPointer
	tst.l d6  ; non-zero scratch capacity requires a writable scratch buffer
	beq checkProgramPointer
	move.l a6, d0
	tst.l d0
	beq invalidArgument

checkProgramPointer
	tst.l d7  ; bytecode length 0 cannot encode a valid tokenizer program
	beq invalidProgramAtCursor
	move.l a3, d0
	tst.l d0
	beq invalidArgument

	moveq #0, d0  ; proactively reject CR/LF because this slice models one line-input stream only
newlineScanLoop
	cmp.l d4, d0
	bcc newlineScanDone
	cmpi.b #10, 0(a4, d0.l)
	beq newlineUnsupported
	cmpi.b #13, 0(a4, d0.l)
	beq newlineUnsupported
	addq.l #1, d0
	bra newlineScanLoop

newlineUnsupported
	move.l d0, d2
	moveq #TK_STATUS_NEWLINE_UNSUPPORTED, d0
	bra return

newlineScanDone
	moveq #0, d0
	move.w TkvmProgramStartState, d0
	cmp.l TkvmProgramStateCount, d0
	bcc invalidProgramAtCursor
	move.l TkvmProgramStateTablePtr, d1
	tst.l d1
	beq.w invalidProgramAtCursor
	movea.l d1, a1
	move.l 0.W(a1, d0.l*4), d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	lea 0(a3, d0.l), a0
	lea 0(a3, d7.l), a1
	clr.l d1  ; token count must still enter the first loop iteration as 0

; Main bytecode dispatch loop.
; A0 is the native program counter, A1 is the bytecode end pointer, D2 is the
; source cursor, and LOCAL_CURRENT_BYTE stores the last ReadChar result. This
; corresponds directly to the Rust match over TokenizerVmOpcode.
programLoop
	move.l LOCAL_STEP_COUNT(a2), d0
	addq.l #1, d0
	move.l d0, LOCAL_STEP_COUNT(a2)
	cmp.l LOCAL_STEP_LIMIT(a2), d0
	bhi stepLimitExceeded
	lea 0(a3, d7.l), a1
	cmp.l d4, d2
	bhi invalidProgramAtCursor
	cmpa.l a1, a0
	bcc invalidProgramAtCursor

dispatchOpcode
	moveq #0, d0
	move.b (a0)+, d0

	; The native slice only implements opcode values 0..18.
	; Unsupported shared VM slots still get explicit table entries so the
	; opcode-to-handler mapping stays visible and future additions stay local.
	cmpi.b #TK_OPCODE_SCAN_SYMBOL, d0
	bhi invalidProgramAtCursor
	add.w d0, d0
	add.w d0, d0
	lea TkvmOpcodeDispatchTable(PC), a1
	movea.l 0(a1, d0.W), a1
	jmp (a1)

TkvmOpcodeDispatchTable
	.long opcodeEnd
	.long opcodeReadChar
	.long opcodeAdvance
	.long opcodeStartLexeme
	.long opcodePushChar
	.long opcodeEmitToken
	.long opcodeSetState
	.long opcodeJump
	.long opcodeJumpIfEol
	.long opcodeJumpIfByteEq
	.long opcodeJumpIfClass
	.long opcodeFail
	.long opcodeEmitDiag
	.long invalidProgramAtCursor
	.long invalidProgramAtCursor
	.long opcodeScanIdentifier
	.long opcodeScanNumber
	.long opcodeScanString
	.long opcodeScanSymbol

opcodeEnd
	cmp.l d4, d2  ; Rust runtime also only accepts END when the source cursor is at EOL
	bne invalidProgramAtCursor
	moveq #TK_STATUS_SUCCESS, d0
	bra return

	; ReadChar mirrors VmTokenizerInputStream.current_byte(): live bytes are zero-extended
	; into D0 and only EOF uses the -1 sentinel stored in LOCAL_CURRENT_BYTE.
opcodeReadChar
	moveq #0, d0
	cmp.l d4, d2
	bcc storeEofByte
	move.b 0(a4, d2.l), d0
	bra storeCurrentByte
storeEofByte
	moveq #-1, d0
storeCurrentByte
	move.l d0, LOCAL_CURRENT_BYTE(a2)

	bra programLoop

opcodeAdvance
	cmp.l d4, d2  ; advance saturates at EOL, same as VmTokenizerInputStream.advance()
	bcc programLoop
	addq.l #1, d2
	bra programLoop

opcodeStartLexeme
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	move.l d2, LOCAL_PENDING_START(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	bra programLoop

opcodePushChar
	move.l LOCAL_CURRENT_BYTE(a2), d0
	tst.l d0
	bmi invalidProgramAtCursor
	move.l d1, LOCAL_TEMP_U32(a2)
	move.l d3, d1
	add.l LOCAL_PENDING_LEX_LEN(a2), d1
	cmp.l d6, d1
	bcc lexemeOverflowAtCursor
	movea.l a6, a1
	adda.l d1, a1
	move.b d0, (a1)
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	move.l d2, d1
	addq.l #1, d1
	move.l d1, LOCAL_PENDING_END(a2)
	move.l LOCAL_TEMP_U32(a2), d1
	bra programLoop

opcodeEmitToken
	lea 0(a3, d7.l), a1
	cmpa.l a1, a0
	bcc invalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0
	move.w d0, LOCAL_PENDING_KIND(a2)
	jsr commitPendingToken
	tst.l d0
	bne return
	bra programLoop

opcodeSetState
	move.l a0, d0
	sub.l a3, d0
	addq.l #2, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	move.l d1, LOCAL_TEMP_U32(a2)
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.w #8, d1
	or.w d1, d0
	cmp.l TkvmProgramStateCount, d0
	bcc invalidProgramAtCursor
	move.l TkvmProgramStateTablePtr, d1
	tst.l d1
	beq.w invalidProgramAtCursor
	movea.l d1, a1
	move.l 0.W(a1, d0.l*4), d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	lea 0(a3, d0.l), a0
	move.l LOCAL_TEMP_U32(a2), d1
	bra programLoop

opcodeFail
	move.l a0, d0
	sub.l a3, d0
	addq.l #1, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0
	move.w #TK_VM_FAILURE_KIND_FAIL, TkvmLastFailureKind
	move.w d0, TkvmLastFailureOperand
	bra vmFailureAtCursor

opcodeEmitDiag
	move.l a0, d0
	sub.l a3, d0
	addq.l #1, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0
	move.w #TK_VM_FAILURE_KIND_EMIT_DIAG, TkvmLastFailureKind
	move.w d0, TkvmLastFailureOperand
	bra vmFailureAtCursor

opcodeJump
	move.l a0, d0
	sub.l a3, d0
	addq.l #4, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	move.l (a0), d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	adda.l #4, a0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra programLoop

opcodeJumpIfEol
	move.l a0, d0
	sub.l a3, d0
	addq.l #4, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	move.l (a0), d0
	ror.w #8, d0
	swap d0
	ror.w #8, d0
	adda.l #4, a0
	cmp.l d4, d2
	bne programLoop
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra programLoop

opcodeJumpIfByteEq
	move.l a0, d0
	sub.l a3, d0
	addq.l #5, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
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
	bmi programLoop
	cmp.w LOCAL_PENDING_KIND(a2), d0
	bne programLoop
	move.l LOCAL_TEMP_U32(a2), d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra programLoop

opcodeJumpIfClass
	move.l a0, d0
	sub.l a3, d0
	addq.l #5, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
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
	bmi programLoop
	moveq #0, d0
	move.w LOCAL_PENDING_KIND(a2), d0
	cmpi.b #1, d0
	beq.w classWhitespace
	cmpi.b #2, d0
	beq.w classIdentStart
	cmpi.b #3, d0
	beq.w classIdentContinue
	cmpi.b #4, d0
	beq.w classDigit
	cmpi.b #5, d0
	beq.w classQuote
	bra programLoop

classWhitespace
	; Class 1 is intentionally tiny in this first slice: only inline space
	; and tab are skipped by the demo loop because CR/LF are rejected up front.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tkvmIsWhitespace
	tst.l d0
	beq programLoop
	bra applyClassJump

classIdentStart
	; Class 2 mirrors the Rust identifier-start mask used by the default
	; tokenizer VM policy for ASCII letters, underscore, and dot.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tkvmIsIdentifierStart
	tst.l d0
	beq programLoop
	bra applyClassJump

classIdentContinue
	; Class 3 is wider than the start class so identifiers can continue with
	; digits and assembler-flavored suffix bytes such as '$' and '@'.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tkvmIsIdentifierContinue
	tst.l d0
	beq programLoop
	bra applyClassJump

classDigit
	; Class 4 is kept inline because the Rust helper ultimately reduces to an
	; ASCII digit check for the default family tokenizer program.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	cmpi.b #'0', d0
	blo programLoop
	cmpi.b #'9', d0
	bhi programLoop
	bra applyClassJump

classQuote
	; Class 5 delegates to the same quote-set logic reused by string scanning.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr tkvmIsQuoteChar
	tst.l d0
	beq programLoop

applyClassJump
	move.l LOCAL_TEMP_U32(a2), d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	lea 0(a3, d0.l), a0
	bra programLoop

; The scan helpers below mirror vm_scan_identifier_token,
; vm_scan_number_token, vm_scan_string_token, and vm_scan_symbol_token in
; tokenizer_runtime_utils.rs. The helper bodies reuse A0, so the interpreter
; saves and restores the native program counter around each call.
opcodeScanIdentifier
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr scanIdentifierToken
	tst.l d0
	bne return
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra programLoop

opcodeScanNumber
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr scanNumberToken
	tst.l d0
	bne return
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra programLoop

opcodeScanString
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr scanStringToken
	tst.l d0
	bne return
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra programLoop

opcodeScanSymbol
	move.l a0, LOCAL_PROGRAM_COUNTER(a2)
	jsr scanSymbolToken
	tst.l d0
	bne return
	movea.l LOCAL_PROGRAM_COUNTER(a2), a0
	bra programLoop

invalidArgument
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #TK_STATUS_INVALID_ARGUMENT, d0
	bra return

vmFailureAtCursor
	moveq #TK_STATUS_VM_FAILURE, d0
	bra return

stepLimitExceeded
	moveq #TK_STATUS_STEP_LIMIT_EXCEEDED, d0
	bra return

lexemeOverflowAtCursor
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	bra return

; Invalid program is reserved for truncated bytecode, bad jump targets, or
; opcode/operand combinations that the native interpreter refuses to execute.
invalidProgramAtCursor
	moveq #TK_STATUS_INVALID_PROGRAM, d0

return
	adda.l #LOCAL_SIZE, sp
	movem.l (sp)+, d4-d7/a4-a6
	rts
	.bend  ; tkvmRun68000

	.priv

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

commitPendingToken	.block
	cmp.l d5, d1  ; token_count < token_capacity
	bcc pendingTokenOverflow
	move.l d3, d0  ; scratch_used + pending_len must stay within scratch_capacity
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bhi pendingLexemeOverflow
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
pendingTokenOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_TOKEN_OVERFLOW, d0
	rts

pendingLexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts
	.bend  ; commitPendingToken

; Stage a fixed lexeme literal from the static data table into scratch.
; This is used for punctuation and operator tokens whose lexeme spelling is
; known upfront and does not need to be copied from the source buffer.
stageFixedLexeme	.block
	move.l d0, LOCAL_PENDING_LEX_LEN(a2)  ; fixed operator/punctuation lexeme length from the inline template string
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bhi lexemeOverflow
	movea.l a6, a1
	adda.l d3, a1
	move.l LOCAL_PENDING_LEX_LEN(a2), d0
stageFixedLexemeLoop
	tst.l d0
	beq stageFixedLexemeDone
	move.b (a0)+, (a1)+  ; copy the canonical lexeme bytes that Rust would expose in PortableToken text/raw
	subq.l #1, d0
	bra stageFixedLexemeLoop

stageFixedLexemeDone
	moveq #TK_STATUS_SUCCESS, d0
	rts

lexemeOverflow
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_LEXEME_OVERFLOW, d0
	rts
	.bend  ; stageFixedLexeme

; ---------------------------------------------------------------------------
; Scanner helpers.
;
; These are the native counterparts of the Rust tokenizer helper routines in
; tokenizer_runtime_utils.rs. Each helper advances D2 as the live source cursor,
; populates LOCAL_PENDING_* metadata, stages lexeme bytes into the scratch
; buffer, then commits a token record.
; ---------------------------------------------------------------------------
scanIdentifierToken	.block
	; Identifier scan is the native mirror of vm_scan_identifier_token():
	; walk identifier-continue bytes, lowercase ASCII letters for the demo
	; policy, then emit one identifier record backed by scratch bytes.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	movea.l a6, a0
	adda.l d3, a0

scanIdentifierLoop
	; D2 stays as the source cursor, A0 walks the next free scratch byte,
	; and LOCAL_PENDING_LEX_LEN grows in lockstep so commit can later write
	; both the source span and scratch payload length into the token record.
	cmp.l d4, d2
	bcc scanIdentifierDone
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tkvmIsIdentifierContinue  ; mirrors vm_matches_identifier_continue_class()
	tst.l d0
	beq scanIdentifierDone
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc pendingLexemeOverflowFromScan
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'A', d0
	blo copyIdentifierByte
	cmpi.b #'Z', d0
	bhi copyIdentifierByte
	ori.b #$20, d0  ; native demo bakes in ASCII-lower identifier normalization used by the Rust bridge tests
copyIdentifierByte
	move.b d0, (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	bra scanIdentifierLoop

scanIdentifierDone
	; Match vm_scan_identifier_token(): a trailing prime belongs to the
	; identifier/register lexeme for Z80 alternate-register spellings like AF'.
	cmp.l d4, d2
	bcc scanIdentifierCommit
	cmpi.b #39, 0(a4, d2.l)
	bne scanIdentifierCommit
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc pendingLexemeOverflowFromScan
	move.b #39, (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)

scanIdentifierCommit
	; Identifier spans are half-open in cursor space and become 1-based only
	; when commitPendingToken serializes them into the native record.
	move.w #TK_KIND_IDENTIFIER, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
	rts
	.bend  ; scanIdentifierToken

scanNumberToken	.block
	; Number scan accepts the same permissive body bytes as the Rust helper,
	; leaving base interpretation to downstream token consumers/report logic.
	move.l d2, LOCAL_PENDING_START(a2)
	clr.l LOCAL_PENDING_LEX_LEN(a2)
	movea.l a6, a0
	adda.l d3, a0

scanNumberLoop
	; Number scanning intentionally keeps the raw source spelling, including
	; prefixes/suffixes/underscores, so later consumers can decide how to
	; interpret base markers just like the Rust runtime does.
	cmp.l d4, d2
	bcc scanNumberDone
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'%', d0
	bne scanNumberCheckBody
	cmp.l LOCAL_PENDING_START(a2), d2
	beq scanNumberAcceptByte
scanNumberCheckBody
	jsr tkvmIsNumberBody  ; same permissive number-body walk as vm_scan_number_token()
	tst.l d0
	beq scanNumberDone
scanNumberAcceptByte
	move.l d3, d0
	add.l LOCAL_PENDING_LEX_LEN(a2), d0
	cmp.l d6, d0
	bcc pendingLexemeOverflowFromScan
	move.b 0(a4, d2.l), (a0)+
	addq.l #1, d2
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	bra scanNumberLoop

scanNumberDone
	move.w #TK_KIND_NUMBER, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
	rts
	.bend  ; scanNumberToken

scanStringToken	.block
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

scanStringLoop
	; Strings advance one payload unit at a time. Plain bytes copy through,
	; while escape sequences normalize into their decoded payload bytes so
	; LEXHEX reflects runtime string contents rather than source spelling.
	cmp.l d4, d2
	bcc scanStringFailure
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmp.l LOCAL_CURRENT_BYTE(a2), d0
	beq scanStringClose
	cmpi.b #'\\', d0  ; decode the same escape surface exercised by vm_scan_string_token()
	bne scanStringCopyLiteral
	addq.l #1, d2
	cmp.l d4, d2
	bcc scanStringFailure
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'n', d0
	beq stringEscapeNewline
	cmpi.b #'r', d0
	beq stringEscapeReturn
	cmpi.b #'t', d0
	beq stringEscapeTab
	cmpi.b #'x', d0  ; \xHH is decoded into one payload byte, just like the Rust helper
	beq stringEscapeHex
	bra scanStringEmitDecoded

stringEscapeNewline
	; The decoded escape value remains in D0 and falls through the shared
	; emit path that appends one payload byte to scratch.
	moveq #10, d0
	bra scanStringEmitDecoded

stringEscapeReturn
	moveq #13, d0
	bra scanStringEmitDecoded

stringEscapeTab
	moveq #9, d0
	bra scanStringEmitDecoded

stringEscapeHex
	; Parse exactly two hex digits after \x and combine them into one byte,
	; mirroring tokenizer_runtime_utils::vm_scan_string_token().
	addq.l #1, d2
	cmp.l d4, d2
	bcc scanStringFailure
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tkvmHexDigitValue
	tst.l d0
	bmi scanStringFailure
	move.l d0, LOCAL_TEMP_U32(a2)
	addq.l #1, d2
	cmp.l d4, d2
	bcc scanStringFailure
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tkvmHexDigitValue
	tst.l d0
	bmi scanStringFailure
	move.l d1, -(sp)
	move.l LOCAL_TEMP_U32(a2), d1
	lsl.l #4, d1
	or.l d1, d0
	move.l (sp)+, d1

	bra scanStringEmitDecoded

scanStringCopyLiteral
	; Literal non-escape bytes use the same capacity accounting as decoded
	; escapes so both paths feed one consistent scratch payload stream.
	move.l d1, -(sp)
	move.l d3, d1
	add.l LOCAL_PENDING_LEX_LEN(a2), d1
	cmp.l d6, d1
	bcc scanStringLiteralOverflow
	move.l (sp)+, d1
	bra scanStringEmitDecoded

scanStringLiteralOverflow
	move.l (sp)+, d1
	bra pendingLexemeOverflowFromScan

scanStringEmitDecoded
	move.b d0, (a0)+
	addq.l #1, LOCAL_PENDING_LEX_LEN(a2)
	addq.l #1, d2
	bra scanStringLoop

scanStringClose
	; D2 is advanced past the closing delimiter before commit so the source
	; span matches the Rust token span semantics for quoted strings.
	addq.l #1, d2
	move.w #TK_KIND_STRING, LOCAL_PENDING_KIND(a2)
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
	rts

; Unterminated strings and malformed escape sequences both collapse to the same
; VM failure status in this first native slice.
scanStringFailure
	moveq #TK_STATUS_VM_FAILURE, d0
	rts
	.bend  ; scanStringToken

; Symbol scan covers punctuation, operators, comments, and prefixed numeric
; forms. The structure intentionally parallels vm_scan_symbol_token() in Rust:
; dispatch by lead byte, optionally consume a longer form, then commit the
; canonical lexeme bytes through tkvmStageAndCommitSymbol.
scanSymbolToken	.block
	; The dispatch order matters. More syntactically specific lead bytes are
	; tested before generic operator fallbacks so multi-byte forms get the
	; same precedence as in the Rust helper.
	move.l d2, LOCAL_PENDING_START(a2)
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #';', d0
	beq tkvmScanCommentToEol
	cmpi.b #'.', d0
	beq tkvmScanDotLike
	cmpi.b #'$', d0
	beq tkvmScanDollarOrPrefixedNumber
	cmpi.b #'%', d0
	beq tkvmScanPercentOrPrefixedNumber
	cmpi.b #'#', d0
	beq tkvmStageHash
	cmpi.b #'?', d0
	beq tkvmStageQuestion
	cmpi.b #'[', d0
	beq tkvmStageOpenBracket
	cmpi.b #']', d0
	beq tkvmStageCloseBracket
	cmpi.b #'{', d0
	beq tkvmStageOpenBrace
	cmpi.b #'}', d0
	beq tkvmStageCloseBrace
	cmpi.b #',', d0
	beq tkvmStageComma
	cmpi.b #':', d0
	beq tkvmStageColon
	cmpi.b #'(', d0
	beq tkvmStageOpenParen
	cmpi.b #')', d0
	beq tkvmStageCloseParen
	cmpi.b #'+', d0
	beq tkvmStagePlus
	cmpi.b #'-', d0
	beq tkvmStageMinus
	cmpi.b #'*', d0
	beq tkvmScanStarLike
	cmpi.b #'/', d0
	beq tkvmStageDivide
	cmpi.b #'~', d0
	beq tkvmStageBitNot
	cmpi.b #'=', d0
	beq tkvmScanEqualLike
	cmpi.b #'!', d0
	beq tkvmScanBangLike
	cmpi.b #'&', d0
	beq tkvmScanAndLike
	cmpi.b #'|', d0
	beq tkvmScanOrLike
	cmpi.b #'^', d0
	beq tkvmScanCaretLike
	cmpi.b #'<', d0
	beq tkvmScanLessLike
	cmpi.b #'>', d0
	beq tkvmScanGreaterLike
	move.w #TK_VM_FAILURE_KIND_FAIL, TkvmLastFailureKind
	move.w d0, TkvmLastFailureOperand
	moveq #TK_STATUS_VM_FAILURE, d0
	rts

tkvmScanCommentToEol
	move.l d4, d2  ; comments consume the rest of the line and emit no token, matching vm_scan_symbol_token()
	moveq #TK_STATUS_SUCCESS, d0
	rts

tkvmScanDotLike
	; '.', '..', and '..=' are grouped together so the operator family stays
	; adjacent in both the native and Rust scanner implementations.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageDot
	cmpi.b #'.', 0(a4, d2.l)
	bne tkvmStageDot
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageRange
	cmpi.b #'=', 0(a4, d2.l)
	bne tkvmStageRange
	addq.l #1, d2
	move.w #TK_KIND_OP_RANGE_INCLUSIVE, LOCAL_PENDING_KIND(a2)
	lea LexRangeInclusive, a0
	moveq #3, d0
	bra tkvmStageAndCommitSymbol

tkvmStageRange
	; '..' and '..=' share the same entry path so the inclusive form only
	; needs one extra lookahead byte and a different fixed lexeme template.
	move.w #TK_KIND_OP_RANGE, LOCAL_PENDING_KIND(a2)
	lea LexRange, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageDot
	move.w #TK_KIND_DOT, LOCAL_PENDING_KIND(a2)
	lea LexDot, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanDollarOrPrefixedNumber
	; '$' is ambiguous by design: either a standalone dollar token or the
	; prefix for a hex-like number literal if a valid body byte follows.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageDollar
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	jsr tkvmIsHexDigitOrUnderscore  ; '$' starts either a hex literal or a standalone dollar token
	tst.l d0
	beq tkvmStageDollar
	move.l LOCAL_PENDING_START(a2), d2
	jsr scanNumberToken
	rts

tkvmStageDollar
	move.w #TK_KIND_DOLLAR, LOCAL_PENDING_KIND(a2)
	lea LexDollar, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanPercentOrPrefixedNumber
	; '%' is likewise split between modulo and binary-prefixed number forms.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStagePercent
	moveq #0, d0
	move.b 0(a4, d2.l), d0
	cmpi.b #'0', d0
	beq tkvmScanPercentAsNumber
	cmpi.b #'1', d0
	bne tkvmStagePercent

tkvmScanPercentAsNumber
	jsr tkvmPercentHasPrefixContext
	tst.l d0
	beq tkvmStagePercent
	move.l LOCAL_PENDING_START(a2), d2  ; rewind so the number scanner sees the leading '%', like Rust prefixed-number handling
	jsr scanNumberToken
	rts

tkvmStagePercent
	move.w #TK_KIND_OP_MOD, LOCAL_PENDING_KIND(a2)
	lea LexMod, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageHash
	addq.l #1, d2
	move.w #TK_KIND_HASH, LOCAL_PENDING_KIND(a2)
	lea LexHash, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageQuestion
	; The single-byte staging labels below follow one repeated pattern:
	; advance the source cursor, choose a token kind, point at the canonical
	; lexeme bytes, set the lexeme length, then funnel through the shared
	; stage-and-commit tail.
	addq.l #1, d2
	move.w #TK_KIND_QUESTION, LOCAL_PENDING_KIND(a2)
	lea LexQuestion, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageOpenBracket
	addq.l #1, d2
	move.w #TK_KIND_OPEN_BRACKET, LOCAL_PENDING_KIND(a2)
	lea LexOpenBracket, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageCloseBracket
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_BRACKET, LOCAL_PENDING_KIND(a2)
	lea LexCloseBracket, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageOpenBrace
	addq.l #1, d2
	move.w #TK_KIND_OPEN_BRACE, LOCAL_PENDING_KIND(a2)
	lea LexOpenBrace, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageCloseBrace
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_BRACE, LOCAL_PENDING_KIND(a2)
	lea LexCloseBrace, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageComma
	addq.l #1, d2
	move.w #TK_KIND_COMMA, LOCAL_PENDING_KIND(a2)
	lea LexComma, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageColon
	addq.l #1, d2
	move.w #TK_KIND_COLON, LOCAL_PENDING_KIND(a2)
	lea LexColon, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageOpenParen
	addq.l #1, d2
	move.w #TK_KIND_OPEN_PAREN, LOCAL_PENDING_KIND(a2)
	lea LexOpenParen, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageCloseParen
	addq.l #1, d2
	move.w #TK_KIND_CLOSE_PAREN, LOCAL_PENDING_KIND(a2)
	lea LexCloseParen, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStagePlus
	addq.l #1, d2
	move.w #TK_KIND_OP_PLUS, LOCAL_PENDING_KIND(a2)
	lea LexPlus, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageMinus
	addq.l #1, d2
	move.w #TK_KIND_OP_MINUS, LOCAL_PENDING_KIND(a2)
	lea LexMinus, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanStarLike
	; '*' and '**' match the Rust tokenizer's multiply/power split.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageMultiply
	cmpi.b #'*', 0(a4, d2.l)
	beq tkvmStagePower

tkvmStageMultiply
	move.w #TK_KIND_OP_MULTIPLY, LOCAL_PENDING_KIND(a2)
	lea LexMultiply, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStagePower
	addq.l #1, d2
	move.w #TK_KIND_OP_POWER, LOCAL_PENDING_KIND(a2)
	lea LexPower, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageDivide
	addq.l #1, d2
	move.w #TK_KIND_OP_DIVIDE, LOCAL_PENDING_KIND(a2)
	lea LexDivide, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmStageBitNot
	addq.l #1, d2
	move.w #TK_KIND_OP_BIT_NOT, LOCAL_PENDING_KIND(a2)
	lea LexBitNot, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanEqualLike
	; '=' and '==' both normalize to the same equality token kind, matching
	; the Rust tokenizer helper's forgiving equality parsing.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageEq
	cmpi.b #'=', 0(a4, d2.l)
	bne tkvmStageEq
	addq.l #1, d2

tkvmStageEq
	; The canonical fixed lexeme is always "==" for equality so report output
	; normalizes '=' and '==' into one operator surface.
	move.w #TK_KIND_OP_EQ, LOCAL_PENDING_KIND(a2)
	lea LexEq, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmScanBangLike
	; '!' stands for logical-not, while '!=' upgrades to not-equal.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageLogicNot
	cmpi.b #'=', 0(a4, d2.l)
	bne tkvmStageLogicNot
	addq.l #1, d2
	move.w #TK_KIND_OP_NE, LOCAL_PENDING_KIND(a2)
	lea LexNe, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageLogicNot
	; Unlike equality, logical-not preserves its single-byte spelling in the
	; report/output surface because '!' and '!=' are distinct token kinds.
	move.w #TK_KIND_OP_LOGIC_NOT, LOCAL_PENDING_KIND(a2)
	lea LexLogicNot, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanAndLike
	; '&' / '&&' map to bitwise and logical forms respectively.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageBitAnd
	cmpi.b #'&', 0(a4, d2.l)
	bne tkvmStageBitAnd
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_AND, LOCAL_PENDING_KIND(a2)
	lea LexLogicAnd, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageBitAnd
	move.w #TK_KIND_OP_BIT_AND, LOCAL_PENDING_KIND(a2)
	lea LexBitAnd, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanOrLike
	; '|' / '||' map to bitwise and logical forms respectively.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageBitOr
	cmpi.b #'|', 0(a4, d2.l)
	bne tkvmStageBitOr
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_OR, LOCAL_PENDING_KIND(a2)
	lea LexLogicOr, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageBitOr
	move.w #TK_KIND_OP_BIT_OR, LOCAL_PENDING_KIND(a2)
	lea LexBitOr, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanCaretLike
	; '^' is bitwise xor, while '^^' is promoted to the logical xor
	; token used by the native report-name table.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageBitXor
	cmpi.b #'^', 0(a4, d2.l)
	bne tkvmStageBitXor
	addq.l #1, d2
	move.w #TK_KIND_OP_LOGIC_XOR, LOCAL_PENDING_KIND(a2)
	lea LexLogicXor, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageBitXor
	move.w #TK_KIND_OP_BIT_XOR, LOCAL_PENDING_KIND(a2)
	lea LexBitXor, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanLessLike
	; '<' expands into four related operators: <, <<, <=, and <>/!=.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageLt
	cmpi.b #'<', 0(a4, d2.l)
	beq tkvmStageShl
	cmpi.b #'=', 0(a4, d2.l)
	beq tkvmStageLe
	cmpi.b #'>', 0(a4, d2.l)
	beq tkvmStageAltNe
	bra tkvmStageLt

tkvmStageShl
	addq.l #1, d2
	move.w #TK_KIND_OP_SHL, LOCAL_PENDING_KIND(a2)
	lea LexShl, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageLe
	addq.l #1, d2
	move.w #TK_KIND_OP_LE, LOCAL_PENDING_KIND(a2)
	lea LexLe, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageAltNe
	addq.l #1, d2
	move.w #TK_KIND_OP_NE, LOCAL_PENDING_KIND(a2)
	lea LexNe, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageLt
	; The family labels above all converge here with a fully-chosen token
	; kind and lexeme template, so the commit tail can stay generic.
	move.w #TK_KIND_OP_LT, LOCAL_PENDING_KIND(a2)
	lea LexLt, a0
	moveq #1, d0
	bra tkvmStageAndCommitSymbol

tkvmScanGreaterLike
	; '>' expands into >, >>, and >=.
	addq.l #1, d2
	cmp.l d4, d2
	bcc tkvmStageGt
	cmpi.b #'>', 0(a4, d2.l)
	beq tkvmStageShr
	cmpi.b #'=', 0(a4, d2.l)
	beq tkvmStageGe
	bra tkvmStageGt

tkvmStageShr
	addq.l #1, d2
	move.w #TK_KIND_OP_SHR, LOCAL_PENDING_KIND(a2)
	lea LexShr, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageGe
	addq.l #1, d2
	move.w #TK_KIND_OP_GE, LOCAL_PENDING_KIND(a2)
	lea LexGe, a0
	moveq #2, d0
	bra tkvmStageAndCommitSymbol

tkvmStageGt
	move.w #TK_KIND_OP_GT, LOCAL_PENDING_KIND(a2)
	lea LexGt, a0
	moveq #1, d0

tkvmStageAndCommitSymbol
	; At this point LOCAL_PENDING_START already marks the source span start,
	; D2 already points just past the consumed source bytes, and A0/D0 name
	; the canonical lexeme bytes to materialize into scratch.
	jsr stageFixedLexeme  ; stage the canonical lexeme bytes before committing the token metadata
	tst.l d0
	bne tkvmStageAndCommitSymbolDone
	move.l d2, LOCAL_PENDING_END(a2)
	jsr commitPendingToken
tkvmStageAndCommitSymbolDone
	rts

; Used when symbol lookahead discovers a shape the native bytecode contract does
; not allow, such as the unsupported '**' power operator.
tkvmScanSymbolInvalidProgram
	move.l LOCAL_PENDING_START(a2), d2
	moveq #TK_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; scanSymbolToken

; Shared overflow exit for any scanner that would exceed scratch capacity.
pendingLexemeOverflowFromScan
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
tkvmIsWhitespace	.block
	cmpi.b #' ', d0  ; this line-input slice only treats space and tab as intra-line whitespace
	beq tkvmPredicateTrue
	cmpi.b #9, d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsWhitespace

tkvmIsIdentifierStart	.block
	; These predicate chains intentionally avoid lookup tables so the native
	; implementation stays easy to audit against the Rust helper masks.
	cmpi.b #'A', d0
	blo tkvmCheckIdentStartLower
	cmpi.b #'Z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentStartLower
	cmpi.b #'a', d0
	blo tkvmCheckIdentStartPunct
	cmpi.b #'z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentStartPunct
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	cmpi.b #'.', d0  ; '.' remains identifier-start-capable because the runtime class mask includes it
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsIdentifierStart

tkvmIsIdentifierContinue	.block
	cmpi.b #'A', d0
	blo tkvmCheckIdentContinueLower
	cmpi.b #'Z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentContinueLower
	cmpi.b #'a', d0
	blo tkvmCheckIdentContinueDigit
	cmpi.b #'z', d0
	bls tkvmPredicateTrue
tkvmCheckIdentContinueDigit
	cmpi.b #'0', d0
	blo tkvmCheckIdentExtra
	cmpi.b #'9', d0
	bls tkvmPredicateTrue
tkvmCheckIdentExtra
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	cmpi.b #'.', d0
	beq tkvmPredicateTrue
	cmpi.b #'$', d0  ; '$' and '@' stay valid continue bytes per tokenizer_runtime_utils.rs masks
	beq tkvmPredicateTrue
	cmpi.b #'@', d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsIdentifierContinue

tkvmIsQuoteChar	.block
	cmpi.b #'"', d0  ; demo program accepts both quote styles, matching the Rust helper's quote-char set
	beq tkvmPredicateTrue
	cmpi.b #39, d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsQuoteChar

tkvmIsNumberBody	.block
	; Number bodies are deliberately permissive at scan time. Validation of
	; bases and suffix meaning is deferred to later consumers, matching the
	; Rust tokenizer helper contract.
	cmpi.b #'0', d0
	blo tkvmCheckNumberLetters
	cmpi.b #'9', d0
	bls tkvmPredicateTrue
tkvmCheckNumberLetters
	cmpi.b #'A', d0
	blo tkvmCheckNumberLower
	cmpi.b #'Z', d0
	bls tkvmPredicateTrue
tkvmCheckNumberLower
	cmpi.b #'a', d0
	blo tkvmCheckNumberExtra
	cmpi.b #'z', d0
	bls tkvmPredicateTrue
tkvmCheckNumberExtra
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	cmpi.b #'$', d0
	beq tkvmPredicateTrue
	cmpi.b #'%', d0
	beq tkvmPredicateTrue
	cmpi.b #'@', d0
	beq tkvmPredicateTrue
	moveq #0, d0
	rts
	.bend  ; tkvmIsNumberBody

tkvmIsHexDigitOrUnderscore	.block
	; Used only as a fast probe for deciding whether '$' begins a number or
	; remains a standalone token.
	cmpi.b #'_', d0
	beq tkvmPredicateTrue
	jsr tkvmHexDigitValue
	tst.l d0
	bmi tkvmPredicateFalse
	bra tkvmPredicateTrue
	.bend  ; tkvmIsHexDigitOrUnderscore

tkvmPredicateTrue
	moveq #1, d0
	rts

tkvmPredicateFalse
	moveq #0, d0
	rts

; Shared hex nibble decoder for both string escape parsing and '$'-prefixed
; number probing. Returns -1 for non-hex input so callers can branch cleanly.
tkvmHexDigitValue	.block
	cmpi.b #'0', d0  ; shared nibble decoder for \xHH strings and '$'-prefixed number probing
	blo tkvmHexUpper
	cmpi.b #'9', d0
	bhi tkvmHexUpper
	subi.b #'0', d0
	andi.l #$FF, d0
	rts

tkvmHexUpper
	cmpi.b #'A', d0
	blo tkvmHexLower
	cmpi.b #'F', d0
	bhi tkvmHexLower
	subi.b #'A', d0
	addi.b #10, d0
	andi.l #$FF, d0
	rts

tkvmHexLower
	cmpi.b #'a', d0
	blo tkvmHexInvalid
	cmpi.b #'f', d0
	bhi tkvmHexInvalid
	subi.b #'a', d0
	addi.b #10, d0
	andi.l #$FF, d0
	rts

tkvmHexInvalid
	moveq #-1, d0
	rts
	.bend  ; tkvmHexDigitValue

; Rust treats '%' as a binary-number prefix only when the byte appears where an
; expression can start. Without that context, % remains the modulo operator.
tkvmPercentHasPrefixContext	.block
	move.l LOCAL_PENDING_START(a2), d0
	beq tkvmPercentPrefixTrue

	clr.w LOCAL_PENDING_KIND(a2)
	clr.l LOCAL_TEMP_U32(a2)
	subq.l #1, d0
	move.l d0, LOCAL_TEMP_U32(a2)
	lea 0(a4, d0.l), a1
	moveq #0, d0
	move.b (a1), d0
	cmpi.b #' ', d0
	beq tkvmPercentMarkLeadingSpace
	cmpi.b #9, d0
	bne tkvmPercentCheckPrevNonSpaceByte

tkvmPercentMarkLeadingSpace
	moveq #1, d0
	move.w d0, LOCAL_PENDING_KIND(a2)

tkvmPercentPrevNonSpaceLoop
	move.l LOCAL_TEMP_U32(a2), d0
	tst.l d0
	beq tkvmPercentPrefixTrue
	subq.l #1, d0
	move.l d0, LOCAL_TEMP_U32(a2)
	lea 0(a4, d0.l), a1
	moveq #0, d0
	move.b (a1), d0

tkvmPercentCheckPrevNonSpaceByte
	cmpi.b #' ', d0
	beq tkvmPercentPrevNonSpaceLoop
	cmpi.b #9, d0
	beq tkvmPercentPrevNonSpaceLoop
	cmpi.b #'(', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #',', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'+', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'-', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'*', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'/', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'%', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'&', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'|', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'^', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'~', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'!', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'<', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'>', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'=', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #'?', d0
	beq tkvmPercentPrefixTrue
	cmpi.b #':', d0
	beq tkvmPercentPrefixTrue

	tst.w LOCAL_PENDING_KIND(a2)
	beq tkvmPercentPrefixFalse
	jsr tkvmIsIdentifierContinue
	tst.l d0
	bne tkvmPercentPrefixTrue

tkvmPercentPrefixFalse
	moveq #0, d0
	rts

tkvmPercentPrefixTrue
	moveq #1, d0
	rts
	.bend  ; tkvmPercentHasPrefixContext

	.endsection
	.section data, kind=data
	.pub

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

; Canonical lexeme spellings used by tkvmStageFixedLexeme.
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
