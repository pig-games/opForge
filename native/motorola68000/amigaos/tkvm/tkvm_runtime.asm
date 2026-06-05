; Native tokenizer VM runtime used by tkpkg and test harnesses.
;
; Owns the runtime-model constants and interpreter loop.

	.module tkvm.amigaos.runtime
	.cpu 68020
	.pub
	.use tkvm.amigaos.state
	.use tkvm.amigaos.char_predicates
	.use tkvm.amigaos.scanner

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

TOKEN_RECORD_SIZE               = 20
SOURCE_BUFFER_CAPACITY          = 1024
TOKEN_BUFFER_CAPACITY           = 64
SCRATCH_BUFFER_CAPACITY         = 1024

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
LOCAL_OPCODE_CURSOR             = 24
LOCAL_STEP_COUNT                = 28
LOCAL_STEP_LIMIT                = 32
LOCAL_SIZE                      = 36

	.section code, kind=code

	.pub

; ---------------------------------------------------------------------------
; Native tokenizer VM interpreter.
;
; Inputs:
; - A0: source buffer pointer.
; - A1: token record output buffer pointer.
; - A2: scratch/lexeme buffer pointer.
; - A3: tokenizer bytecode program pointer.
; - D0: source byte length.
; - D1: token capacity in 20-byte records.
; - D2: scratch capacity in bytes.
; - D3: tokenizer bytecode length in bytes.
;
; Outputs:
; - D0: TK_STATUS_* result.
; - D1: emitted token count on success or partial progress.
; - D2: status-specific cursor/column value on failures that report one.
; - D3: committed scratch bytes on success or partial progress.
;
; Clobbers:
; - D0-D7/A0-A6/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
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
	move.l state.TkvmStepBudget, d0
	move.l d0, LOCAL_STEP_LIMIT(a2)
	moveq #-1, d0  ; sentinel current byte = EOF until ReadChar runs
	move.l d0, LOCAL_CURRENT_BYTE(a2)
	clr.w state.TkvmLastFailureKind
	clr.w state.TkvmLastFailureOperand

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
	beq invalidArgument

checkTokenPointer
	tst.l d5  ; non-zero token capacity requires a writable token buffer
	beq checkScratchPointer
	move.l a5, d0
	beq invalidArgument

checkScratchPointer
	tst.l d6  ; non-zero scratch capacity requires a writable scratch buffer
	beq checkProgramPointer
	move.l a6, d0
	beq invalidArgument

checkProgramPointer
	tst.l d7  ; bytecode length 0 cannot encode a valid tokenizer program
	beq invalidProgramAtCursor
	move.l a3, d0
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
	move.w state.TkvmProgramStartState, d0
	cmp.l state.TkvmProgramStateCount, d0
	bcc invalidProgramAtCursor
	move.l state.TkvmProgramStateTablePtr, d1
	beq.w invalidProgramAtCursor
	movea.l d1, a1
	add.l d0, d0
	add.l d0, d0
	adda.l d0, a1
	move.l (a1), d0
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
	move.l a0, LOCAL_OPCODE_CURSOR(a2)
	jsr scanner.commitPendingToken
	bne return
	movea.l LOCAL_OPCODE_CURSOR(a2), a0
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
	cmp.l state.TkvmProgramStateCount, d0
	bcc invalidProgramAtCursor
	move.l state.TkvmProgramStateTablePtr, d1
	beq.w invalidProgramAtCursor
	movea.l d1, a1
	add.l d0, d0
	add.l d0, d0
	adda.l d0, a1
	move.l (a1), d0
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
	move.w #TK_VM_FAILURE_KIND_FAIL, state.TkvmLastFailureKind
	move.w d0, state.TkvmLastFailureOperand
	bra vmFailureAtCursor

opcodeEmitDiag
	move.l a0, d0
	sub.l a3, d0
	addq.l #1, d0
	cmp.l d7, d0
	bhi invalidProgramAtCursor
	moveq #0, d0
	move.b (a0)+, d0
	move.w #TK_VM_FAILURE_KIND_EMIT_DIAG, state.TkvmLastFailureKind
	move.w d0, state.TkvmLastFailureOperand
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
	jsr char_predicates.tkvmIsWhitespace
	beq programLoop
	bra applyClassJump

classIdentStart
	; Class 2 mirrors the Rust identifier-start mask used by the default
	; tokenizer VM policy for ASCII letters, underscore, and dot.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr char_predicates.tkvmIsIdentifierStart
	beq programLoop
	bra applyClassJump

classIdentContinue
	; Class 3 is wider than the start class so identifiers can continue with
	; digits and assembler-flavored suffix bytes such as '$' and '@'.
	move.l LOCAL_CURRENT_BYTE(a2), d0
	jsr char_predicates.tkvmIsIdentifierContinue
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
	jsr char_predicates.tkvmIsQuoteChar
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
	move.l a0, LOCAL_OPCODE_CURSOR(a2)
	jsr scanner.scanIdentifierToken
	bne return
	movea.l LOCAL_OPCODE_CURSOR(a2), a0
	bra programLoop

opcodeScanNumber
	move.l a0, LOCAL_OPCODE_CURSOR(a2)
	jsr scanner.scanNumberToken
	bne return
	movea.l LOCAL_OPCODE_CURSOR(a2), a0
	bra programLoop

opcodeScanString
	move.l a0, LOCAL_OPCODE_CURSOR(a2)
	jsr scanner.scanStringToken
	bne return
	movea.l LOCAL_OPCODE_CURSOR(a2), a0
	bra programLoop

opcodeScanSymbol
	move.l a0, LOCAL_OPCODE_CURSOR(a2)
	jsr scanner.scanSymbolToken
	bne return
	movea.l LOCAL_OPCODE_CURSOR(a2), a0
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

	.endsection
	.endmodule
