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

tokvmStepBudget:
        .long TOKVM_DEFAULT_MAX_STEPS_PER_LINE

demoStateEntryOffsets:
        .long DEMO_PC_READ_CHAR

tokvmProgramStateTablePtr:
        .long demoStateEntryOffsets

tokvmProgramStateCount:
        .long 1

tokvmProgramStartState:
        .word 0

tokvmLastFailureKind:
        .word 0

tokvmLastFailureOperand:
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
tokvm_set_step_budget_68000:
        TST.L D0
        BGT.S tokvmSetStepBudgetStore
        MOVE.L #TOKVM_DEFAULT_MAX_STEPS_PER_LINE, D0
tokvmSetStepBudgetStore:
        MOVE.L D0, tokvmStepBudget
        RTS

; Install a package-provided state table; invalid counts fall back to demo state 0.
tokvm_set_program_state_table_68000:
        TST.L D0
        BGT.S tokvmSetProgramStateStore
        LEA demoStateEntryOffsets, A0
        MOVEQ #1, D0
        MOVEQ #0, D1
tokvmSetProgramStateStore:
        MOVE.L A0, tokvmProgramStateTablePtr
        MOVE.L D0, tokvmProgramStateCount
        MOVE.W D1, tokvmProgramStartState
        RTS

; Return the last explicit VM failure kind/operand captured by tokvm_run_68000.
tokvm_read_last_failure_68000:
        MOVEQ #0, D0
        MOVE.W tokvmLastFailureKind, D0
        MOVEQ #0, D1
        MOVE.W tokvmLastFailureOperand, D1
        RTS

tokvm_run_68000:
        MOVEM.L D4-D7/A4-A6, -(SP)
        MOVEA.L A2, A6  ; preserve scratch base separately so A2 can become the interpreter-local frame pointer
        MOVEA.L A0, A4  ; source bytes base, equivalent to VmTokenizerInputStream.bytes
        MOVEA.L A1, A5  ; token record output base
        MOVE.L D0, D4  ; source byte length / maximum cursor
        MOVE.L D1, D5  ; token capacity in 20-byte records
        MOVE.L D2, D6  ; scratch capacity in bytes
        MOVE.L D3, D7  ; bytecode length in bytes
        SUBA.L #LOCAL_SIZE, SP  ; local spill area for pending token metadata and saved PC
        LEA 0(SP), A2  ; A2 now points at LOCAL_* slots for the duration of interpretation

        CLR.L D1  ; emitted token count starts at 0
        CLR.L D2  ; source cursor starts at column 1 / byte 0
        CLR.L D3  ; scratch bytes committed starts at 0
        CLR.L LOCAL_STEP_COUNT(A2)
        MOVE.L tokvmStepBudget, D0
        MOVE.L D0, LOCAL_STEP_LIMIT(A2)
        MOVEQ #-1, D0  ; sentinel current byte = EOF until ReadChar runs
        MOVE.L D0, LOCAL_CURRENT_BYTE(A2)
        CLR.W tokvmLastFailureKind
        CLR.W tokvmLastFailureOperand

        TST.L D4  ; reject negative lengths/capacities before dereferencing any caller pointers
        BMI tokvmInvalidArgument
        TST.L D5
        BMI tokvmInvalidArgument
        TST.L D6
        BMI tokvmInvalidArgument
        TST.L D7
        BMI tokvmInvalidArgument

        TST.L D4  ; non-empty source requires a non-null source pointer
        BEQ tokvmCheckTokenPointer
        MOVE.L A4, D0
        TST.L D0
        BEQ tokvmInvalidArgument

tokvmCheckTokenPointer:
        TST.L D5  ; non-zero token capacity requires a writable token buffer
        BEQ tokvmCheckScratchPointer
        MOVE.L A5, D0
        TST.L D0
        BEQ tokvmInvalidArgument

tokvmCheckScratchPointer:
        TST.L D6  ; non-zero scratch capacity requires a writable scratch buffer
        BEQ tokvmCheckProgramPointer
        MOVE.L A6, D0
        TST.L D0
        BEQ tokvmInvalidArgument

tokvmCheckProgramPointer:
        TST.L D7  ; bytecode length 0 cannot encode a valid tokenizer program
        BEQ tokvmInvalidProgramAtCursor
        MOVE.L A3, D0
        TST.L D0
        BEQ tokvmInvalidArgument

        MOVEQ #0, D0  ; proactively reject CR/LF because this slice models one line-input stream only
tokvmNewlineScanLoop:
        CMP.L D4, D0
        BCC tokvmNewlineScanDone
        CMPI.B #10, 0(A4, D0.L)
        BEQ tokvmNewlineUnsupported
        CMPI.B #13, 0(A4, D0.L)
        BEQ tokvmNewlineUnsupported
        ADDQ.L #1, D0
        BRA tokvmNewlineScanLoop

tokvmNewlineUnsupported:
        MOVE.L D0, D2
        MOVEQ #TK_STATUS_NEWLINE_UNSUPPORTED, D0
        BRA tokvmReturn

tokvmNewlineScanDone:
        MOVEQ #0, D0
        MOVE.W tokvmProgramStartState, D0
        CMP.L tokvmProgramStateCount, D0
        BCC tokvmInvalidProgramAtCursor
        MOVE.L tokvmProgramStateTablePtr, D1
        TST.L D1
        BEQ tokvmInvalidProgramAtCursor
        MOVEA.L D1, A1
        MOVE.L 0.W(A1, D0.L*4), D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        LEA 0(A3, D0.L), A0
        LEA 0(A3, D7.L), A1
        CLR.L D1  ; token count must still enter the first loop iteration as 0

; Main bytecode dispatch loop.
; A0 is the native program counter, A1 is the bytecode end pointer, D2 is the
; source cursor, and LOCAL_CURRENT_BYTE stores the last ReadChar result. This
; corresponds directly to the Rust match over TokenizerVmOpcode.
tokvmProgramLoop:
        MOVE.L LOCAL_STEP_COUNT(A2), D0
        ADDQ.L #1, D0
        MOVE.L D0, LOCAL_STEP_COUNT(A2)
        CMP.L LOCAL_STEP_LIMIT(A2), D0
        BHI tokvmStepLimitExceeded
        LEA 0(A3, D7.L), A1
        CMP.L D4, D2
        BHI tokvmInvalidProgramAtCursor
        CMPA.L A1, A0
        BCC tokvmInvalidProgramAtCursor

tokvmProgramLoopDispatchOpcode:
        MOVEQ #0, D0
        MOVE.B (A0)+, D0

        ; The native slice only implements opcode values 0..18.
        ; Unsupported shared VM slots still get explicit table entries so the
        ; opcode-to-handler mapping stays visible and future additions stay local.
        CMPI.B #TK_OPCODE_SCAN_SYMBOL, D0
        BHI tokvmInvalidProgramAtCursor
        ADD.W D0, D0
        ADD.W D0, D0
        LEA tokvmOpcodeDispatchTable(PC), A1
        MOVEA.L 0(A1, D0.W), A1
        JMP (A1)

tokvmOpcodeDispatchTable:
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

tokvmOpcodeEnd:
        CMP.L D4, D2  ; Rust runtime also only accepts END when the source cursor is at EOL
        BNE tokvmInvalidProgramAtCursor
        MOVEQ #TK_STATUS_SUCCESS, D0
        BRA tokvmReturn

        ; ReadChar mirrors VmTokenizerInputStream.current_byte(): live bytes are zero-extended
        ; into D0 and only EOF uses the -1 sentinel stored in LOCAL_CURRENT_BYTE.
tokvmOpcodeReadChar:
        MOVEQ #0, D0
        CMP.L D4, D2
        BCC tokvmStoreEofByte
        MOVE.B 0(A4, D2.L), D0
        BRA tokvmStoreCurrentByte
tokvmStoreEofByte:
        MOVEQ #-1, D0
tokvmStoreCurrentByte:
        MOVE.L D0, LOCAL_CURRENT_BYTE(A2)

        BRA tokvmProgramLoop

tokvmOpcodeAdvance:
        CMP.L D4, D2  ; advance saturates at EOL, same as VmTokenizerInputStream.advance()
        BCC tokvmProgramLoop
        ADDQ.L #1, D2
        BRA tokvmProgramLoop

tokvmOpcodeStartLexeme:
        CLR.L LOCAL_PENDING_LEX_LEN(A2)
        MOVE.L D2, LOCAL_PENDING_START(A2)
        MOVE.L D2, LOCAL_PENDING_END(A2)
        BRA tokvmProgramLoop

tokvmOpcodePushChar:
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0
        TST.L D0
        BMI tokvmInvalidProgramAtCursor
        MOVE.L D1, LOCAL_TEMP_U32(A2)
        MOVE.L D3, D1
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D1
        CMP.L D6, D1
        BCC tokvmPendingLexemeOverflow
        MOVEA.L A6, A1
        ADDA.L D1, A1
        MOVE.B D0, (A1)
        ADDQ.L #1, LOCAL_PENDING_LEX_LEN(A2)
        MOVE.L D2, D1
        ADDQ.L #1, D1
        MOVE.L D1, LOCAL_PENDING_END(A2)
        MOVE.L LOCAL_TEMP_U32(A2), D1
        BRA tokvmProgramLoop

tokvmOpcodeEmitToken:
        LEA 0(A3, D7.L), A1
        CMPA.L A1, A0
        BCC tokvmInvalidProgramAtCursor
        MOVEQ #0, D0
        MOVE.B (A0)+, D0
        MOVE.W D0, LOCAL_PENDING_KIND(A2)
        JSR tokvmCommitPendingToken
        TST.L D0
        BNE tokvmReturn
        BRA tokvmProgramLoop

tokvmOpcodeSetState:
        MOVE.L A0, D0
        SUB.L A3, D0
        ADDQ.L #2, D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        MOVE.L D1, LOCAL_TEMP_U32(A2)
        MOVEQ #0, D0
        MOVE.B (A0)+, D0
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        LSL.W #8, D1
        OR.W D1, D0
        CMP.L tokvmProgramStateCount, D0
        BCC tokvmInvalidProgramAtCursor
        MOVE.L tokvmProgramStateTablePtr, D1
        TST.L D1
        BEQ tokvmInvalidProgramAtCursor
        MOVEA.L D1, A1
        MOVE.L 0.W(A1, D0.L*4), D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        LEA 0(A3, D0.L), A0
        MOVE.L LOCAL_TEMP_U32(A2), D1
        BRA tokvmProgramLoop

tokvmOpcodeFail:
        MOVE.L A0, D0
        SUB.L A3, D0
        ADDQ.L #1, D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        MOVEQ #0, D0
        MOVE.B (A0)+, D0
        MOVE.W #TK_VM_FAILURE_KIND_FAIL, tokvmLastFailureKind
        MOVE.W D0, tokvmLastFailureOperand
        BRA tokvmVmFailureAtCursor

tokvmOpcodeEmitDiag:
        MOVE.L A0, D0
        SUB.L A3, D0
        ADDQ.L #1, D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        MOVEQ #0, D0
        MOVE.B (A0)+, D0
        MOVE.W #TK_VM_FAILURE_KIND_EMIT_DIAG, tokvmLastFailureKind
        MOVE.W D0, tokvmLastFailureOperand
        BRA tokvmVmFailureAtCursor

tokvmOpcodeJump:
        MOVE.L A0, D0
        SUB.L A3, D0
        ADDQ.L #4, D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        MOVE.L (A0), D0
        ROR.W #8, D0
        SWAP D0
        ROR.W #8, D0
        ADDA.L #4, A0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        LEA 0(A3, D0.L), A0
        BRA tokvmProgramLoop

tokvmOpcodeJumpIfEol:
        MOVE.L A0, D0
        SUB.L A3, D0
        ADDQ.L #4, D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        MOVE.L (A0), D0
        ROR.W #8, D0
        SWAP D0
        ROR.W #8, D0
        ADDA.L #4, A0
        CMP.L D4, D2
        BNE tokvmProgramLoop
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        LEA 0(A3, D0.L), A0
        BRA tokvmProgramLoop

tokvmOpcodeJumpIfByteEq:
        MOVE.L A0, D0
        SUB.L A3, D0
        ADDQ.L #5, D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        MOVEQ #0, D0
        MOVE.B (A0)+, D0  ; operand 0 = byte literal to compare against LOCAL_CURRENT_BYTE
        MOVE.W D0, LOCAL_PENDING_KIND(A2)
        MOVE.L (A0), D0
        ROR.W #8, D0
        SWAP D0
        ROR.W #8, D0
        ADDA.L #4, A0
        MOVE.L D0, LOCAL_TEMP_U32(A2)
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0  ; no jump fires at EOF, matching Rust's Option<u8>-based predicate path
        TST.L D0
        BMI tokvmProgramLoop
        CMP.W LOCAL_PENDING_KIND(A2), D0
        BNE tokvmProgramLoop
        MOVE.L LOCAL_TEMP_U32(A2), D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        LEA 0(A3, D0.L), A0
        BRA tokvmProgramLoop

tokvmOpcodeJumpIfClass:
        MOVE.L A0, D0
        SUB.L A3, D0
        ADDQ.L #5, D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        MOVEQ #0, D0
        MOVE.B (A0)+, D0  ; operand 0 = compact character-class id from builder.rs default demo loop
        MOVE.W D0, LOCAL_PENDING_KIND(A2)
        MOVE.L (A0), D0
        ROR.W #8, D0
        SWAP D0
        ROR.W #8, D0
        ADDA.L #4, A0
        MOVE.L D0, LOCAL_TEMP_U32(A2)
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0  ; EOF never matches a class, same as vm_char_class_matches(None, ...)
        TST.L D0
        BMI tokvmProgramLoop
        MOVEQ #0, D0
        MOVE.W LOCAL_PENDING_KIND(A2), D0
        CMPI.B #1, D0
        BEQ.W tokvmClassWhitespace
        CMPI.B #2, D0
        BEQ.W tokvmClassIdentStart
        CMPI.B #3, D0
        BEQ.W tokvmClassIdentContinue
        CMPI.B #4, D0
        BEQ.W tokvmClassDigit
        CMPI.B #5, D0
        BEQ.W tokvmClassQuote
        BRA tokvmProgramLoop

tokvmClassWhitespace:
        ; Class 1 is intentionally tiny in this first slice: only inline space
        ; and tab are skipped by the demo loop because CR/LF are rejected up front.
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0
        JSR tokvmIsWhitespace
        TST.L D0
        BEQ tokvmProgramLoop
        BRA tokvmApplyClassJump

tokvmClassIdentStart:
        ; Class 2 mirrors the Rust identifier-start mask used by the default
        ; tokenizer VM policy for ASCII letters, underscore, and dot.
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0
        JSR tokvmIsIdentifierStart
        TST.L D0
        BEQ tokvmProgramLoop
        BRA tokvmApplyClassJump

tokvmClassIdentContinue:
        ; Class 3 is wider than the start class so identifiers can continue with
        ; digits and assembler-flavored suffix bytes such as '$' and '@'.
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0
        JSR tokvmIsIdentifierContinue
        TST.L D0
        BEQ tokvmProgramLoop
        BRA tokvmApplyClassJump

tokvmClassDigit:
        ; Class 4 is kept inline because the Rust helper ultimately reduces to an
        ; ASCII digit check for the default family tokenizer program.
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0
        CMPI.B #'0', D0
        BLO tokvmProgramLoop
        CMPI.B #'9', D0
        BHI tokvmProgramLoop
        BRA tokvmApplyClassJump

tokvmClassQuote:
        ; Class 5 delegates to the same quote-set logic reused by string scanning.
        MOVE.L LOCAL_CURRENT_BYTE(A2), D0
        JSR tokvmIsQuoteChar
        TST.L D0
        BEQ tokvmProgramLoop

tokvmApplyClassJump:
        MOVE.L LOCAL_TEMP_U32(A2), D0
        CMP.L D7, D0
        BHI tokvmInvalidProgramAtCursor
        LEA 0(A3, D0.L), A0
        BRA tokvmProgramLoop

; The scan helpers below mirror vm_scan_identifier_token,
; vm_scan_number_token, vm_scan_string_token, and vm_scan_symbol_token in
; tokenizer_runtime_utils.rs. The helper bodies reuse A0, so the interpreter
; saves and restores the native program counter around each call.
tokvmOpcodeScanIdentifier:
        MOVE.L A0, LOCAL_PROGRAM_COUNTER(A2)
        JSR tokvmScanIdentifierToken
        TST.L D0
        BNE tokvmReturn
        MOVEA.L LOCAL_PROGRAM_COUNTER(A2), A0
        BRA tokvmProgramLoop

tokvmOpcodeScanNumber:
        MOVE.L A0, LOCAL_PROGRAM_COUNTER(A2)
        JSR tokvmScanNumberToken
        TST.L D0
        BNE tokvmReturn
        MOVEA.L LOCAL_PROGRAM_COUNTER(A2), A0
        BRA tokvmProgramLoop

tokvmOpcodeScanString:
        MOVE.L A0, LOCAL_PROGRAM_COUNTER(A2)
        JSR tokvmScanStringToken
        TST.L D0
        BNE tokvmReturn
        MOVEA.L LOCAL_PROGRAM_COUNTER(A2), A0
        BRA tokvmProgramLoop

tokvmOpcodeScanSymbol:
        MOVE.L A0, LOCAL_PROGRAM_COUNTER(A2)
        JSR tokvmScanSymbolToken
        TST.L D0
        BNE tokvmReturn
        MOVEA.L LOCAL_PROGRAM_COUNTER(A2), A0
        BRA tokvmProgramLoop

tokvmInvalidArgument:
        CLR.L D1
        CLR.L D2
        CLR.L D3
        MOVEQ #TK_STATUS_INVALID_ARGUMENT, D0
        BRA tokvmReturn

tokvmVmFailureAtCursor:
        MOVEQ #TK_STATUS_VM_FAILURE, D0
        BRA tokvmReturn

tokvmStepLimitExceeded:
        MOVEQ #TK_STATUS_STEP_LIMIT_EXCEEDED, D0
        BRA tokvmReturn

; Invalid program is reserved for truncated bytecode, bad jump targets, or
; opcode/operand combinations that the native interpreter refuses to execute.
tokvmInvalidProgramAtCursor:
        MOVEQ #TK_STATUS_INVALID_PROGRAM, D0

tokvmReturn:
        ADDA.L #LOCAL_SIZE, SP
        MOVEM.L (SP)+, D4-D7/A4-A6
        RTS

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

tokvmCommitPendingToken:
        CMP.L D5, D1  ; token_count < token_capacity
        BCC tokvmPendingTokenOverflow
        MOVE.L D3, D0  ; scratch_used + pending_len must stay within scratch_capacity
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D0
        CMP.L D6, D0
        BHI tokvmPendingLexemeOverflow
        MOVE.L D1, D0  ; compute record_index * TOKEN_RECORD_SIZE without a MUL dependency
        ADD.L D0, D0  ; *2
        MOVEA.L D0, A0
        ADD.L D0, D0  ; *4
        ADD.L D0, D0  ; *8
        ADD.L D0, D0  ; *16
        ADDA.L D0, A0  ; *18
        MOVE.L D1, D0
        ADD.L D0, D0  ; +2 => *20 byte stride
        ADDA.L D0, A0
        MOVEA.L A5, A1
        ADDA.L A0, A1
        MOVE.W LOCAL_PENDING_KIND(A2), (A1)  ; field 0: token kind code
        CLR.W 2(A1)
        MOVE.L LOCAL_PENDING_START(A2), D0  ; field 4: 1-based start column
        ADDQ.L #1, D0
        MOVE.L D0, 4(A1)
        MOVE.L LOCAL_PENDING_END(A2), D0  ; field 8: 1-based end column
        ADDQ.L #1, D0
        MOVE.L D0, 8(A1)
        MOVE.L D3, 12(A1)  ; field 12: lexeme offset into scratch
        MOVE.L LOCAL_PENDING_LEX_LEN(A2), 16(A1)  ; field 16: lexeme length in bytes
        ADDQ.L #1, D1
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D3
        MOVEQ #TK_STATUS_SUCCESS, D0
        RTS

; Overflow exits report the start column of the token that could not be fully
; materialized. This matches the Rust-side behavior of attributing capacity
; failures to the token currently being scanned rather than the following byte.
tokvmPendingTokenOverflow:
        MOVE.L LOCAL_PENDING_START(A2), D2
        MOVEQ #TK_STATUS_TOKEN_OVERFLOW, D0
        RTS

tokvmPendingLexemeOverflow:
        MOVE.L LOCAL_PENDING_START(A2), D2
        MOVEQ #TK_STATUS_LEXEME_OVERFLOW, D0
        RTS

; Stage a fixed lexeme literal from the static data table into scratch.
; This is used for punctuation and operator tokens whose lexeme spelling is
; known upfront and does not need to be copied from the source buffer.
tokvmStageFixedLexeme:
        MOVE.L D0, LOCAL_PENDING_LEX_LEN(A2)  ; fixed operator/punctuation lexeme length from the inline template string
        MOVE.L D3, D0
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D0
        CMP.L D6, D0
        BHI tokvmPendingLexemeOverflow
        MOVEA.L A6, A1
        ADDA.L D3, A1
        MOVE.L LOCAL_PENDING_LEX_LEN(A2), D0
tokvmStageFixedLexemeLoop:
        TST.L D0
        BEQ tokvmStageFixedLexemeDone
        MOVE.B (A0)+, (A1)+  ; copy the canonical lexeme bytes that Rust would expose in PortableToken text/raw
        SUBQ.L #1, D0
        BRA tokvmStageFixedLexemeLoop

tokvmStageFixedLexemeDone:
        MOVEQ #TK_STATUS_SUCCESS, D0
        RTS

; ---------------------------------------------------------------------------
; Scanner helpers.
;
; These are the native counterparts of the Rust tokenizer helper routines in
; tokenizer_runtime_utils.rs. Each helper advances D2 as the live source cursor,
; populates LOCAL_PENDING_* metadata, stages lexeme bytes into the scratch
; buffer, then commits a token record.
; ---------------------------------------------------------------------------

tokvmScanIdentifierToken:
        ; Identifier scan is the native mirror of vm_scan_identifier_token():
        ; walk identifier-continue bytes, lowercase ASCII letters for the demo
        ; policy, then emit one identifier record backed by scratch bytes.
        MOVE.L D2, LOCAL_PENDING_START(A2)
        CLR.L LOCAL_PENDING_LEX_LEN(A2)
        MOVEA.L A6, A0
        ADDA.L D3, A0

tokvmScanIdentifierLoop:
        ; D2 stays as the source cursor, A0 walks the next free scratch byte,
        ; and LOCAL_PENDING_LEX_LEN grows in lockstep so commit can later write
        ; both the source span and scratch payload length into the token record.
        CMP.L D4, D2
        BCC tokvmScanIdentifierDone
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        JSR tokvmIsIdentifierContinue  ; mirrors vm_matches_identifier_continue_class()
        TST.L D0
        BEQ tokvmScanIdentifierDone
        MOVE.L D3, D0
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D0
        CMP.L D6, D0
        BCC tokvmPendingLexemeOverflowFromScan
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        CMPI.B #'A', D0
        BLO tokvmCopyIdentifierByte
        CMPI.B #'Z', D0
        BHI tokvmCopyIdentifierByte
        ORI.B #$20, D0  ; native demo bakes in ASCII-lower identifier normalization used by the Rust bridge tests
tokvmCopyIdentifierByte:
        MOVE.B D0, (A0)+
        ADDQ.L #1, D2
        ADDQ.L #1, LOCAL_PENDING_LEX_LEN(A2)
        BRA tokvmScanIdentifierLoop

tokvmScanIdentifierDone:
        ; Match vm_scan_identifier_token(): a trailing prime belongs to the
        ; identifier/register lexeme for Z80 alternate-register spellings like AF'.
        CMP.L D4, D2
        BCC tokvmScanIdentifierCommit
        CMPI.B #39, 0(A4, D2.L)
        BNE tokvmScanIdentifierCommit
        MOVE.L D3, D0
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D0
        CMP.L D6, D0
        BCC tokvmPendingLexemeOverflowFromScan
        MOVE.B #39, (A0)+
        ADDQ.L #1, D2
        ADDQ.L #1, LOCAL_PENDING_LEX_LEN(A2)

tokvmScanIdentifierCommit:
        ; Identifier spans are half-open in cursor space and become 1-based only
        ; when tokvmCommitPendingToken serializes them into the native record.
        MOVE.W #TK_KIND_IDENTIFIER, LOCAL_PENDING_KIND(A2)
        MOVE.L D2, LOCAL_PENDING_END(A2)
        JSR tokvmCommitPendingToken
        RTS

tokvmScanNumberToken:
        ; Number scan accepts the same permissive body bytes as the Rust helper,
        ; leaving base interpretation to downstream token consumers/report logic.
        MOVE.L D2, LOCAL_PENDING_START(A2)
        CLR.L LOCAL_PENDING_LEX_LEN(A2)
        MOVEA.L A6, A0
        ADDA.L D3, A0

tokvmScanNumberLoop:
        ; Number scanning intentionally keeps the raw source spelling, including
        ; prefixes/suffixes/underscores, so later consumers can decide how to
        ; interpret base markers just like the Rust runtime does.
        CMP.L D4, D2
        BCC tokvmScanNumberDone
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        CMPI.B #'%', D0
        BNE tokvmScanNumberCheckBody
        CMP.L LOCAL_PENDING_START(A2), D2
        BEQ tokvmScanNumberAcceptByte
tokvmScanNumberCheckBody:
        JSR tokvmIsNumberBody  ; same permissive number-body walk as vm_scan_number_token()
        TST.L D0
        BEQ tokvmScanNumberDone
tokvmScanNumberAcceptByte:
        MOVE.L D3, D0
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D0
        CMP.L D6, D0
        BCC tokvmPendingLexemeOverflowFromScan
        MOVE.B 0(A4, D2.L), (A0)+
        ADDQ.L #1, D2
        ADDQ.L #1, LOCAL_PENDING_LEX_LEN(A2)
        BRA tokvmScanNumberLoop

tokvmScanNumberDone:
        MOVE.W #TK_KIND_NUMBER, LOCAL_PENDING_KIND(A2)
        MOVE.L D2, LOCAL_PENDING_END(A2)
        JSR tokvmCommitPendingToken
        RTS

tokvmScanStringToken:
        ; Strings keep their raw delimiter choice for closing rules, but only the
        ; decoded payload bytes are staged into scratch and exposed in LEXHEX.
        MOVE.L D2, LOCAL_PENDING_START(A2)
        CLR.L LOCAL_PENDING_LEX_LEN(A2)
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0  ; remember whether the string opened with ' or " so we can require the same closer
        MOVE.L D0, LOCAL_CURRENT_BYTE(A2)
        ADDQ.L #1, D2
        MOVEA.L A6, A0
        ADDA.L D3, A0

tokvmScanStringLoop:
        ; Strings advance one payload unit at a time. Plain bytes copy through,
        ; while escape sequences normalize into their decoded payload bytes so
        ; LEXHEX reflects runtime string contents rather than source spelling.
        CMP.L D4, D2
        BCC tokvmScanStringFailure
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        CMP.L LOCAL_CURRENT_BYTE(A2), D0
        BEQ tokvmScanStringClose
        CMPI.B #'\\', D0  ; decode the same escape surface exercised by vm_scan_string_token()
        BNE tokvmScanStringCopyLiteral
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmVmFailureAtCursor
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        CMPI.B #'n', D0
        BEQ tokvmStringEscapeNewline
        CMPI.B #'r', D0
        BEQ tokvmStringEscapeReturn
        CMPI.B #'t', D0
        BEQ tokvmStringEscapeTab
        CMPI.B #'x', D0  ; \xHH is decoded into one payload byte, just like the Rust helper
        BEQ tokvmStringEscapeHex
        BRA tokvmScanStringEmitDecoded

tokvmStringEscapeNewline:
        ; The decoded escape value remains in D0 and falls through the shared
        ; emit path that appends one payload byte to scratch.
        MOVEQ #10, D0
        BRA tokvmScanStringEmitDecoded

tokvmStringEscapeReturn:
        MOVEQ #13, D0
        BRA tokvmScanStringEmitDecoded

tokvmStringEscapeTab:
        MOVEQ #9, D0
        BRA tokvmScanStringEmitDecoded

tokvmStringEscapeHex:
        ; Parse exactly two hex digits after \x and combine them into one byte,
        ; mirroring tokenizer_runtime_utils::vm_scan_string_token().
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmScanStringFailure
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        JSR tokvmHexDigitValue
        TST.L D0
        BMI tokvmScanStringFailure
        MOVE.L D0, LOCAL_TEMP_U32(A2)
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmScanStringFailure
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        JSR tokvmHexDigitValue
        TST.L D0
        BMI tokvmScanStringFailure
        MOVE.L D1, -(SP)
        MOVE.L LOCAL_TEMP_U32(A2), D1
        LSL.L #4, D1
        OR.L D1, D0
        MOVE.L (SP)+, D1

        BRA tokvmScanStringEmitDecoded

tokvmScanStringCopyLiteral:
        ; Literal non-escape bytes use the same capacity accounting as decoded
        ; escapes so both paths feed one consistent scratch payload stream.
        MOVE.L D1, -(SP)
        MOVE.L D3, D1
        ADD.L LOCAL_PENDING_LEX_LEN(A2), D1
        CMP.L D6, D1
        BCC tokvmScanStringLiteralOverflow
        MOVE.L (SP)+, D1
        BRA tokvmScanStringEmitDecoded

tokvmScanStringLiteralOverflow:
        MOVE.L (SP)+, D1
        BRA tokvmPendingLexemeOverflowFromScan

tokvmScanStringEmitDecoded:
        MOVE.B D0, (A0)+
        ADDQ.L #1, LOCAL_PENDING_LEX_LEN(A2)
        ADDQ.L #1, D2
        BRA tokvmScanStringLoop

tokvmScanStringClose:
        ; D2 is advanced past the closing delimiter before commit so the source
        ; span matches the Rust token span semantics for quoted strings.
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_STRING, LOCAL_PENDING_KIND(A2)
        MOVE.L D2, LOCAL_PENDING_END(A2)
        JSR tokvmCommitPendingToken
        RTS

; Unterminated strings and malformed escape sequences both collapse to the same
; VM failure status in this first native slice.
tokvmScanStringFailure:
        MOVEQ #TK_STATUS_VM_FAILURE, D0
        RTS

; Symbol scan covers punctuation, operators, comments, and prefixed numeric
; forms. The structure intentionally parallels vm_scan_symbol_token() in Rust:
; dispatch by lead byte, optionally consume a longer form, then commit the
; canonical lexeme bytes through tokvmStageAndCommitSymbol.
tokvmScanSymbolToken:
        ; The dispatch order matters. More syntactically specific lead bytes are
        ; tested before generic operator fallbacks so multi-byte forms get the
        ; same precedence as in the Rust helper.
        MOVE.L D2, LOCAL_PENDING_START(A2)
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        CMPI.B #';', D0
        BEQ tokvmScanCommentToEol
        CMPI.B #'.', D0
        BEQ tokvmScanDotLike
        CMPI.B #'$', D0
        BEQ tokvmScanDollarOrPrefixedNumber
        CMPI.B #'%', D0
        BEQ tokvmScanPercentOrPrefixedNumber
        CMPI.B #'#', D0
        BEQ tokvmStageHash
        CMPI.B #'?', D0
        BEQ tokvmStageQuestion
        CMPI.B #'[', D0
        BEQ tokvmStageOpenBracket
        CMPI.B #']', D0
        BEQ tokvmStageCloseBracket
        CMPI.B #'{', D0
        BEQ tokvmStageOpenBrace
        CMPI.B #'}', D0
        BEQ tokvmStageCloseBrace
        CMPI.B #',', D0
        BEQ tokvmStageComma
        CMPI.B #':', D0
        BEQ tokvmStageColon
        CMPI.B #'(', D0
        BEQ tokvmStageOpenParen
        CMPI.B #')', D0
        BEQ tokvmStageCloseParen
        CMPI.B #'+', D0
        BEQ tokvmStagePlus
        CMPI.B #'-', D0
        BEQ tokvmStageMinus
        CMPI.B #'*', D0
        BEQ tokvmScanStarLike
        CMPI.B #'/', D0
        BEQ tokvmStageDivide
        CMPI.B #'~', D0
        BEQ tokvmStageBitNot
        CMPI.B #'=', D0
        BEQ tokvmScanEqualLike
        CMPI.B #'!', D0
        BEQ tokvmScanBangLike
        CMPI.B #'&', D0
        BEQ tokvmScanAndLike
        CMPI.B #'|', D0
        BEQ tokvmScanOrLike
        CMPI.B #'^', D0
        BEQ tokvmScanCaretLike
        CMPI.B #'<', D0
        BEQ tokvmScanLessLike
        CMPI.B #'>', D0
        BEQ tokvmScanGreaterLike
        MOVE.W #TK_VM_FAILURE_KIND_FAIL, tokvmLastFailureKind
        MOVE.W D0, tokvmLastFailureOperand
        MOVEQ #TK_STATUS_VM_FAILURE, D0
        RTS

tokvmScanCommentToEol:
        MOVE.L D4, D2  ; comments consume the rest of the line and emit no token, matching vm_scan_symbol_token()
        MOVEQ #TK_STATUS_SUCCESS, D0
        RTS

tokvmScanDotLike:
        ; '.', '..', and '..=' are grouped together so the operator family stays
        ; adjacent in both the native and Rust scanner implementations.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageDot
        CMPI.B #'.', 0(A4, D2.L)
        BNE tokvmStageDot
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageRange
        CMPI.B #'=', 0(A4, D2.L)
        BNE tokvmStageRange
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_RANGE_INCLUSIVE, LOCAL_PENDING_KIND(A2)
        LEA lexRangeInclusive, A0
        MOVEQ #3, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageRange:
        ; '..' and '..=' share the same entry path so the inclusive form only
        ; needs one extra lookahead byte and a different fixed lexeme template.
        MOVE.W #TK_KIND_OP_RANGE, LOCAL_PENDING_KIND(A2)
        LEA lexRange, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageDot:
        MOVE.W #TK_KIND_DOT, LOCAL_PENDING_KIND(A2)
        LEA lexDot, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanDollarOrPrefixedNumber:
        ; '$' is ambiguous by design: either a standalone dollar token or the
        ; prefix for a hex-like number literal if a valid body byte follows.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageDollar
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        JSR tokvmIsHexDigitOrUnderscore  ; '$' starts either a hex literal or a standalone dollar token
        TST.L D0
        BEQ tokvmStageDollar
        MOVE.L LOCAL_PENDING_START(A2), D2
        JSR tokvmScanNumberToken
        RTS

tokvmStageDollar:
        MOVE.W #TK_KIND_DOLLAR, LOCAL_PENDING_KIND(A2)
        LEA lexDollar, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanPercentOrPrefixedNumber:
        ; '%' is likewise split between modulo and binary-prefixed number forms.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStagePercent
        MOVEQ #0, D0
        MOVE.B 0(A4, D2.L), D0
        CMPI.B #'0', D0
        BEQ tokvmScanPercentAsNumber
        CMPI.B #'1', D0
        BNE tokvmStagePercent

tokvmScanPercentAsNumber:
        JSR tokvmPercentHasPrefixContext
        TST.L D0
        BEQ tokvmStagePercent
        MOVE.L LOCAL_PENDING_START(A2), D2  ; rewind so the number scanner sees the leading '%', like Rust prefixed-number handling
        JSR tokvmScanNumberToken
        RTS

tokvmStagePercent:
        MOVE.W #TK_KIND_OP_MOD, LOCAL_PENDING_KIND(A2)
        LEA lexMod, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageHash:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_HASH, LOCAL_PENDING_KIND(A2)
        LEA lexHash, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageQuestion:
        ; The single-byte staging labels below follow one repeated pattern:
        ; advance the source cursor, choose a token kind, point at the canonical
        ; lexeme bytes, set the lexeme length, then funnel through the shared
        ; stage-and-commit tail.
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_QUESTION, LOCAL_PENDING_KIND(A2)
        LEA lexQuestion, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageOpenBracket:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OPEN_BRACKET, LOCAL_PENDING_KIND(A2)
        LEA lexOpenBracket, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageCloseBracket:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_CLOSE_BRACKET, LOCAL_PENDING_KIND(A2)
        LEA lexCloseBracket, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageOpenBrace:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OPEN_BRACE, LOCAL_PENDING_KIND(A2)
        LEA lexOpenBrace, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageCloseBrace:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_CLOSE_BRACE, LOCAL_PENDING_KIND(A2)
        LEA lexCloseBrace, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageComma:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_COMMA, LOCAL_PENDING_KIND(A2)
        LEA lexComma, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageColon:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_COLON, LOCAL_PENDING_KIND(A2)
        LEA lexColon, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageOpenParen:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OPEN_PAREN, LOCAL_PENDING_KIND(A2)
        LEA lexOpenParen, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageCloseParen:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_CLOSE_PAREN, LOCAL_PENDING_KIND(A2)
        LEA lexCloseParen, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStagePlus:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_PLUS, LOCAL_PENDING_KIND(A2)
        LEA lexPlus, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageMinus:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_MINUS, LOCAL_PENDING_KIND(A2)
        LEA lexMinus, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanStarLike:
        ; '*' and '**' match the Rust tokenizer's multiply/power split.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageMultiply
        CMPI.B #'*', 0(A4, D2.L)
        BEQ tokvmStagePower

tokvmStageMultiply:
        MOVE.W #TK_KIND_OP_MULTIPLY, LOCAL_PENDING_KIND(A2)
        LEA lexMultiply, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStagePower:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_POWER, LOCAL_PENDING_KIND(A2)
        LEA lexPower, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageDivide:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_DIVIDE, LOCAL_PENDING_KIND(A2)
        LEA lexDivide, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageBitNot:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_BIT_NOT, LOCAL_PENDING_KIND(A2)
        LEA lexBitNot, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanEqualLike:
        ; '=' and '==' both normalize to the same equality token kind, matching
        ; the Rust tokenizer helper's forgiving equality parsing.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageEq
        CMPI.B #'=', 0(A4, D2.L)
        BNE tokvmStageEq
        ADDQ.L #1, D2

tokvmStageEq:
        ; The canonical fixed lexeme is always "==" for equality so report output
        ; normalizes '=' and '==' into one operator surface.
        MOVE.W #TK_KIND_OP_EQ, LOCAL_PENDING_KIND(A2)
        LEA lexEq, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanBangLike:
        ; '!' stands for logical-not, while '!=' upgrades to not-equal.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageLogicNot
        CMPI.B #'=', 0(A4, D2.L)
        BNE tokvmStageLogicNot
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_NE, LOCAL_PENDING_KIND(A2)
        LEA lexNe, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageLogicNot:
        ; Unlike equality, logical-not preserves its single-byte spelling in the
        ; report/output surface because '!' and '!=' are distinct token kinds.
        MOVE.W #TK_KIND_OP_LOGIC_NOT, LOCAL_PENDING_KIND(A2)
        LEA lexLogicNot, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanAndLike:
        ; '&' / '&&' map to bitwise and logical forms respectively.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageBitAnd
        CMPI.B #'&', 0(A4, D2.L)
        BNE tokvmStageBitAnd
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_LOGIC_AND, LOCAL_PENDING_KIND(A2)
        LEA lexLogicAnd, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageBitAnd:
        MOVE.W #TK_KIND_OP_BIT_AND, LOCAL_PENDING_KIND(A2)
        LEA lexBitAnd, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanOrLike:
        ; '|' / '||' map to bitwise and logical forms respectively.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageBitOr
        CMPI.B #'|', 0(A4, D2.L)
        BNE tokvmStageBitOr
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_LOGIC_OR, LOCAL_PENDING_KIND(A2)
        LEA lexLogicOr, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageBitOr:
        MOVE.W #TK_KIND_OP_BIT_OR, LOCAL_PENDING_KIND(A2)
        LEA lexBitOr, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanCaretLike:
        ; '^' is bitwise xor, while '^^' is promoted to the logical xor
        ; token used by the native report-name table.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageBitXor
        CMPI.B #'^', 0(A4, D2.L)
        BNE tokvmStageBitXor
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_LOGIC_XOR, LOCAL_PENDING_KIND(A2)
        LEA lexLogicXor, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageBitXor:
        MOVE.W #TK_KIND_OP_BIT_XOR, LOCAL_PENDING_KIND(A2)
        LEA lexBitXor, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanLessLike:
        ; '<' expands into four related operators: <, <<, <=, and <>/!=.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageLt
        CMPI.B #'<', 0(A4, D2.L)
        BEQ tokvmStageShl
        CMPI.B #'=', 0(A4, D2.L)
        BEQ tokvmStageLe
        CMPI.B #'>', 0(A4, D2.L)
        BEQ tokvmStageAltNe
        BRA tokvmStageLt

tokvmStageShl:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_SHL, LOCAL_PENDING_KIND(A2)
        LEA lexShl, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageLe:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_LE, LOCAL_PENDING_KIND(A2)
        LEA lexLe, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageAltNe:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_NE, LOCAL_PENDING_KIND(A2)
        LEA lexNe, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageLt:
        ; The family labels above all converge here with a fully-chosen token
        ; kind and lexeme template, so the commit tail can stay generic.
        MOVE.W #TK_KIND_OP_LT, LOCAL_PENDING_KIND(A2)
        LEA lexLt, A0
        MOVEQ #1, D0
        BRA tokvmStageAndCommitSymbol

tokvmScanGreaterLike:
        ; '>' expands into >, >>, and >=.
        ADDQ.L #1, D2
        CMP.L D4, D2
        BCC tokvmStageGt
        CMPI.B #'>', 0(A4, D2.L)
        BEQ tokvmStageShr
        CMPI.B #'=', 0(A4, D2.L)
        BEQ tokvmStageGe
        BRA tokvmStageGt

tokvmStageShr:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_SHR, LOCAL_PENDING_KIND(A2)
        LEA lexShr, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageGe:
        ADDQ.L #1, D2
        MOVE.W #TK_KIND_OP_GE, LOCAL_PENDING_KIND(A2)
        LEA lexGe, A0
        MOVEQ #2, D0
        BRA tokvmStageAndCommitSymbol

tokvmStageGt:
        MOVE.W #TK_KIND_OP_GT, LOCAL_PENDING_KIND(A2)
        LEA lexGt, A0
        MOVEQ #1, D0

tokvmStageAndCommitSymbol:
        ; At this point LOCAL_PENDING_START already marks the source span start,
        ; D2 already points just past the consumed source bytes, and A0/D0 name
        ; the canonical lexeme bytes to materialize into scratch.
        JSR tokvmStageFixedLexeme  ; stage the canonical lexeme bytes before committing the token metadata
        TST.L D0
        BNE tokvmStageAndCommitSymbolDone
        MOVE.L D2, LOCAL_PENDING_END(A2)
        JSR tokvmCommitPendingToken
tokvmStageAndCommitSymbolDone:
        RTS

; Used when symbol lookahead discovers a shape the native bytecode contract does
; not allow, such as the unsupported '**' power operator.
tokvmScanSymbolInvalidProgram:
        MOVE.L LOCAL_PENDING_START(A2), D2
        MOVEQ #TK_STATUS_INVALID_PROGRAM, D0
        RTS

; Shared overflow exit for any scanner that would exceed scratch capacity.
tokvmPendingLexemeOverflowFromScan:
        MOVE.L LOCAL_PENDING_START(A2), D2
        MOVEQ #TK_STATUS_LEXEME_OVERFLOW, D0
        RTS

; ---------------------------------------------------------------------------
; Character-class and byte-shape predicates.
;
; These intentionally parallel vm_char_class_matches, vm_matches_identifier_*,
; and related helper logic in tokenizer_runtime_utils.rs. The native demo loop
; calls them through JumpIfClass and the scan helpers reuse them while walking
; identifiers, number bodies, strings, and prefixed constants.
; ---------------------------------------------------------------------------

tokvmIsWhitespace:
        CMPI.B #' ', D0  ; this line-input slice only treats space and tab as intra-line whitespace
        BEQ tokvmPredicateTrue
        CMPI.B #9, D0
        BEQ tokvmPredicateTrue
        MOVEQ #0, D0
        RTS

tokvmIsIdentifierStart:
        ; These predicate chains intentionally avoid lookup tables so the native
        ; implementation stays easy to audit against the Rust helper masks.
        CMPI.B #'A', D0
        BLO tokvmCheckIdentStartLower
        CMPI.B #'Z', D0
        BLS tokvmPredicateTrue
tokvmCheckIdentStartLower:
        CMPI.B #'a', D0
        BLO tokvmCheckIdentStartPunct
        CMPI.B #'z', D0
        BLS tokvmPredicateTrue
tokvmCheckIdentStartPunct:
        CMPI.B #'_', D0
        BEQ tokvmPredicateTrue
        CMPI.B #'.', D0  ; '.' remains identifier-start-capable because the runtime class mask includes it
        BEQ tokvmPredicateTrue
        MOVEQ #0, D0
        RTS

tokvmIsIdentifierContinue:
        CMPI.B #'A', D0
        BLO tokvmCheckIdentContinueLower
        CMPI.B #'Z', D0
        BLS tokvmPredicateTrue
tokvmCheckIdentContinueLower:
        CMPI.B #'a', D0
        BLO tokvmCheckIdentContinueDigit
        CMPI.B #'z', D0
        BLS tokvmPredicateTrue
tokvmCheckIdentContinueDigit:
        CMPI.B #'0', D0
        BLO tokvmCheckIdentExtra
        CMPI.B #'9', D0
        BLS tokvmPredicateTrue
tokvmCheckIdentExtra:
        CMPI.B #'_', D0
        BEQ tokvmPredicateTrue
        CMPI.B #'.', D0
        BEQ tokvmPredicateTrue
        CMPI.B #'$', D0  ; '$' and '@' stay valid continue bytes per tokenizer_runtime_utils.rs masks
        BEQ tokvmPredicateTrue
        CMPI.B #'@', D0
        BEQ tokvmPredicateTrue
        MOVEQ #0, D0
        RTS

tokvmIsQuoteChar:
        CMPI.B #'"', D0  ; demo program accepts both quote styles, matching the Rust helper's quote-char set
        BEQ tokvmPredicateTrue
        CMPI.B #39, D0
        BEQ tokvmPredicateTrue
        MOVEQ #0, D0
        RTS

tokvmIsNumberBody:
        ; Number bodies are deliberately permissive at scan time. Validation of
        ; bases and suffix meaning is deferred to later consumers, matching the
        ; Rust tokenizer helper contract.
        CMPI.B #'0', D0
        BLO tokvmCheckNumberLetters
        CMPI.B #'9', D0
        BLS tokvmPredicateTrue
tokvmCheckNumberLetters:
        CMPI.B #'A', D0
        BLO tokvmCheckNumberLower
        CMPI.B #'Z', D0
        BLS tokvmPredicateTrue
tokvmCheckNumberLower:
        CMPI.B #'a', D0
        BLO tokvmCheckNumberExtra
        CMPI.B #'z', D0
        BLS tokvmPredicateTrue
tokvmCheckNumberExtra:
        CMPI.B #'_', D0
        BEQ tokvmPredicateTrue
        CMPI.B #'$', D0
        BEQ tokvmPredicateTrue
        CMPI.B #'%', D0
        BEQ tokvmPredicateTrue
        CMPI.B #'@', D0
        BEQ tokvmPredicateTrue
        MOVEQ #0, D0
        RTS

tokvmIsHexDigitOrUnderscore:
        ; Used only as a fast probe for deciding whether '$' begins a number or
        ; remains a standalone token.
        CMPI.B #'_', D0
        BEQ tokvmPredicateTrue
        JSR tokvmHexDigitValue
        TST.L D0
        BMI tokvmPredicateFalse
tokvmPredicateTrue:
        MOVEQ #1, D0
        RTS

tokvmPredicateFalse:
        MOVEQ #0, D0
        RTS

; Shared hex nibble decoder for both string escape parsing and '$'-prefixed
; number probing. Returns -1 for non-hex input so callers can branch cleanly.
tokvmHexDigitValue:
        CMPI.B #'0', D0  ; shared nibble decoder for \xHH strings and '$'-prefixed number probing
        BLO tokvmHexUpper
        CMPI.B #'9', D0
        BHI tokvmHexUpper
        SUBI.B #'0', D0
        ANDI.L #$FF, D0
        RTS

tokvmHexUpper:
        CMPI.B #'A', D0
        BLO tokvmHexLower
        CMPI.B #'F', D0
        BHI tokvmHexLower
        SUBI.B #'A', D0
        ADDI.B #10, D0
        ANDI.L #$FF, D0
        RTS

tokvmHexLower:
        CMPI.B #'a', D0
        BLO tokvmHexInvalid
        CMPI.B #'f', D0
        BHI tokvmHexInvalid
        SUBI.B #'a', D0
        ADDI.B #10, D0
        ANDI.L #$FF, D0
        RTS

tokvmHexInvalid:
        MOVEQ #-1, D0
        RTS

; Rust treats '%' as a binary-number prefix only when the byte appears where an
; expression can start. Without that context, % remains the modulo operator.
tokvmPercentHasPrefixContext:
        MOVE.L LOCAL_PENDING_START(A2), D0
        BEQ tokvmPercentPrefixTrue

        CLR.W LOCAL_PENDING_KIND(A2)
        CLR.L LOCAL_TEMP_U32(A2)
        SUBQ.L #1, D0
        MOVE.L D0, LOCAL_TEMP_U32(A2)
        LEA 0(A4, D0.L), A1
        MOVEQ #0, D0
        MOVE.B (A1), D0
        CMPI.B #' ', D0
        BEQ tokvmPercentMarkLeadingSpace
        CMPI.B #9, D0
        BNE tokvmPercentCheckPrevNonSpaceByte

tokvmPercentMarkLeadingSpace:
        MOVEQ #1, D0
        MOVE.W D0, LOCAL_PENDING_KIND(A2)

tokvmPercentPrevNonSpaceLoop:
        MOVE.L LOCAL_TEMP_U32(A2), D0
        TST.L D0
        BEQ tokvmPercentPrefixTrue
        SUBQ.L #1, D0
        MOVE.L D0, LOCAL_TEMP_U32(A2)
        LEA 0(A4, D0.L), A1
        MOVEQ #0, D0
        MOVE.B (A1), D0

tokvmPercentCheckPrevNonSpaceByte:
        CMPI.B #' ', D0
        BEQ tokvmPercentPrevNonSpaceLoop
        CMPI.B #9, D0
        BEQ tokvmPercentPrevNonSpaceLoop
        CMPI.B #'(', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #',', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'+', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'-', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'*', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'/', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'%', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'&', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'|', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'^', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'~', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'!', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'<', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'>', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'=', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #'?', D0
        BEQ tokvmPercentPrefixTrue
        CMPI.B #':', D0
        BEQ tokvmPercentPrefixTrue

        TST.W LOCAL_PENDING_KIND(A2)
        BEQ tokvmPercentPrefixFalse
        JSR tokvmIsIdentifierContinue
        TST.L D0
        BNE tokvmPercentPrefixTrue

tokvmPercentPrefixFalse:
        MOVEQ #0, D0
        RTS

tokvmPercentPrefixTrue:
        MOVEQ #1, D0
        RTS

        .endsection
        .section data, kind=data

; Data section: ABI marker, bytecode macros, demo bytecode, and fixed lexeme
; templates used by the symbol scanner.
abiMarker:
        .byte "OPFORGE-TOKVM-ABI-V1", 0

; The emit* helpers encode the same little-endian jump-target format produced by
; Rust's default_family_tokenizer_vm_program_bytes(). Keep demoProgram and the
; DEMO_PC_* offsets together so the control-flow map stays readable.
emitLe32 .macro value
        ; Keep jump targets readable in source while still emitting the same
        ; little-endian u32 layout as builder.rs and the Rust VM loader expect.
        .byte (.value) & $ff
        .byte ((.value) >> 8) & $ff
        .byte ((.value) >> 16) & $ff
        .byte ((.value) >> 24) & $ff
.endmacro

emitJumpTarget .macro opcode, target
        ; Macros keep the demo bytecode readable without obscuring the exact byte
        ; sequence that the Rust builder would emit.
        .byte .opcode
        .emitLe32 .target
.endmacro

emitClassJump .macro class_id, target
        .byte TK_OPCODE_JUMP_IF_CLASS, .class_id
        .emitLe32 .target
.endmacro

emitByteJump .macro byte_value, target
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
demoProgram:
demoReadChar:
        .byte TK_OPCODE_READ_CHAR
        .emitJumpTarget TK_OPCODE_JUMP_IF_EOL, DEMO_PC_FINISH
        .emitClassJump TK_CLASS_WHITESPACE, DEMO_PC_SKIP_WHITESPACE
        .emitByteJump '.', DEMO_PC_SCAN_SYMBOL
        .emitClassJump TK_CLASS_IDENTIFIER_START, DEMO_PC_SCAN_IDENTIFIER
        .emitClassJump TK_CLASS_DIGIT, DEMO_PC_SCAN_NUMBER
        .emitClassJump TK_CLASS_QUOTE, DEMO_PC_SCAN_STRING

demoScanSymbol:
        ; Every scan arm jumps back to DEMO_PC_READ_CHAR so the program behaves
        ; as a tight read-dispatch-scan loop until EOL.
        .byte TK_OPCODE_SCAN_SYMBOL
        .emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

demoSkipWhitespace:
        ; Whitespace is the only class that does not emit a token. It simply
        ; advances one byte and loops back to the next ReadChar.
        .byte TK_OPCODE_ADVANCE
        .emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

demoScanIdentifier:
        .byte TK_OPCODE_SCAN_IDENTIFIER
        .emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

demoScanNumber:
        .byte TK_OPCODE_SCAN_NUMBER
        .emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

demoScanString:
        .byte TK_OPCODE_SCAN_STRING
        .emitJumpTarget TK_OPCODE_JUMP, DEMO_PC_READ_CHAR

demoFinish:
        .byte TK_OPCODE_END

; Canonical lexeme spellings used by tokvmStageFixedLexeme.
; Keeping them in one contiguous table makes the symbol scanner readable and
; ensures the report's LEXHEX field stays stable across all operator forms.
; Grouping also makes it obvious which operators are implemented natively in
; this slice: if there is no fixed lexeme entry here, the scanner cannot emit it.
lexDot:
        .byte "."
lexDollar:
        .byte "$"
lexHash:
        .byte "#"
lexQuestion:
        .byte "?"
lexOpenBracket:
        .byte "["
lexCloseBracket:
        .byte "]"
lexOpenBrace:
        .byte "{"
lexCloseBrace:
        .byte "}"
lexComma:
        .byte ","
lexColon:
        .byte ":"
lexOpenParen:
        .byte "("
lexCloseParen:
        .byte ")"
lexPlus:
        .byte "+"
lexMinus:
        .byte "-"
lexMultiply:
        .byte "*"
lexPower:
        .byte "**"
lexDivide:
        .byte "/"
lexBitNot:
        .byte "~"
lexEq:
        .byte "=="
lexNe:
        .byte "!="
lexLogicNot:
        .byte "!"
lexBitAnd:
        .byte "&"
lexBitOr:
        .byte "|"
lexLogicAnd:
        .byte "&&"
lexLogicOr:
        .byte "||"
lexBitXor:
        .byte "^"
lexLogicXor:
        .byte "^^"
lexLt:
        .byte "<"
lexLe:
        .byte "<="
lexGt:
        .byte ">"
lexGe:
        .byte ">="
lexShl:
        .byte "<<"
lexShr:
        .byte ">>"
lexMod:
        .byte "%"
lexRange:
        .byte ".."
lexRangeInclusive:
        .byte "..="

; 67 bytes is the assembled size of demoProgram and must stay aligned with the
; symbolic DEMO_PC_* offsets above as well as the Rust builder's default loop.
demoProgramLen:
        .long 67

        .endsection
        .endmodule
