; Native tokenizer VM default demo program and fixed lexeme data.

	.module tkvm.amigaos.demo_program
	.cpu 68020
	.pub

; Private bytecode constants for assembling DemoProgram without importing the
; runtime module and creating a .use cycle.
TK_OPCODE_END                   = 0
TK_OPCODE_READ_CHAR             = 1
TK_OPCODE_ADVANCE               = 2
TK_OPCODE_JUMP                  = 7
TK_OPCODE_JUMP_IF_EOL           = 8
TK_OPCODE_JUMP_IF_BYTE_EQ       = 9
TK_OPCODE_JUMP_IF_CLASS         = 10
TK_OPCODE_SCAN_IDENTIFIER       = 15
TK_OPCODE_SCAN_NUMBER           = 16
TK_OPCODE_SCAN_STRING           = 17
TK_OPCODE_SCAN_SYMBOL           = 18

TK_CLASS_WHITESPACE             = 1
TK_CLASS_IDENTIFIER_START       = 2
TK_CLASS_DIGIT                  = 4
TK_CLASS_QUOTE                  = 5

; Program-counter labels for DemoProgram. These offsets intentionally match the
; little-endian jump targets emitted by the Rust builder default loop.
DEMO_PC_READ_CHAR               = 0
DEMO_PC_SCAN_SYMBOL             = 36
DEMO_PC_SKIP_WHITESPACE         = 42
DEMO_PC_SCAN_IDENTIFIER         = 48
DEMO_PC_SCAN_NUMBER             = 54
DEMO_PC_SCAN_STRING             = 60
DEMO_PC_FINISH                  = 66

	.section data, kind=data
	.pub

; Data section: ABI marker, bytecode macros, demo bytecode, and fixed lexeme
; templates used by the symbol scanner.
AbiMarker
	.byte "OPFORGE-TOKVM-ABI-V1", 0

DemoStateEntryOffsets
	.long DEMO_PC_READ_CHAR

; The emit* helpers encode the same little-endian jump-target format produced by
; Rust's default_family_tokenizer_vm_program_bytes(). Keep DemoProgram and the
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

; 67 bytes is the assembled size of DemoProgram and must stay aligned with the
; symbolic DEMO_PC_* offsets above as well as the Rust builder's default loop.
DemoProgramLen
	.long 67

	.endsection
	.endmodule
