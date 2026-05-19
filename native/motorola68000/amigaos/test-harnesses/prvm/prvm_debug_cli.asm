; Optional AmigaOS PRVM report demo for the parity-locked runtime.

	.module main
	.cpu 68020
	.use prvm.amigaos.runtime (prvmRun68000)

SYS_BASE                        = 4
RETURN_OK                       = 0
RETURN_FAIL                     = 20

OPEN_LIBRARY                    = -552
CLOSE_LIBRARY                   = -414
PUT_STR                         = -948

PRVM_REQUEST_FRAME_SIZE         = 112
PRVM_MAGIC_OPRP                 = $4F505250
PRVM_ABI_VERSION_V1             = 1
PRVM_CALL_MODE_START            = 0
PRVM_ENTRY_KIND_OPASM_STATEMENT = 1
PRVM_TOKEN_RECORD_SIZE          = 20
PRVM_PARSER_CONTRACT_VERSION_V2 = 2

PRVM_STATUS_OK                  = 0

PRVM_RESULT_BEGIN_STATEMENT     = 1
PRVM_RESULT_LABEL_TEXT          = 2
PRVM_RESULT_MNEMONIC_TEXT       = 3
PRVM_RESULT_FINISH_LINE         = 5

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
PRVM_DEBUG_PROGRAM_LEN          = 59

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
	bsr.w buildRequestFrame
	lea RequestFrame(PC), a0
	move.l #PRVM_REQUEST_FRAME_SIZE, d0
	jsr prvmRun68000

	lea PrvmStatus(PC), a0
	move.l d0, 0(a0)
	move.l d1, 4(a0)
	move.l d2, 8(a0)
	move.l d3, 12(a0)

	bsr.w validateResult
	tst.l d0
	bne.s reportFailure

	lea ReportSuccessText(PC), a1
	move.l a1, d1
	bsr.w putStr
	moveq #RETURN_OK, d7
	bra.s closeDos

reportFailure
	bsr.w formatStatus
	lea ReportFailureText(PC), a1
	move.l a1, d1
	bsr.w putStr

closeDos
	movea.l a5, a1
	movea.l SYS_BASE.W, a6
	jsr CLOSE_LIBRARY(a6)

done
	move.l d7, d0
	rts
	.bend  ; start
	.priv

putStr	.block
	movea.l a5, a6
	jsr PUT_STR(a6)
	rts
	.bend  ; putStr

buildRequestFrame	.block
	lea RequestFrame(PC), a0
	move.l #PRVM_MAGIC_OPRP, 0(a0)
	move.w #PRVM_ABI_VERSION_V1, 4(a0)
	move.w #PRVM_REQUEST_FRAME_SIZE, 6(a0)
	move.w #PRVM_CALL_MODE_START, 8(a0)
	move.w #PRVM_ENTRY_KIND_OPASM_STATEMENT, 10(a0)
	move.l #1, 12(a0)
	lea SourceLine(PC), a1
	move.l a1, 16(a0)
	move.l #10, 20(a0)
	lea TokenRecord(PC), a1
	move.l a1, 24(a0)
	move.l #3, 28(a0)
	move.w #PRVM_TOKEN_RECORD_SIZE, 32(a0)
	clr.w 34(a0)
	lea LexemeBytes(PC), a1
	move.l a1, 36(a0)
	move.l #8, 40(a0)
	lea ParserProgram(PC), a1
	move.l a1, 44(a0)
	move.l #PRVM_DEBUG_PROGRAM_LEN, 48(a0)
	lea ResultBuffer(PC), a1
	move.l a1, 52(a0)
	move.l #128, 56(a0)
	lea DiagnosticBuffer(PC), a1
	move.l a1, 60(a0)
	move.l #32, 64(a0)
	lea ResumeBuffer(PC), a1
	move.l a1, 68(a0)
	move.l #40, 72(a0)
	lea ExprRequestBuffer(PC), a1
	move.l a1, 76(a0)
	move.l #32, 80(a0)
	lea ExprResultBuffer(PC), a1
	move.l a1, 84(a0)
	move.l #0, 88(a0)
	move.l #PRVM_PARSER_CONTRACT_VERSION_V2, 92(a0)
	move.l #64, 96(a0)
	clr.l 100(a0)
	clr.l 104(a0)
	clr.l 108(a0)
	rts
	.bend  ; buildRequestFrame

validateResult	.block
	lea PrvmStatus(PC), a1
	lea ResultBuffer(PC), a0
	cmpi.l #PRVM_STATUS_OK, 0(a1)
	bne.w invalid
	cmpi.l #4, 4(a1)
	bne.w invalid
	cmpi.l #3, 8(a1)
	bne.w invalid
	cmpi.l #128, 12(a1)
	bne.w invalid
	cmpi.w #PRVM_RESULT_BEGIN_STATEMENT, 0(a0)
	bne.w invalid
	cmpi.w #PRVM_RESULT_LABEL_TEXT, 32(a0)
	bne.w invalid
	cmpi.l #1, 40(a0)
	bne.w invalid
	cmpi.l #6, 44(a0)
	bne.w invalid
	tst.l 48(a0)
	bne.w invalid
	cmpi.l #5, 52(a0)
	bne.w invalid
	cmpi.w #PRVM_RESULT_MNEMONIC_TEXT, 64(a0)
	bne.w invalid
	cmpi.l #8, 72(a0)
	bne.w invalid
	cmpi.l #11, 76(a0)
	bne.w invalid
	cmpi.l #5, 80(a0)
	bne.w invalid
	cmpi.l #3, 84(a0)
	bne.w invalid
	cmpi.w #PRVM_RESULT_FINISH_LINE, 96(a0)
	bne.w invalid
	clr.l d0
	rts

invalid
	moveq #1, d0
	rts
	.bend  ; validateResult

formatStatus	.block
	lea PrvmStatus(PC), a1
	move.l 0(a1), d0
	lea ReportFailureStatusHexDigits(PC), a0
	moveq #7, d2

loop
	rol.l #4, d0
	move.l d0, d3
	andi.b #$0F, d3
	cmpi.b #10, d3
	bcs.s digit
	addi.b #7, d3

digit
	addi.b #"0", d3
	move.b d3, (a0)+
	dbra d2, loop
	rts
	.bend  ; formatStatus

DosName
	.byte "dos.library", 0
ReportSuccessText
	.byte "OPFORGE-PRVM 1", 10
	.byte "STATUS 0", 10
	.byte "RESULTS 4", 10
	.byte "CURSOR 3", 10
	.byte "BYTES 128", 10
	.byte "RESULT 0 KIND begin_statement", 10
	.byte "RESULT 1 KIND label_text START 1 END 6 LEN 5 LEXHEX 7374617274", 10
	.byte "RESULT 2 KIND mnemonic_text START 8 END 11 LEN 3 LEXHEX 4E4F50", 10
	.byte "RESULT 3 KIND finish_line", 10
	.byte "END", 10, 0
ReportFailureText
	.byte "OPFORGE-PRVM 1", 10
	.byte "STATUS $"
ReportFailureStatusHexDigits
	.byte "00000000", 10
	.byte "RESULTS 0", 10
	.byte "CURSOR 0", 10
	.byte "BYTES 0", 10
	.byte "END", 10, 0

SourceLine
	.byte "start: NOP"

LexemeBytes
	.byte "startNOP"

ParserProgram
	.byte $60, $40, $13, $03, $08, $00, $64, $00
	.byte $14, $03, $0E, $00, $66, $00
	.byte $15, $03, $24, $00
	.byte $33, $04, ".", "o", "r", "g", $62, $20, $22, $02, $41, $50
	.byte $FF, $FF, $FF, $FF, $64, $00
	.byte $10, $03, $03, $30, $00, $20, $30, $65, $20, $01, $33, $00
	.byte $30, $62, $20, $41, $50, $FF, $FF, $FF, $FF, $64, $00

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

PrvmStatus
	.long 0
PrvmResultCount
	.long 0
PrvmCursor
	.long 0
PrvmResultBytes
	.long 0

RequestFrame
	.fill byte, 112, 0
ResultBuffer
	.fill byte, 128, 0
DiagnosticBuffer
	.fill byte, 32, 0
ResumeBuffer
	.fill byte, 40, 0
ExprRequestBuffer
	.fill byte, 32, 0
ExprResultBuffer
	.fill byte, 32, 0

	.endsection
	.output "build/prvm_debug_cli.hunk", format=hunk, sections=entry, code
	.endmodule
