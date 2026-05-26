; FS-UAE-friendly native smoke executable for the PRVM line iterator.

	.module main
	.cpu 68020

SYS_BASE                        = 4
RETURN_OK                       = 0
RETURN_FAIL                     = 20

OPEN_LIBRARY                    = -552
CLOSE_LIBRARY                   = -414
PUT_STR                         = -948

PRVM_ITER_FRAME_SIZE            = 116
PRVM_ITER_MAGIC_OPLI            = $4F504C49
PRVM_ITER_ABI_VERSION_V1        = 1
PRVM_TOKEN_RECORD_SIZE          = 20
PRVM_PARSER_CONTRACT_VERSION_V2 = 2
PRVM_ITER_STATUS_OK             = 0
PRVM_STATUS_UNSUPPORTED_ROUTE   = 100
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
	bsr.w buildFrame
	lea IteratorFrame(PC), a0
	move.l #PRVM_ITER_FRAME_SIZE, d0
	movea.l PrvmIteratorEntryPtr(PC), a1
	jsr (a1)

	lea IteratorStatus(PC), a0
	move.l d0, 0(a0)
	move.l d1, 4(a0)
	move.l d2, 8(a0)
	move.l d3, 12(a0)

	bsr.w validateResult
	tst.l d0
	bne.s reportFailure

	bsr.w buildFrame
	lea IteratorFrame(PC), a0
	move.l #40, 24(a0)
	lea UnsupportedProcessorText(PC), a1
	move.l a1, 8(a0)
	move.l #3, 12(a0)
	lea IteratorFrame(PC), a0
	move.l #PRVM_ITER_FRAME_SIZE, d0
	movea.l PrvmIteratorEntryPtr(PC), a1
	jsr (a1)

	lea IteratorStatus(PC), a0
	move.l d0, 0(a0)
	move.l d1, 4(a0)
	move.l d2, 8(a0)
	move.l d3, 12(a0)

	bsr.w validateFailFastResult
	tst.l d0
	bne.s reportFailure

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
	.bend  ; start
	.priv

putStr	.block
	movea.l a5, a6
	jsr PUT_STR(a6)
	rts
	.bend  ; putStr

buildFrame	.block
	lea IteratorFrame(PC), a0
	move.l #PRVM_ITER_MAGIC_OPLI, 0(a0)
	move.w #PRVM_ITER_ABI_VERSION_V1, 4(a0)
	move.w #PRVM_ITER_FRAME_SIZE, 6(a0)
	lea ProcessorAsmText(PC), a1
	move.l a1, 8(a0)
	move.l #3, 12(a0)
	lea KindStatementText(PC), a1
	move.l a1, 16(a0)
	move.l #9, 20(a0)
	move.l #7, 24(a0)
	lea SourceText(PC), a1
	move.l a1, 28(a0)
	move.l #21, 32(a0)
	lea TokenRecord(PC), a1
	move.l a1, 36(a0)
	move.l #3, 40(a0)
	move.w #PRVM_TOKEN_RECORD_SIZE, 44(a0)
	clr.w 46(a0)
	lea LexemeBytes(PC), a1
	move.l a1, 48(a0)
	move.l #8, 52(a0)
	lea ParserProgram(PC), a1
	move.l a1, 56(a0)
	move.l #PRVM_DEBUG_PROGRAM_LEN, 60(a0)
	lea ResultBuffer(PC), a1
	move.l a1, 64(a0)
	move.l #128, 68(a0)
	lea DiagnosticBuffer(PC), a1
	move.l a1, 72(a0)
	move.l #32, 76(a0)
	lea ResumeBuffer(PC), a1
	move.l a1, 80(a0)
	move.l #40, 84(a0)
	lea ExprRequestBuffer(PC), a1
	move.l a1, 88(a0)
	move.l #32, 92(a0)
	lea ExprResultBuffer(PC), a1
	move.l a1, 96(a0)
	move.l #0, 100(a0)
	move.l #PRVM_PARSER_CONTRACT_VERSION_V2, 104(a0)
	move.l #64, 108(a0)
	clr.l 112(a0)
	rts
	.bend  ; buildFrame

validateResult	.block
	lea IteratorStatus(PC), a0
	cmpi.l #PRVM_ITER_STATUS_OK, 0(a0)
	bne.s invalidStatus
	cmpi.l #2, 4(a0)
	bne.s invalidRouted
	tst.l 8(a0)
	bne.s invalidFailLine
	cmpi.l #2, 12(a0)
	bne.s invalidTotal
	clr.l d0
	rts
	.bend  ; validateResult

validateFailFastResult	.block
	lea IteratorStatus(PC), a0
	cmpi.l #PRVM_STATUS_UNSUPPORTED_ROUTE, 0(a0)
	bne.s invalidFailFastStatus
	tst.l 4(a0)
	bne.s invalidFailFastRouted
	cmpi.l #40, 8(a0)
	bne.s invalidFailFastFailLine
	cmpi.l #1, 12(a0)
	bne.s invalidFailFastTotal
	clr.l d0
	rts
	.bend  ; validateFailFastResult

invalidStatus	.block
	lea FailureStatusText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidStatus

invalidRouted	.block
	lea FailureRoutedText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidRouted

invalidFailLine	.block
	lea FailureFailLineText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidFailLine

invalidTotal	.block
	lea FailureTotalText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidTotal

invalidFailFastStatus	.block
	lea FailureFailFastStatusText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidFailFastStatus

invalidFailFastRouted	.block
	lea FailureFailFastRoutedText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidFailFastRouted

invalidFailFastFailLine	.block
	lea FailureFailFastFailLineText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidFailFastFailLine

invalidFailFastTotal	.block
	lea FailureFailFastTotalText(PC), a1
	moveq #1, d0
	rts
	.bend  ; invalidFailFastTotal

DosName
	.byte "dos.library", 0
ProcessorAsmText
	.byte "asm"
KindStatementText
	.byte "statement"
UnsupportedProcessorText
	.byte "bad"
SuccessText
	.byte "OPFORGE-PRVM-ITER smoke OK", 10, 0
FailureStatusText
	.byte "OPFORGE-PRVM-ITER smoke FAIL status", 10, 0
FailureRoutedText
	.byte "OPFORGE-PRVM-ITER smoke FAIL routed", 10, 0
FailureFailLineText
	.byte "OPFORGE-PRVM-ITER smoke FAIL fail-line", 10, 0
FailureTotalText
	.byte "OPFORGE-PRVM-ITER smoke FAIL total", 10, 0
FailureFailFastStatusText
	.byte "OPFORGE-PRVM-ITER smoke FAIL fail-fast status", 10, 0
FailureFailFastRoutedText
	.byte "OPFORGE-PRVM-ITER smoke FAIL fail-fast routed", 10, 0
FailureFailFastFailLineText
	.byte "OPFORGE-PRVM-ITER smoke FAIL fail-fast line", 10, 0
FailureFailFastTotalText
	.byte "OPFORGE-PRVM-ITER smoke FAIL fail-fast total", 10, 0

SourceText
	.byte "start: NOP", 10, "start: NOP"
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

IteratorStatus
	.long 0
IteratorRoutedCount
	.long 0
IteratorFailLine
	.long 0
IteratorTotalLines
	.long 0

IteratorFrame
	.fill byte, 116, 0
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
PrvmIteratorEntryPtr
	.long line_iterator.prvmIterateLines68000

	.endsection
	.use prvm.amigaos.line_iterator
	.output "build/prvm_line_iterator_smoke.hunk", format=hunk, sections=entry, code
	.endmodule
