; @opforge-evidence: Permanent initial macro descriptor contract fixture runner.
; Streaming native macro-entry PRVM fixture harness.
	.module main
	.cpu 68020
	.use prvm.amigaos.abi as prvm_abi
	.use prvm.amigaos.runtime

SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
PUT_STR = -948
OPEN = -30
CLOSE = -36
READ = -42
WRITE = -48
MODE_OLDFILE = 1005
MODE_NEWFILE = 1006
RETURN_FAIL = 20
RETURN_OK = 0
PROGRAM_CAPACITY = 256
SOURCE_CAPACITY = 4096
TOKEN_CAPACITY = 512
RESULT_CAPACITY = 2048
RESULT_RECORD_BYTES = 16

	.section entry, kind=code
	.pub
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w noDos
	move.l d0, DosBase
	lea StartText, a1
	move.l a1, d1
	bsr.w putStr
	bsr.w openFiles
	bne.w openFailed
	bsr.w runCases
	move.l d0, ReturnCode
	bsr.w closeFiles
closeDos
done
	move.l ReturnCode, d0
	beq.s reportOk
	lea FailText, a1
	bra.s report
reportOk
	lea OkText, a1
report
	move.l a1, d1
	bsr.w putStr
	movea.l DosBase, a1
	movea.l SYS_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
	move.l ReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
openFailed
	bsr.w closeFiles
	bra.s closeDos
noDos
	move.l #RETURN_FAIL, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start

; Print a DOS string. Inputs: D1 = zero-terminated text.
; Clobbers: D0/A6/CCR; DOSBase remains stored separately.
putStr	.block
	movea.l DosBase, a6
	jsr PUT_STR(a6)
	rts
	.bend  ; putStr

; Open input and truncate/create output. Outputs D0=0 or 1.
; Clobbers D0-D2/A6/CCR.
openFiles	.block
	movea.l DosBase, a6
	move.l #InputPath, d1
	move.l #MODE_OLDFILE, d2
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, InputHandle
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, OutputHandle
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; openFiles

; Close any opened files. DOS Close has no completion result to inspect.
; Clobbers D0-D1/A6/CCR.
closeFiles	.block
	movea.l DosBase, a6
	move.l InputHandle, d1
	beq.s closeOutput
	jsr CLOSE(a6)
	clr.l InputHandle
closeOutput
	move.l OutputHandle, d1
	beq.s done
	jsr CLOSE(a6)
	clr.l OutputHandle
done
	moveq #0, d0
	rts
	.bend  ; closeFiles

; Consume the count, then stream and execute one bounded case at a time.
; D0=0 only after exact input exhaustion and complete output writes.
; Clobbers D0-D7/A0-A6/CCR.
runCases	.block
	lea CaseCount, a0
	move.l a0, ReadPointer
	moveq #4, d0
	move.l d0, ReadRemaining
	bsr.w readExact
	tst.l d0
	bne.w fail
	move.l CaseCount, d7
caseLoop
	tst.l d7
	beq.w checkEnd
	lea CaseHeader, a0
	move.l a0, ReadPointer
	move.l #16, ReadRemaining
	bsr.w readExact
	tst.l d0
	bne.w fail
	bsr.w loadCase
	tst.l d0
	bne.w fail
	bsr.w invokeRuntime
	bsr.w writeCase
	tst.l d0
	bne.w fail
	subq.l #1, d7
	bra.w caseLoop
checkEnd
	movea.l DosBase, a6
	move.l InputHandle, d1
	lea ExtraByte, a0
	move.l a0, d2
	moveq #1, d3
	jsr READ(a6)
	tst.l d0
	bmi.s fail
	bne.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; runCases

; Validate the fixed header, then read padded program/source and lexical records.
; Inputs: CaseHeader; output buffers updated. D0=0 success, 1 malformed/I/O.
; Clobbers D0-D5/A0-A1/CCR.
loadCase	.block
	lea CaseHeader, a0
	move.l (a0), d0
	move.l d0, StepBudget
	move.l 4(a0), d0
	move.l d0, ResultLimit
	moveq #0, d0
	move.w 8(a0), d0
	cmpi.l #SOURCE_CAPACITY, d0
	bhi.w fail
	move.l d0, SourceLength
	moveq #0, d1
	move.w 10(a0), d1
	cmpi.l #PROGRAM_CAPACITY, d1
	bhi.w fail
	move.l d1, ProgramLength
	moveq #0, d2
	move.w 12(a0), d2
	cmpi.l #TOKEN_CAPACITY, d2
	bhi.w fail
	move.l d2, TokenCount
	move.w 14(a0), d0
	cmpi.w #prvm_abi.PRVM_ENTRY_KIND_MACRO_DESCRIPTORS, d0
	beq.w entryReady
	cmpi.w #prvm_abi.PRVM_ENTRY_KIND_PACKED_MACRO, d0
	beq.w entryReady
	cmpi.w #prvm_abi.PRVM_ENTRY_KIND_MACRO_FRAGMENTS, d0
	bne.w fail
entryReady
	move.w d0, EntryKind
	cmpi.l #RESULT_CAPACITY, ResultLimit
	bhi.w fail
	move.l ProgramLength, d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	move.l d0, ProgramPaddedLength
	lea ProgramBuffer, a0
	move.l a0, ReadPointer
	move.l d0, ReadRemaining
	bsr.w readExact
	tst.l d0
	bne.w fail
	move.l SourceLength, d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	move.l d0, SourcePaddedLength
	lea SourceBuffer, a0
	move.l a0, ReadPointer
	move.l d0, ReadRemaining
	bsr.w readExact
	tst.l d0
	bne.w fail
	move.l TokenCount, d0
	mulu.w #prvm_abi.PRVM_TOKEN_RECORD_SIZE, d0
	move.l d0, TokenBytes
	lea TokenBuffer, a0
	move.l a0, ReadPointer
	move.l d0, ReadRemaining
	bsr.w readExact
	tst.l d0
	bne.w fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; loadCase

; Initialize a fresh v1 macro request and call the shared PRVM runtime.
; Outputs stored as native BE longs in StatusRecord.
; Clobbers D0-D4/A0-A1/CCR; preserves DOSBase in memory.
invokeRuntime	.block
	move.w #RESULT_CAPACITY/4-1, d4
	lea ResultBytes, a0
fillResult
	move.l #$A5A5A5A5, (a0)+
	dbra d4, fillResult
	lea RequestFrame, a0
	move.l #prvm_abi.PRVM_MAGIC_OPRP, prvm_abi.PRVM_FRAME_MAGIC(a0)
	move.w #prvm_abi.PRVM_ABI_VERSION_V1, prvm_abi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #prvm_abi.PRVM_REQUEST_FRAME_SIZE, prvm_abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #prvm_abi.PRVM_CALL_MODE_START, prvm_abi.PRVM_FRAME_CALL_MODE(a0)
	move.w EntryKind, prvm_abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l #1, prvm_abi.PRVM_FRAME_LINE_NUM(a0)
	lea SourceBuffer, a1
	move.l a1, prvm_abi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l SourceLength, prvm_abi.PRVM_FRAME_SOURCE_LEN(a0)
	lea TokenBuffer, a1
	move.l a1, prvm_abi.PRVM_FRAME_TOKEN_PTR(a0)
	move.l TokenCount, prvm_abi.PRVM_FRAME_TOKEN_COUNT(a0)
	move.w #prvm_abi.PRVM_TOKEN_RECORD_SIZE, prvm_abi.PRVM_FRAME_TOKEN_RECORD_SIZE(a0)
	cmpi.w #prvm_abi.PRVM_ENTRY_KIND_PACKED_MACRO, EntryKind
	bne.w lexicalReady
	clr.l prvm_abi.PRVM_FRAME_TOKEN_PTR(a0)
	clr.l prvm_abi.PRVM_FRAME_TOKEN_COUNT(a0)
lexicalReady
	clr.w 34(a0)
	lea SourceBuffer, a1
	move.l a1, prvm_abi.PRVM_FRAME_LEXEME_PTR(a0)
	move.l SourceLength, prvm_abi.PRVM_FRAME_LEXEME_LEN(a0)
	lea ProgramBuffer, a1
	move.l a1, prvm_abi.PRVM_FRAME_PROGRAM_PTR(a0)
	move.l ProgramLength, prvm_abi.PRVM_FRAME_PROGRAM_LEN(a0)
	lea ResultBytes, a1
	move.l a1, prvm_abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l ResultLimit, prvm_abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	clr.l prvm_abi.PRVM_FRAME_DIAGNOSTIC_PTR(a0)
	clr.l prvm_abi.PRVM_FRAME_DIAGNOSTIC_CAPACITY(a0)
	clr.l prvm_abi.PRVM_FRAME_RESUME_PTR(a0)
	clr.l prvm_abi.PRVM_FRAME_RESUME_CAPACITY(a0)
	clr.l prvm_abi.PRVM_FRAME_EXPR_REQUEST_PTR(a0)
	clr.l prvm_abi.PRVM_FRAME_EXPR_REQUEST_SIZE(a0)
	clr.l prvm_abi.PRVM_FRAME_EXPR_RESULT_PTR(a0)
	clr.l prvm_abi.PRVM_FRAME_EXPR_RESULT_COUNT(a0)
	move.l #prvm_abi.PRVM_PARSER_CONTRACT_VERSION_V2, prvm_abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a0)
	move.l StepBudget, prvm_abi.PRVM_FRAME_STEP_BUDGET(a0)
	clr.l prvm_abi.PRVM_FRAME_FLAGS(a0)
	move.l #prvm_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr runtime.prvmRun68000.l
	lea StatusRecord, a0
	move.l d0, (a0)
	move.l d2, 4(a0)
	move.l d1, 8(a0)
	move.l d3, 12(a0)
	rts
	.bend  ; invokeRuntime

; Write status/count/offset/used and the complete sentinel-backed result area.
; D0=0 for exact output transfer, otherwise 1. Clobbers D0-D4/A0/A6/CCR.
writeCase	.block
	move.l OutputHandle, d1
	lea StatusRecord, a0
	move.l a0, d2
	move.l #RESULT_RECORD_BYTES, d3
	movea.l DosBase, a6
	jsr WRITE(a6)
	cmpi.l #RESULT_RECORD_BYTES, d0
	bne.s fail
	move.l OutputHandle, d1
	lea ResultBytes, a0
	move.l a0, d2
	move.l #RESULT_CAPACITY, d3
	jsr WRITE(a6)
	cmpi.l #RESULT_CAPACITY, d0
	bne.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; writeCase

; Read exactly ReadRemaining bytes into ReadPointer, allowing short DOS reads.
; Returns D0=0 success or 1 EOF/error; clobbers D0-D4/A0/A6/CCR.
readExact	.block
	move.l ReadPointer, ReadCursor
	move.l ReadRemaining, ReadLeft
readLoop
	tst.l ReadLeft
	beq.s done
	movea.l DosBase, a6
	move.l InputHandle, d1
	move.l ReadCursor, d2
	move.l ReadLeft, d3
	jsr READ(a6)
	tst.l d0
	ble.s fail
	add.l d0, ReadCursor
	sub.l d0, ReadLeft
	bra.s readLoop
done
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; readExact

	.endsection

	.section data, kind=data
DosName
	.byte "dos.library", 0
InputPath
	.byte "Work:prvm-macro-cases.bin", 0
OutputPath
	.byte "Work:build/prvm-macro-results.bin", 0
StartText
	.byte "OPFORGE-PRVM-MACRO smoke start", 10, 0
OkText
	.byte "OPFORGE-PRVM-MACRO smoke OK", 10, 0
FailText
	.byte "OPFORGE-PRVM-MACRO smoke FAIL", 10, 0
	.endsection

	.section bss, kind=bss
	.align 4
DosBase
	.res long, 1
InputHandle
	.res long, 1
OutputHandle
	.res long, 1
ReturnCode
	.res long, 1
ReadPointer
	.res long, 1
ReadRemaining
	.res long, 1
ReadCursor
	.res long, 1
ReadLeft
	.res long, 1
CaseCount
	.res long, 1
CaseHeader
	.res byte, 16
StepBudget
	.res long, 1
ResultLimit
	.res long, 1
SourceLength
	.res long, 1
ProgramLength
	.res long, 1
TokenCount
	.res long, 1
EntryKind
	.res word, 1
	.align 4
ProgramPaddedLength
	.res long, 1
SourcePaddedLength
	.res long, 1
TokenBytes
	.res long, 1
RequestFrame
	.res byte, prvm_abi.PRVM_REQUEST_FRAME_SIZE
StatusRecord
	.res long, 4
ProgramBuffer
	.res byte, PROGRAM_CAPACITY+2
SourceBuffer
	.res byte, SOURCE_CAPACITY+2
TokenBuffer
	.res byte, TOKEN_CAPACITY*prvm_abi.PRVM_TOKEN_RECORD_SIZE
ResultBytes
	.res byte, RESULT_CAPACITY
ExtraByte
	.res byte, 1
	.endsection

	.output "build/prvm_macro_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
