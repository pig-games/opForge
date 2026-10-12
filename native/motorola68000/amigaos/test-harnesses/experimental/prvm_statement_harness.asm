; Current generated signature/token/program cases with live shared Rust oracles.
	.module main
	.cpu 68020
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_compile as compile
	.use prvm.amigaos.statement_match as matcher
	.use prvm.amigaos.statement_head as head
	.use prvm.amigaos.statement_text as text
	.priv
CaseHeader	.struct
Mode	.long ?
Budget	.long ?
OutputCapacity	.long ?
WorkspaceCapacity	.long ?
ProgramBytes	.long ?
PlanBytes	.long ?
TokenCount	.long ?
ArenaBytes	.long ?
NamesCount	.long ?
	.endstruct
CASE_HEADER_BYTES = CaseHeader.NamesCount+4
Result	.struct
Status	.long ?
Primary	.long ?
Secondary	.long ?
Tertiary	.long ?
PayloadBytes	.long ?
	.endstruct
RESULT_BYTES = Result.PayloadBytes+4
CASE_COMPILE = 1
CASE_MATCH = 2
CASE_HEAD = 3
CASE_TEXT = 4
CASE_HEAD_COMPLETE = 5
OUTPUT_BYTES = 8192
WORKSPACE_BYTES = 16384
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.pub
	.section entry, kind=code
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	jsr cases
	move.l d0, ReturnCode
	movea.l DosBase, a6
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.endsection
	.priv
	.section code, kind=code
; Execute every counted fixture, checking exact exhaustion and atomic publication.
; D0=guest return code; clobbers D1-D7/A0-A6/CCR.
cases	.block
	move.l #Fixture, Cursor
	moveq #4, d0
	bsr.w take
	bne.w fail
	move.l (a0), d7
	bmi.w fail
next
	tst.l d7
	beq.w exhausted
	move.l #CASE_HEADER_BYTES, d0
	bsr.w take
	bne.w fail
	movea.l a0, a4
	move.l CaseHeader.ProgramBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, ProgramPtr
	move.l CaseHeader.PlanBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, PlanPtr
	move.l CaseHeader.TokenCount(a4), d0
	cmpi.l #$03ffffff, d0
	bhi.w fail
	lsl.l #5, d0  ; abi.TOKEN_BYTES = 32
	bsr.w take
	bne.w fail
	move.l a0, TokenPtr
	move.l CaseHeader.ArenaBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, ArenaPtr
	move.l CaseHeader.NamesCount(a4), d0
	cmpi.l #$3fffffff, d0
	bhi.w fail
	add.l d0, d0
	bsr.w take
	bne.w fail
	move.l a0, NamesPtr
	cmpi.l #OUTPUT_BYTES, CaseHeader.OutputCapacity(a4)
	bhi.w fail
	cmpi.l #WORKSPACE_BYTES, CaseHeader.WorkspaceCapacity(a4)
	bhi.w fail
	lea OutputBuffer, a0
	move.w #OUTPUT_BYTES/4-1, d0
fill
	move.l #$a5a5a5a5, (a0)+
	dbra d0, fill
	clr.l ResultData+Result.Tertiary
	move.l CaseHeader.Budget(a4), TextBudget
	bsr.w invoke
	move.l d0, ResultData+Result.Status
	move.l d1, ResultData+Result.Primary
	move.l d2, ResultData+Result.Secondary
	moveq #0, d4
	tst.l d0
	bne.w payloadReady
	cmpi.l #CASE_HEAD, (a4)  ; CaseHeader.Mode (zero offset)
	beq.w headResult
	cmpi.l #CASE_HEAD_COMPLETE, (a4)
	beq.w headResult
	cmpi.l #CASE_MATCH, (a4)
	beq.w matchResult
	move.l d1, d4
	bra.w payloadReady
headResult
	move.l d3, ResultData+Result.Tertiary
	bra.w payloadReady
matchResult
	tst.l d1
	beq.w payloadReady
	move.l d2, d4
	cmpi.l #OUTPUT_BYTES/abi.CAPTURE_BYTES, d4
	bhi.w fail
	lsl.l #2, d4
	move.l d4, d0
	add.l d4, d4
	add.l d0, d4  ; abi.CAPTURE_BYTES = 3*4
payloadReady
	cmpi.l #OUTPUT_BYTES, d4
	bhi.w fail
	move.l d4, ResultData+Result.PayloadBytes
	lea OutputBuffer, a0
	adda.l d4, a0
	move.l #OUTPUT_BYTES, d0
	sub.l d4, d0
guard
	tst.l d0
	beq.w write
	cmpi.b #$a5, (a0)+
	bne.w fail
	subq.l #1, d0
	bra.w guard
write
	lea ResultData, a0
	moveq #RESULT_BYTES, d0
	bsr.w writeSpan
	bne.w fail
	lea OutputBuffer, a0
	move.l d4, d0
	bsr.w writeSpan
	bne.w fail
	subq.l #1, d7
	bra.w next
exhausted
	movea.l Cursor, a0
	lea FixtureEnd, a1
	cmpa.l a1, a0
	bne.w fail
	moveq #0, d0
	rts
fail
	moveq #20, d0
	rts
	.bend  ; cases

; A4=CaseHeader. D0/status,D1/primary,D2/secondary,D3/head tertiary.
; Caller owns fixture metadata and aligned input spans; request services validate.
invoke	.block
	cmpi.l #CASE_COMPILE, (a4)  ; CaseHeader.Mode (zero offset)
	beq.w compileCase
	cmpi.l #CASE_MATCH, (a4)
	beq.w matchCase
	cmpi.l #CASE_HEAD, (a4)
	beq.w headCase
	cmpi.l #CASE_HEAD_COMPLETE, (a4)
	beq.w headCase
	cmpi.l #CASE_TEXT, (a4)
	beq.w textCase
	moveq #abi.INVALID_ARGUMENT, d0
	moveq #0, d1
	moveq #0, d2
	rts
compileCase
	lea CompileRequest, a0
	move.l ProgramPtr, (a0)  ; CompileFrame.ProgramPtr (zero offset)
	move.l CaseHeader.ProgramBytes(a4), abi.CompileFrame.ProgramBytes(a0)
	move.l TokenPtr, abi.CompileFrame.TokenPtr(a0)
	move.l CaseHeader.TokenCount(a4), abi.CompileFrame.TokenCount(a0)
	move.l ArenaPtr, abi.CompileFrame.ArenaPtr(a0)
	move.l CaseHeader.ArenaBytes(a4), abi.CompileFrame.ArenaBytes(a0)
	move.l #OutputBuffer, abi.CompileFrame.OutputPtr(a0)
	move.l CaseHeader.OutputCapacity(a4), abi.CompileFrame.OutputCapacity(a0)
	move.l #Workspace, abi.CompileFrame.WorkspacePtr(a0)
	move.l CaseHeader.WorkspaceCapacity(a4), abi.CompileFrame.WorkspaceCapacity(a0)
	move.l CaseHeader.Budget(a4), abi.CompileFrame.StepBudget(a0)
	moveq #abi.COMPILE_FRAME_BYTES, d0
	jsr compile.run
	rts
matchCase
	lea MatchRequest, a0
	move.l ProgramPtr, (a0)  ; MatchFrame.ProgramPtr (zero offset)
	move.l CaseHeader.ProgramBytes(a4), abi.MatchFrame.ProgramBytes(a0)
	move.l PlanPtr, abi.MatchFrame.PlanPtr(a0)
	move.l CaseHeader.PlanBytes(a4), abi.MatchFrame.PlanBytes(a0)
	move.l TokenPtr, abi.MatchFrame.TokenPtr(a0)
	move.l CaseHeader.TokenCount(a4), abi.MatchFrame.TokenCount(a0)
	move.l ArenaPtr, abi.MatchFrame.ArenaPtr(a0)
	move.l CaseHeader.ArenaBytes(a4), abi.MatchFrame.ArenaBytes(a0)
	move.l #OutputBuffer, abi.MatchFrame.OutputPtr(a0)
	move.l CaseHeader.OutputCapacity(a4), abi.MatchFrame.OutputCapacity(a0)
	move.l #Workspace, abi.MatchFrame.WorkspacePtr(a0)
	move.l CaseHeader.WorkspaceCapacity(a4), abi.MatchFrame.WorkspaceCapacity(a0)
	move.l CaseHeader.Budget(a4), abi.MatchFrame.StepBudget(a0)
	moveq #abi.MATCH_FRAME_BYTES, d0
	jsr matcher.run
	rts
headCase
	lea HeadRequest, a0
	move.l ProgramPtr, (a0)  ; HeadFrame.ProgramPtr (zero offset)
	move.l CaseHeader.ProgramBytes(a4), abi.HeadFrame.ProgramBytes(a0)
	move.l TokenPtr, abi.HeadFrame.TokenPtr(a0)
	move.l CaseHeader.TokenCount(a4), abi.HeadFrame.TokenCount(a0)
	move.l NamesPtr, abi.HeadFrame.NamesPtr(a0)
	move.l CaseHeader.NamesCount(a4), abi.HeadFrame.NamesCount(a0)
	clr.l abi.HeadFrame.Mode(a0)
	cmpi.l #CASE_HEAD_COMPLETE, (a4)
	bne.w headReady
	move.l #abi.HEAD_COMPLETE, abi.HeadFrame.Mode(a0)
headReady
	move.l CaseHeader.Budget(a4), abi.HeadFrame.StepBudget(a0)
	moveq #abi.HEAD_FRAME_BYTES, d0
	jsr head.run
	rts
textCase
	movea.l ArenaPtr, a0
	move.l CaseHeader.ArenaBytes(a4), d0
	lea OutputBuffer, a1
	move.l CaseHeader.OutputCapacity(a4), d1
	lea TextBudget, a2
	jsr text.lossy
	moveq #0, d2
	tst.l d0
	rts
	.bend  ; invoke

; D0=nonnegative bytes. A0=bounded fixture span,D0=status/CCR; others preserved.
take	.block
	movem.l d1, -(sp)
	tst.l d0
	bmi.w invalid
	move.l #FixtureEnd, d1
	sub.l Cursor, d1
	cmp.l d1, d0
	bhi.w invalid
	movea.l Cursor, a0
	add.l d0, Cursor
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1
	rts
	.bend  ; take
; D0=raw bytes; consume its even-byte-padded fixture block. Same outputs as take.
padded	.block
	tst.l d0
	bmi.w invalid
	btst #0, d0
	beq.w ready
	addq.l #1, d0
ready
	bsr.w take
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; padded
; A0/D0=output span. D0=status/CCR; others preserved. Exact partial-write loop.
writeSpan	.block
	movem.l d1-d5/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d5
next
	tst.l d5
	beq.w valid
	movea.l DosBase, a6
	move.l OutputHandle, d1
	move.l a3, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w fail
	cmp.l d5, d0
	bhi.w fail
	adda.l d0, a3
	sub.l d0, d5
	bra.w next
valid
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1-d5/a0-a6
	rts
	.bend  ; writeSpan
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:statement-results.bin", 0
	.align 2
Fixture	.incbin "statement-cases.bin"
FixtureEnd
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
Cursor	.res long, 1
ProgramPtr	.res long, 1
PlanPtr	.res long, 1
TokenPtr	.res long, 1
ArenaPtr	.res long, 1
NamesPtr	.res long, 1
TextBudget	.res long, 1
CompileRequest	.res byte, abi.COMPILE_FRAME_BYTES
MatchRequest	.res byte, abi.MATCH_FRAME_BYTES
HeadRequest	.res byte, abi.HEAD_FRAME_BYTES
ResultData	.res byte, RESULT_BYTES
OutputBuffer	.res byte, OUTPUT_BYTES
Workspace	.res byte, WORKSPACE_BYTES
	.endsection
	.output "statement.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
