; Current generated signature/token/program cases with live shared Rust oracles.
	.module main
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.vm.prvm.statement_compile as compile
	.use opforge.vm.prvm.statement_match as matcher
	.use opforge.vm.prvm.statement_head as head
	.use opforge.vm.prvm.statement_text as text
	.use opforge.tests.assembler.prvm_stmt_invoke_harness as invocation
	.priv
CASE_HEADER_BYTES = invocation.HEADER_BYTES
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
PLAN_CAPACITY = 256
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
	move.l invocation.Header.ProgramBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, ProgramPtr
	move.l invocation.Header.PlanBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, PlanPtr
	move.l invocation.Header.TokenCount(a4), d0
	cmpi.l #$03ffffff, d0
	bhi.w fail
	lsl.l #5, d0  ; abi.TOKEN_BYTES = 32
	bsr.w take
	bne.w fail
	move.l a0, TokenPtr
	move.l invocation.Header.ArenaBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, ArenaPtr
	move.l invocation.Header.NamesCount(a4), d0
	cmpi.l #$3fffffff, d0
	bhi.w fail
	add.l d0, d0
	bsr.w take
	bne.w fail
	move.l a0, NamesPtr
	move.l invocation.Header.ScopeCount(a4), d0
	cmpi.l #$07ffffff, d0
	bhi.w fail
	lsl.l #4, d0  ; abi.SCOPE_ROW_BYTES
	bsr.w take
	bne.w fail
	move.l a0, InvocationContext+invocation.Context.Scope
	move.l invocation.Header.KeywordsBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, InvocationContext+invocation.Context.Keywords
	move.l invocation.Header.PlanCount(a4), d6
	cmpi.l #PLAN_CAPACITY, d6
	bhi.w fail
	lea PlanRefs, a5
plan
	tst.l d6
	beq.w plansReady
	moveq #4, d0
	bsr.w take
	bne.w fail
	move.l (a0), d5
	move.l d5, d0
	bsr.w padded
	bne.w fail
	move.l a0, (a5)+  ; PlanRef.Ptr (zero offset)
	move.l d5, (a5)+
	subq.l #1, d6
	bra.w plan
plansReady
	move.l invocation.Header.HeadBytes(a4), d0
	bsr.w padded
	bne.w fail
	move.l a0, InvocationContext+invocation.Context.Head
	cmpi.l #OUTPUT_BYTES, invocation.Header.OutputCapacity(a4)
	bhi.w fail
	cmpi.l #WORKSPACE_BYTES, invocation.Header.WorkspaceCapacity(a4)
	bhi.w fail
	lea OutputBuffer, a0
	move.w #2*OUTPUT_BYTES/4-1, d0
fill
	move.l #$a5a5a5a5, (a0)+
	dbra d0, fill
	clr.l ResultData+Result.Tertiary
	move.l invocation.Header.Budget(a4), TextBudget
	bsr.w invoke
	move.l d0, ResultData+Result.Status
	move.l d1, ResultData+Result.Primary
	move.l d2, ResultData+Result.Secondary
	moveq #0, d4
	tst.l d0
	bne.w payloadReady
	cmpi.l #CASE_HEAD, (a4)  ; invocation.Header.Mode (zero offset)
	beq.w headResult
	cmpi.l #CASE_HEAD_COMPLETE, (a4)
	beq.w headResult
	cmpi.l #invocation.CASE_ROUTE, (a4)
	beq.w routeResult
	cmpi.l #invocation.CASE_FALLBACK, (a4)
	beq.w fallbackResult
	cmpi.l #invocation.CASE_SELECT, (a4)
	beq.w selectResult
	cmpi.l #CASE_MATCH, (a4)
	beq.w matchResult
	move.l d1, d4
	bra.w payloadReady
routeResult
	move.l d3, ResultData+Result.Tertiary
	tst.l d1
	beq.w payloadReady
	moveq #abi.ROUTE_BYTES, d4
	bra.w payloadReady
fallbackResult
	move.l d2, d4
	bra.w payloadReady
selectResult
	move.l d3, ResultData+Result.Tertiary
	tst.l d1
	beq.w payloadReady
	move.l d2, d4
	mulu.l #abi.CAPTURE_BYTES, d4
	addi.l #abi.SELECTION_BYTES, d4
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
	lea OutputBuffer, a0
	move.l d4, d0
	bsr.w guardBuffer
	bne.w fail
	moveq #0, d5
	cmpi.l #invocation.CASE_SELECT, (a4)
	bne.w guardFallback
	move.l ResultData+Result.Tertiary, d5
	cmpi.l #OUTPUT_BYTES/abi.TOKEN_BYTES, d5
	bhi.w fail
	lsl.l #5, d5
guardFallback
	lea FallbackBuffer, a0
	move.l d5, d0
	bsr.w guardBuffer
	bne.w fail
	move.l d4, d0
	add.l d5, d0
	cmpi.l #OUTPUT_BYTES, d0
	bhi.w fail
	move.l d0, ResultData+Result.PayloadBytes
	lea OutputBuffer, a0
	adda.l d4, a0
	lea FallbackBuffer, a1
	move.l d5, d0
appendFallback
	tst.l d0
	beq.w combined
	move.b (a1)+, (a0)+
	subq.l #1, d0
	bra.w appendFallback
combined
	add.l d5, d4

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

; A4=invocation.Header. D0/status,D1/primary,D2/secondary,D3/head tertiary.
; Caller owns fixture metadata and aligned input spans; request services validate.
invoke	.block
	cmpi.l #invocation.CASE_ROUTE, (a4)  ; invocation.Header.Mode (zero offset)
	bhs.w invocationCase
	cmpi.l #CASE_COMPILE, (a4)
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
invocationCase
	move.l ProgramPtr, InvocationContext+invocation.Context.Program
	move.l TokenPtr, InvocationContext+invocation.Context.Tokens
	move.l ArenaPtr, InvocationContext+invocation.Context.Arena
	move.l #PlanRefs, InvocationContext+invocation.Context.Plans
	move.l #OutputBuffer, InvocationContext+invocation.Context.Output
	move.l #FallbackBuffer, InvocationContext+invocation.Context.Fallback
	move.l #Workspace, InvocationContext+invocation.Context.Workspace
	lea InvocationContext, a0
	movea.l a4, a1
	jsr invocation.run
	rts
compileCase
	lea CompileRequest, a0
	move.l ProgramPtr, (a0)  ; CompileFrame.ProgramPtr (zero offset)
	move.l invocation.Header.ProgramBytes(a4), abi.CompileFrame.ProgramBytes(a0)
	move.l TokenPtr, abi.CompileFrame.TokenPtr(a0)
	move.l invocation.Header.TokenCount(a4), abi.CompileFrame.TokenCount(a0)
	move.l ArenaPtr, abi.CompileFrame.ArenaPtr(a0)
	move.l invocation.Header.ArenaBytes(a4), abi.CompileFrame.ArenaBytes(a0)
	move.l #OutputBuffer, abi.CompileFrame.OutputPtr(a0)
	move.l invocation.Header.OutputCapacity(a4), abi.CompileFrame.OutputCapacity(a0)
	move.l #Workspace, abi.CompileFrame.WorkspacePtr(a0)
	move.l invocation.Header.WorkspaceCapacity(a4), abi.CompileFrame.WorkspaceCapacity(a0)
	move.l invocation.Header.Budget(a4), abi.CompileFrame.StepBudget(a0)
	moveq #abi.COMPILE_FRAME_BYTES, d0
	jsr compile.run
	rts
matchCase
	lea MatchRequest, a0
	move.l ProgramPtr, (a0)  ; MatchFrame.ProgramPtr (zero offset)
	move.l invocation.Header.ProgramBytes(a4), abi.MatchFrame.ProgramBytes(a0)
	move.l PlanPtr, abi.MatchFrame.PlanPtr(a0)
	move.l invocation.Header.PlanBytes(a4), abi.MatchFrame.PlanBytes(a0)
	move.l TokenPtr, abi.MatchFrame.TokenPtr(a0)
	move.l invocation.Header.TokenCount(a4), abi.MatchFrame.TokenCount(a0)
	move.l ArenaPtr, abi.MatchFrame.ArenaPtr(a0)
	move.l invocation.Header.ArenaBytes(a4), abi.MatchFrame.ArenaBytes(a0)
	move.l #OutputBuffer, abi.MatchFrame.OutputPtr(a0)
	move.l invocation.Header.OutputCapacity(a4), abi.MatchFrame.OutputCapacity(a0)
	move.l #Workspace, abi.MatchFrame.WorkspacePtr(a0)
	move.l invocation.Header.WorkspaceCapacity(a4), abi.MatchFrame.WorkspaceCapacity(a0)
	move.l invocation.Header.Budget(a4), abi.MatchFrame.StepBudget(a0)
	moveq #abi.MATCH_FRAME_BYTES, d0
	jsr matcher.run
	rts
headCase
	lea HeadRequest, a0
	move.l ProgramPtr, (a0)  ; HeadFrame.ProgramPtr (zero offset)
	move.l invocation.Header.ProgramBytes(a4), abi.HeadFrame.ProgramBytes(a0)
	move.l TokenPtr, abi.HeadFrame.TokenPtr(a0)
	move.l invocation.Header.TokenCount(a4), abi.HeadFrame.TokenCount(a0)
	move.l NamesPtr, abi.HeadFrame.NamesPtr(a0)
	move.l invocation.Header.NamesCount(a4), abi.HeadFrame.NamesCount(a0)
	clr.l abi.HeadFrame.Mode(a0)
	cmpi.l #CASE_HEAD_COMPLETE, (a4)
	bne.w headReady
	move.l #abi.HEAD_COMPLETE, abi.HeadFrame.Mode(a0)
headReady
	move.l invocation.Header.Budget(a4), abi.HeadFrame.StepBudget(a0)
	moveq #abi.HEAD_FRAME_BYTES, d0
	jsr head.run
	rts
textCase
	movea.l ArenaPtr, a0
	move.l invocation.Header.ArenaBytes(a4), d0
	lea OutputBuffer, a1
	move.l invocation.Header.OutputCapacity(a4), d1
	lea TextBudget, a2
	jsr text.lossy
	moveq #0, d2
	tst.l d0
	rts
	.bend  ; invoke

; A0=output buffer,D0=published bytes. D0=status/CCR; others preserved.
guardBuffer	.block
	movem.l d1/a0, -(sp)
	adda.l d0, a0
	move.l #OUTPUT_BYTES, d1
	sub.l d0, d1
next
	tst.l d1
	beq.w valid
	cmpi.b #$a5, (a0)+
	bne.w invalid
	subq.l #1, d1
	bra.w next
valid
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1/a0
	rts
	.bend

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
InvocationContext	.res byte, invocation.CONTEXT_BYTES
PlanRefs	.res byte, PLAN_CAPACITY*abi.PLAN_REF_BYTES
ResultData	.res byte, RESULT_BYTES
OutputBuffer	.res byte, OUTPUT_BYTES
FallbackBuffer	.res byte, OUTPUT_BYTES
Workspace	.res byte, WORKSPACE_BYTES
	.endsection
	.output "statement.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
