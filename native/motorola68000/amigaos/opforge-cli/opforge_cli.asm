; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module main
	.cpu 68020
	.use opasm.amigaos.engine (opasmEngineAssemblySessionStart, opasmEngineStmtCount)
	.use opasm.amigaos.engine (opasmEngineImageByteCount, opasmEngineSessionCpuName)

	.use opforge.cli.constants (*)
	.use opforge.cli.state (*)
	.use opforge.cli.strings (*)
	.use opforge.cli.dos (*)
	.use opforge.cli.tkpkg_control_block (*)
	.use opforge.cli.source_reader (*)
	.use opforge.cli.copy (*)
	.use opforge.cli.path (*)
	.use opforge.cli.text_output (*)
	.use opforge.cli.report (*)
	.use opforge.cli.args (*)
	.use opforge.cli.encode_eval_bridge (*)
	.use opforge.cli.output (*)
	.use opforge.cli.engine_callbacks (*)

	.section entry, kind=code

	.pub

; ---------------------------------------------------------------------------
; AmigaOS process entry for the native opForge CLI.
;
; Workbench launches are rejected for this deliverable slice because the native
; host contract is currently Shell/file based. Shell launches hand off to
; opforge_native_cli_run, which mirrors the Rust CLI orchestration surface for
; the supported native subset.
;
; Inputs:
; - AmigaOS process context; no explicit arguments.
;
; Outputs:
; - D0: AmigaDOS return code.
; ---------------------------------------------------------------------------
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	clr.l d2  ; no Workbench startup message is pending until GetMsg succeeds

	suba.l a1, a1  ; Exec FindTask(NULL) => current process
	movea.l SYS_BASE.W, a6  ; Exec base for process and Workbench-message calls
	jsr FIND_TASK(a6)

	movea.l d0, a2
	tst.l PR_CLI(a2)  ; nonzero means Shell launch; zero means Workbench activation
	bne.w opforgeStartCli

	lea PR_MSG_PORT(a2), a0
	jsr WAIT_PORT(a6)
	lea PR_MSG_PORT(a2), a0
	jsr GET_MSG(a6)
	move.l d0, d2  ; preserve startup message so ReplyMsg can be sent before exit
	moveq #RETURN_WORKBENCH_UNSUPPORTED, d7
	bra.w opforgeStartReply

opforgeStartCli
	jsr opforgeNativeCliRun  ; run the Shell-native CLI host path
	move.l d0, d7  ; keep return code stable across optional Workbench reply path

opforgeStartReply
	tst.l d2
	beq.w opforgeStartDone
	jsr FORBID(a6)
	movea.l d2, a1
	jsr REPLY_MSG(a6)

opforgeStartDone
	move.l d7, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts

	.endsection

	.section code, kind=code
; ---------------------------------------------------------------------------
; Run one native opForge CLI invocation.
;
; This host entry owns AmigaDOS setup, argument parsing, package staging, source
; processing, report emission, and output-file writes. It should not grow new
; assembler semantics; future implementation work should move semantic ownership
; into native opasm/tkpkg/opcore stages.
;
; Inputs:
; - Shell argument tail is read from dos.library/GetArgStr, unless an FS-UAE
;   smoke define supplies a deterministic test argument tail.
;
; Outputs:
; - D0: AmigaDOS return code.
; - textual OPFORGE-NATIVE report is written to stdout.
; - flat `.bin` output is written when selected and image bytes exist.
; ---------------------------------------------------------------------------
opforgeNativeCliRun
	movem.l d2-d7/a2-a6, -(sp)
	move.l #RETURN_USAGE, NativeCliReturnCode

	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.W, a6  ; first try the AmigaOS 2.x+ dos.library version expected by tests
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	bne.s opforgeNativeCliHaveDos

	lea DosName, a1
	moveq #0, d0
	movea.l SYS_BASE.W, a6  ; fallback keeps older emulator images usable for smoke runs
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w opforgeNativeCliDone

opforgeNativeCliHaveDos
	move.l d0, NativeCliDosBase  ; all file/console calls below dispatch through dos.library base
	bsr.w opforgeNativeCliInitModuleUseState
	movea.l d0, a6
	jsr GET_ARG_STR(a6)
.ifdef OPFORGE_FS_UAE_SMOKE
	lea defaultFsUaeArgTail, a0
.else
	movea.l d0, a0
.endif
	jsr opforgeNativeCliParseArgs

	cmpi.w #NCLI_PARSE_HELP, d0
	beq.w opforgeNativeCliHelp
	cmpi.w #NCLI_PARSE_VERSION, d0
	beq.w opforgeNativeCliVersion
	tst.w d0
	beq.w opforgeNativeCliParsed

	jsr opforgeNativeCliReportParseError
	move.l #RETURN_USAGE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliHelp
	move.l #HelpText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliVersion
	move.l #VersionText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliParsed
	lea NativeCliInputPath, a0
	jsr opforgeNativeCliOpenInput
	tst.l d0
	bne.s opforgeNativeCliInputOpened
	move.l #InputOpenErrorText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliInputPath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_FILE_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliInputOpened
	move.l d0, d1
	jsr opforgeNativeCliClose
	cmpi.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	bne.s opforgeNativeCliOutputFormatReady
	move.l #NativeHunkNotImplementedText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_NOT_IMPLEMENTED, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliOutputFormatReady
	move.l #StubHeaderText, d1
	jsr opforgeNativeCliPutStr
	move.l #InputLabelText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliInputPath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #BinLabelText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliBinPath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	bsr.w opforgeNativeCliInitAssemblySession
	jsr opforgeNativeCliEmitModulePathRecords
	bsr.w opforgeNativeCliTokenizeFrontend
	tst.l d0
	beq.s opforgeNativeCliTokenizerOk
	move.l #TokenizerFailureText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_RUNTIME_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliTokenizerOk
	move.l #ParserOkText, d1
	jsr opforgeNativeCliPutStr
	jsr opforgeNativeCliRunTwoPassEngine
	tst.l d0
	beq.s opforgeNativeCliPassesOk
	move.l #NativePassFailureText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_RUNTIME_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliPassesOk
	jsr opforgeNativeCliEmitAssemblySessionSummary
	tst.w opasmEngineImageByteCount.l
	beq.s opforgeNativeCliEmitStub
	jsr opforgeNativeCliWriteFlatOutput
	tst.l d0
	beq.s opforgeNativeCliOutputOk
	move.l #NativeOutputFailureText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_FILE_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliOutputOk
	move.l #NativeOutputOkText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliEmitStub
	move.l #EmitterStubText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_NOT_IMPLEMENTED, NativeCliReturnCode

opforgeNativeCliCloseDos
	move.l NativeCliDosBase, d0
	beq.s opforgeNativeCliDone
	movea.l SYS_BASE.W, a6
	movea.l d0, a1
	jsr CLOSE_LIBRARY(a6)
	clr.l NativeCliDosBase

opforgeNativeCliDone
	move.l NativeCliReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts

opforgeNativeCliCopyOperandText
	movem.l d0-d4/a0-a1, -(sp)
	clr.w d5
	move.l #TOKEN_BUFFER_CAPACITY - 1, d4

opforgeNativeCliCopyOperandTextLoop
	tst.l d0
	beq.s opforgeNativeCliCopyOperandTextDone
	moveq #0, d2
	move.b (a0), d2
	beq.s opforgeNativeCliCopyOperandTextDone
	cmpi.b #';', d2
	beq.s opforgeNativeCliCopyOperandTextDone
	cmpi.b #10, d2
	beq.s opforgeNativeCliCopyOperandTextDone
	cmpi.b #13, d2
	beq.s opforgeNativeCliCopyOperandTextDone
	tst.l d4
	beq.s opforgeNativeCliCopyOperandTextDone
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d5
	subq.l #1, d4
	bra.s opforgeNativeCliCopyOperandTextLoop

opforgeNativeCliCopyOperandTextDone
	bsr.w opforgeNativeCliTrimCopiedOperandText
	clr.b (a1)
	movem.l (sp)+, d0-d4/a0-a1
	rts

opforgeNativeCliTrimCopiedOperandText
	tst.w d5
	beq.s opforgeNativeCliTrimCopiedOperandTextDone
	movea.l a1, a0

opforgeNativeCliTrimCopiedOperandTextLoop
	tst.w d5
	beq.s opforgeNativeCliTrimCopiedOperandTextSetEnd
	subq.l #1, a0
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.s opforgeNativeCliTrimCopiedOperandTextOne
	cmpi.b #9, d0
	beq.s opforgeNativeCliTrimCopiedOperandTextOne
	bra.s opforgeNativeCliTrimCopiedOperandTextSetEnd

opforgeNativeCliTrimCopiedOperandTextOne
	subq.w #1, d5
	bra.s opforgeNativeCliTrimCopiedOperandTextLoop

opforgeNativeCliTrimCopiedOperandTextSetEnd
	movea.l a0, a1
	addq.l #1, a1

opforgeNativeCliTrimCopiedOperandTextDone
	rts

opforgeNativeCliEmitStatementExprRequest
	move.l #StatementExprText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprOperandIndex, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSlotIndex, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprStartToken, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprEndToken, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanLine, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanStart, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanEnd, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	rts

opforgeNativeCliCopyIncludeTarget
	tst.l d0
	beq.w opforgeNativeCliCopyIncludeTargetFail
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #'"', d2
	beq.s opforgeNativeCliCopyIncludeTargetQuoted
	cmpi.b #39, d2
	beq.s opforgeNativeCliCopyIncludeTargetQuoted
	move.l #PATH_BUFFER_CAPACITY - 1, d6
	clr.l d5

opforgeNativeCliCopyIncludeTargetBareLoop
	tst.l d0
	beq.s opforgeNativeCliCopyIncludeTargetDone
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	cmpi.b #9, d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	cmpi.b #';', d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	tst.l d6
	beq.s opforgeNativeCliCopyIncludeTargetFail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s opforgeNativeCliCopyIncludeTargetBareLoop

opforgeNativeCliCopyIncludeTargetQuoted
	move.b d2, d4
	addq.l #1, a0
	subq.l #1, d0
	move.l #PATH_BUFFER_CAPACITY - 1, d6
	clr.l d5

opforgeNativeCliCopyIncludeTargetQuotedLoop
	tst.l d0
	beq.s opforgeNativeCliCopyIncludeTargetFail
	moveq #0, d2
	move.b (a0), d2
	cmp.b d4, d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	tst.l d6
	beq.s opforgeNativeCliCopyIncludeTargetFail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s opforgeNativeCliCopyIncludeTargetQuotedLoop

opforgeNativeCliCopyIncludeTargetDone
	tst.l d5
	beq.s opforgeNativeCliCopyIncludeTargetFail
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliCopyIncludeTargetFail
	moveq #1, d0
	rts

; Expand the one-level native `.include` target and emit include report records.
opforgeNativeCliExpandIncludeTarget
	tst.w NativeCliIncludeDepth
	bne.w opforgeNativeCliExpandIncludeFail
	bsr.w opforgeNativeCliResolveIncludePath
	tst.l d0
	bne.w opforgeNativeCliExpandIncludeFail

	move.w NativeCliSourceLineLen, d0
	move.w d0, NativeCliSavedLineLen
	move.w NativeCliSawCr, d0
	move.w d0, NativeCliSavedSawCr
	move.l NativeCliSourceLineNum, d0
	move.l d0, NativeCliSavedLineNum
	lea NativeCliCurrentPath, a0
	lea NativeCliSavedPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w opforgeNativeCliExpandIncludeFail

	move.l #IncludeStageText, d1
	jsr opforgeNativeCliPutStr
	move.l #IncludeRootText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliIncludeRootPath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #IncludeFileText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliIncludePath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #IncludeEnterText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliCurrentPath, d1
	jsr opforgeNativeCliPutStr
	move.l #SpaceText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliIncludePath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr

	move.w #NATIVE_INCLUDE_DEPTH_LIMIT, NativeCliIncludeDepth
	lea NativeCliIncludePath, a0
	lea NativeCliCurrentPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliExpandIncludeRestoreFail
	lea NativeCliIncludePath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	tst.l d0
	bne.s opforgeNativeCliExpandIncludeRestoreFail

	move.l #IncludeLeaveText, d1
	jsr opforgeNativeCliPutStr
	move.l #IncludeOkText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	bra.s opforgeNativeCliExpandIncludeRestore

opforgeNativeCliExpandIncludeRestoreFail
	move.l #IncludeFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0

opforgeNativeCliExpandIncludeRestore
	move.w NativeCliSavedLineLen, d1
	move.w d1, NativeCliSourceLineLen
	move.w NativeCliSavedSawCr, d1
	move.w d1, NativeCliSawCr
	move.l NativeCliSavedLineNum, d1
	move.l d1, NativeCliSourceLineNum
	lea NativeCliSavedPath, a0
	lea NativeCliCurrentPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	clr.w NativeCliIncludeDepth
	rts

opforgeNativeCliExpandIncludeFail
	move.l #IncludeFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts

opforgeNativeCliResolveIncludePath
	lea NativeCliCurrentPath, a0
	lea NativeCliIncludeRootPath, a1
	jsr opforgeNativeCliCopyPathRoot
	tst.l d0
	bne.w opforgeNativeCliResolveIncludeFail
	lea NativeCliIncludeTarget, a0
	jsr opforgeNativeCliPathIsAbsolute
	tst.l d0
	beq.s opforgeNativeCliResolveIncludeRelative
	lea NativeCliIncludeTarget, a0
	lea NativeCliIncludePath, a1
	jsr opforgeNativeCliCopyPathBuffer
	rts

opforgeNativeCliResolveIncludeRelative
	lea NativeCliIncludeRootPath, a0
	lea NativeCliIncludePath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliResolveIncludeFail
	lea NativeCliIncludeTarget, a0
	lea NativeCliIncludePath, a1
	jsr opforgeNativeCliAppendPathBuffer
	rts

opforgeNativeCliResolveIncludeFail
	moveq #1, d0
	rts

; Initialize transitional native assembly-session state for the current CLI run.
opforgeNativeCliInitAssemblySession
	movem.l d0-d1/a0-a1, -(sp)
	lea opasmEngineAssemblySessionStart.l, a0
	move.l #NATIVE_ASSEMBLY_SESSION_BYTES, d0
	bsr.w opforgeNativeCliClearBytes
	lea NativeCliCpuName, a0
	tst.b (a0)
	bne.s opforgeNativeCliInitAssemblySessionHaveCpu
	lea DefaultCpuName, a0

opforgeNativeCliInitAssemblySessionHaveCpu
	lea opasmEngineSessionCpuName, a1
	bsr.w opforgeNativeCliCopySessionCpuName
	movem.l (sp)+, d0-d1/a0-a1
	moveq #0, d0
	rts

opforgeNativeCliCopySessionCpuName
	move.l #TOKEN_BUFFER_CAPACITY - 1, d0

opforgeNativeCliCopySessionCpuNameLoop
	move.b (a0)+, d1
	move.b d1, (a1)+
	beq.s opforgeNativeCliCopySessionCpuNameDone
	subq.l #1, d0
	bne.s opforgeNativeCliCopySessionCpuNameLoop
	clr.b -(a1)

opforgeNativeCliCopySessionCpuNameDone
	rts

; Clear module/use and statement collection state before parsing input.
opforgeNativeCliInitModuleUseState
	movem.l d0-d1/a0, -(sp)
	lea NativeCliModuleUseStateStart, a0
	move.l #NATIVE_MODULE_USE_STATE_BYTES, d0
	bsr.w opforgeNativeCliClearBytes
	clr.w opasmEngineStmtCount.l
	movem.l (sp)+, d0-d1/a0
	rts

; Clear D0 bytes at A0.
opforgeNativeCliClearBytes
	tst.l d0
	beq.s opforgeNativeCliClearBytesDone
	moveq #0, d1

opforgeNativeCliClearBytesLoop
	move.b d1, (a0)+
	subq.l #1, d0
	bne.s opforgeNativeCliClearBytesLoop

opforgeNativeCliClearBytesDone
	rts

	.bend  ; start
	.priv

	.endsection

	.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
	.endmodule
