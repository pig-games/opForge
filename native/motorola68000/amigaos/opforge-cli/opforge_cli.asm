; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module main
	.cpu 68020
	.use opasm.amigaos.engine (opasmEngineAssemblySessionStart, opasmEngineStmtCount)
	.use opasm.amigaos.engine (opasmEngineSessionPass, opasmEngineSourceRecordCount)
	.use opasm.amigaos.engine (opasmEngineLabelCount, opasmEngineImageByteCount)
	.use opasm.amigaos.engine (opasmEngineSessionCpuName, opasmEngineSessionOrigin)
	.use opasm.amigaos.engine (opasmEngineSessionCurrentPc)

	.use opforge.cli.constants (*)
	.use opforge.cli.state (*)
	.use opforge.cli.strings (*)
	.use opforge.cli.dos (*)
	.use opforge.cli.tkpkg_control_block (*)
	.use opforge.cli.source_reader (*)
	.use opforge.cli.copy (*)
	.use opforge.cli.path (*)
	.use opforge.cli.token_util (*)
	.use opforge.cli.line_text (*)
	.use opforge.cli.text_output (*)
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
	bsr.w opforgeNativeCliParseArgs

	cmpi.w #NCLI_PARSE_HELP, d0
	beq.w opforgeNativeCliHelp
	cmpi.w #NCLI_PARSE_VERSION, d0
	beq.w opforgeNativeCliVersion
	tst.w d0
	beq.w opforgeNativeCliParsed

	bsr.w opforgeNativeCliReportParseError
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
	bsr.w opforgeNativeCliEmitModulePathRecords
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
	bsr.w opforgeNativeCliEmitAssemblySessionSummary
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

; Emit the current session summary records into the OPFORGE-NATIVE report.
opforgeNativeCliEmitAssemblySessionSummary
	movem.l d0-d2/a0-a1, -(sp)
	move.l #SessionStageText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionCpuText, d1
	jsr opforgeNativeCliPutStr
	move.l #opasmEngineSessionCpuName, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionPassText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineSessionPass.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionOriginText, d1
	jsr opforgeNativeCliPutStr
	move.l opasmEngineSessionOrigin.l, d0
	bsr.w opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionPcText, d1
	jsr opforgeNativeCliPutStr
	move.l opasmEngineSessionCurrentPc.l, d0
	bsr.w opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionSourceCountText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineSourceRecordCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionStmtCountText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionLabelCountText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionImageBytesText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #SessionReadyText, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d2/a0-a1
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

NCLI_PARSE_OK                   = 0
NCLI_PARSE_HELP                 = 1
NCLI_PARSE_VERSION              = 2
NCLI_PARSE_USAGE                = -1
NCLI_PARSE_QUOTED               = -2
NCLI_PARSE_UNSUPPORTED          = -3
NCLI_PARSE_UNKNOWN_FLAG         = -4
NCLI_PARSE_MISSING_VALUE        = -5
NCLI_PARSE_NO_INPUT             = -6
NCLI_PARSE_HUNK_REQUIRED        = -7
NCLI_PARSE_MIXED_INPUT          = -8
NCLI_PARSE_MULTIPLE_POSITIONAL  = -9
NCLI_PARSE_MODULE_PATH_CAPACITY = -10

NATIVE_OUTPUT_FORMAT_NONE       = 0
NATIVE_OUTPUT_FORMAT_BIN        = 1
NATIVE_OUTPUT_FORMAT_HUNK       = 2

; Parse the native CLI argument tail into fixed buffers and request flags.
opforgeNativeCliParseArgs
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a0, a3  ; A3 walks the AmigaDOS argument tail in-place
	clr.w NativeCliInputStyle
	clr.w NativeCliHunkRequested
	clr.w NativeCliBinRequested
	clr.w NativeCliOutputFormat
	clr.w NativeCliParseStatus
	clr.b NativeCliInputPath
	clr.b NativeCliHunkPath
	clr.b NativeCliBinPath
	clr.b NativeCliOutfileBase
	clr.b NativeCliCpuName
	clr.b NativeCliPackagePath
	move.w #1, NativeCliModulePathCount

opforgeNativeCliParseLoop
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.w opforgeNativeCliParseDone
	cmpi.b #'"', (a3)
	beq.w opforgeNativeCliQuoted
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyToken
	tst.l d0
	bne.w opforgeNativeCliUsage

	lea NativeCliArgToken, a0
	lea FlagHelpLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseHelp
	lea NativeCliArgToken, a0
	lea FlagHelpShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseHelp
	lea NativeCliArgToken, a0
	lea FlagVersionLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseVersion
	lea NativeCliArgToken, a0
	lea FlagVersionShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseVersion
	lea NativeCliArgToken, a0
	lea FlagInfileShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliInfile
	lea NativeCliArgToken, a0
	lea FlagInfileLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliInfile
	lea NativeCliArgToken, a0
	lea FlagHunkLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliHunk
	lea NativeCliArgToken, a0
	lea FlagBinShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliBin
	lea NativeCliArgToken, a0
	lea FlagBinLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliBin
	lea NativeCliArgToken, a0
	lea FlagOutfileShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliOutfile
	lea NativeCliArgToken, a0
	lea FlagOutfileLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliOutfile
	lea NativeCliArgToken, a0
	lea FlagCpuLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliCpu
	lea NativeCliArgToken, a0
	lea FlagPackageLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliPackage
	lea NativeCliArgToken, a0
	lea FlagModuleShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliModulePath
	lea NativeCliArgToken, a0
	lea FlagModuleLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliModulePath
	bsr.w opforgeNativeCliIsUnsupportedFlag
	tst.l d0
	bne.w opforgeNativeCliUnsupported
	lea NativeCliArgToken, a0
	cmpi.b #'-', (a0)
	beq.w opforgeNativeCliUnknownFlag
	bra.w opforgeNativeCliPositionalInput

opforgeNativeCliInfile
	tst.w NativeCliInputStyle
	beq.s opforgeNativeCliInfileFirst
	cmpi.w #1, NativeCliInputStyle
	beq.w opforgeNativeCliMixedInput
	bra.w opforgeNativeCliUsage

opforgeNativeCliInfileFirst
	move.w #2, NativeCliInputStyle
	lea NativeCliInputPath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliHunk
	move.w #1, NativeCliHunkRequested
	move.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	lea NativeCliHunkPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	tst.l d0
	bmi.w opforgeNativeCliQuoted
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliBin
	move.w #1, NativeCliBinRequested
	move.w #NATIVE_OUTPUT_FORMAT_BIN, NativeCliOutputFormat
	lea NativeCliBinPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	tst.l d0
	bmi.w opforgeNativeCliQuoted
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliOutfile
	lea NativeCliOutfileBase, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliCpu
	lea NativeCliCpuName, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliPackage
	lea NativeCliPackagePath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliModulePath
	lea NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyRequiredPathValue
	cmpi.l #1, d0
	beq.w opforgeNativeCliMissingValue
	tst.l d0
	bne.w opforgeNativeCliModulePathCapacity
	bsr.w opforgeNativeCliRecordModulePathValue
	tst.l d0
	bne.w opforgeNativeCliModulePathCapacity
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliPositionalInput
	tst.w NativeCliInputStyle
	beq.s opforgeNativeCliPositionalInputFirst
	cmpi.w #2, NativeCliInputStyle
	beq.w opforgeNativeCliMixedInput
	bra.w opforgeNativeCliMultiplePositional

opforgeNativeCliPositionalInputFirst
	move.w #1, NativeCliInputStyle
	lea NativeCliArgToken, a0
	lea NativeCliInputPath, a1
	bsr.w opforgeNativeCliCopyTokenBuffer
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliParseDone
	tst.w NativeCliInputStyle
	beq.w opforgeNativeCliNoInput
	tst.w NativeCliOutputFormat
	beq.w opforgeNativeCliHunkRequired
	cmpi.w #NATIVE_OUTPUT_FORMAT_BIN, NativeCliOutputFormat
	beq.s opforgeNativeCliDefaultBinPath
	cmpi.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	beq.s opforgeNativeCliDefaultHunkPath
	bra.w opforgeNativeCliUsage

opforgeNativeCliDefaultBinPath
	tst.b NativeCliBinPath
	bne.s opforgeNativeCliParseOk
	tst.b NativeCliOutfileBase
	beq.s opforgeNativeCliParseOk
	lea NativeCliOutfileBase, a0
	lea NativeCliBinPath, a1
	bsr.w opforgeNativeCliCopyTokenBuffer
	bra.s opforgeNativeCliParseOk

opforgeNativeCliDefaultHunkPath
	tst.b NativeCliHunkPath
	bne.s opforgeNativeCliParseOk
	tst.b NativeCliOutfileBase
	beq.s opforgeNativeCliParseOk
	lea NativeCliOutfileBase, a0
	lea NativeCliHunkPath, a1
	bsr.w opforgeNativeCliCopyTokenBuffer

opforgeNativeCliParseOk
	bsr.w opforgeNativeCliRecordImplicitModulePathRoot
	tst.l d0
	bne.w opforgeNativeCliModulePathCapacity
	move.w #NCLI_PARSE_OK, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliParseHelp
	move.w #NCLI_PARSE_HELP, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliParseVersion
	move.w #NCLI_PARSE_VERSION, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliUsage
	move.w #NCLI_PARSE_USAGE, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliQuoted
	move.w #NCLI_PARSE_QUOTED, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliUnsupported
	move.w #NCLI_PARSE_UNSUPPORTED, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliUnknownFlag
	move.w #NCLI_PARSE_UNKNOWN_FLAG, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliMissingValue
	move.w #NCLI_PARSE_MISSING_VALUE, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliNoInput
	move.w #NCLI_PARSE_NO_INPUT, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliHunkRequired
	move.w #NCLI_PARSE_HUNK_REQUIRED, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliMixedInput
	move.w #NCLI_PARSE_MIXED_INPUT, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliMultiplePositional
	move.w #NCLI_PARSE_MULTIPLE_POSITIONAL, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliModulePathCapacity
	move.w #NCLI_PARSE_MODULE_PATH_CAPACITY, NativeCliParseStatus

opforgeNativeCliParseReturn
	move.w NativeCliParseStatus, d0
	ext.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts

opforgeNativeCliSkipWhitespace
	cmpi.b #' ', (a3)
	beq.s opforgeNativeCliSkipOne
	cmpi.b #9, (a3)
	beq.s opforgeNativeCliSkipOne
	cmpi.b #10, (a3)
	beq.s opforgeNativeCliSkipOne
	cmpi.b #13, (a3)
	bne.s opforgeNativeCliSkipDone

opforgeNativeCliSkipOne
	addq.l #1, a3
	bra.s opforgeNativeCliSkipWhitespace

opforgeNativeCliSkipDone
	rts

opforgeNativeCliCopyToken
	move.l #TOKEN_BUFFER_CAPACITY - 1, d6

opforgeNativeCliCopyTokenLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #' ', d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #9, d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #10, d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #13, d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #'"', d0
	beq.s opforgeNativeCliCopyTokenFail
	tst.l d6
	beq.s opforgeNativeCliCopyTokenFail
	move.b d0, (a1)+
	addq.l #1, a3
	subq.l #1, d6
	bra.s opforgeNativeCliCopyTokenLoop

opforgeNativeCliCopyTokenDone
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliCopyTokenFail
	moveq #1, d0
	rts

opforgeNativeCliCopyRequiredValue
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s opforgeNativeCliRequiredMissing
	cmpi.b #'"', (a3)
	beq.s opforgeNativeCliRequiredMissing
	bsr.w opforgeNativeCliCopyToken
	rts

opforgeNativeCliRequiredMissing
	moveq #1, d0
	rts

opforgeNativeCliCopyRequiredPathValue
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s opforgeNativeCliRequiredPathMissing
	cmpi.b #'"', (a3)
	beq.s opforgeNativeCliRequiredPathMissing
	move.l #PATH_BUFFER_CAPACITY - 1, d6

opforgeNativeCliCopyRequiredPathLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #' ', d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #9, d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #10, d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #13, d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #'"', d0
	beq.s opforgeNativeCliCopyRequiredPathCapacity
	tst.l d6
	beq.s opforgeNativeCliCopyRequiredPathCapacity
	move.b d0, (a1)+
	addq.l #1, a3
	subq.l #1, d6
	bra.s opforgeNativeCliCopyRequiredPathLoop

opforgeNativeCliCopyRequiredPathDone
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliRequiredPathMissing
	moveq #1, d0
	rts

opforgeNativeCliCopyRequiredPathCapacity
	moveq #2, d0
	rts

opforgeNativeCliCopyOptionalValue
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s opforgeNativeCliOptionalNone
	cmpi.b #'"', (a3)
	beq.s opforgeNativeCliOptionalQuoted
	cmpi.b #'-', (a3)
	beq.s opforgeNativeCliOptionalNone
	bsr.w opforgeNativeCliCopyToken
	rts

opforgeNativeCliOptionalNone
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliOptionalQuoted
	moveq #-1, d0
	rts

opforgeNativeCliIsUnsupportedFlag
	lea NativeCliArgToken, a0
	lea FlagListShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagListLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagHexShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagHexLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagSrecShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagSrecLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagDefineShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagDefineLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagIncludeShort, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagIncludeLong, a1
	jsr opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	moveq #0, d0
	rts

opforgeNativeCliUnsupportedYes
	moveq #1, d0
	rts

; Print the deterministic diagnostic for the current argument-parse status.
opforgeNativeCliReportParseError
	move.w NativeCliParseStatus, d0
	cmpi.w #NCLI_PARSE_QUOTED, d0
	beq.s opforgeNativeCliReportQuoted
	cmpi.w #NCLI_PARSE_UNSUPPORTED, d0
	beq.s opforgeNativeCliReportUnsupported
	cmpi.w #NCLI_PARSE_UNKNOWN_FLAG, d0
	beq.s opforgeNativeCliReportUnknown
	cmpi.w #NCLI_PARSE_MISSING_VALUE, d0
	beq.s opforgeNativeCliReportMissing
	cmpi.w #NCLI_PARSE_NO_INPUT, d0
	beq.w opforgeNativeCliReportNoInput
	cmpi.w #NCLI_PARSE_HUNK_REQUIRED, d0
	beq.w opforgeNativeCliReportHunkRequired
	cmpi.w #NCLI_PARSE_MIXED_INPUT, d0
	beq.w opforgeNativeCliReportMixedInput
	cmpi.w #NCLI_PARSE_MULTIPLE_POSITIONAL, d0
	beq.w opforgeNativeCliReportMultiplePositional
	cmpi.w #NCLI_PARSE_MODULE_PATH_CAPACITY, d0
	beq.w opforgeNativeCliReportModulePathCapacity
	move.l #UsageText, d1
	bra.w opforgeNativeCliReportText

opforgeNativeCliReportQuoted
	move.l #QuotedText, d1
	bra.w opforgeNativeCliReportText

opforgeNativeCliReportUnsupported
	move.l #UnsupportedText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeSubsetHelpText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportUnknown
	move.l #UnknownFlagText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMissing
	move.l #MissingValueText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportNoInput
	move.l #NoInputText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportHunkRequired
	move.l #HunkRequiredText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMixedInput
	move.l #MixedInputText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMultiplePositional
	move.l #MultiplePositionalText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportModulePathCapacity
	move.l #ModulePathCapacityText, d1

opforgeNativeCliReportText
	jsr opforgeNativeCliPutStr
	rts

opforgeNativeCliRecordImplicitModulePathRoot
	lea NativeCliInputPath, a0
	lea NativeCliModulePathTable, a1
	jsr opforgeNativeCliCopyPathRoot
	rts

opforgeNativeCliRecordModulePathValue
	movem.l d1/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliModulePathCount, d0
	cmpi.w #NATIVE_MODULE_PATH_CAPACITY, d0
	bhs.s opforgeNativeCliRecordModulePathFail
	move.l d0, d1
	lsl.l #8, d1
	lea NativeCliModulePathTable, a1
	adda.l d1, a1
	lea NativeCliIncludeTarget, a0
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliRecordModulePathFail
	move.w NativeCliModulePathCount, d0
	addq.w #1, d0
	move.w d0, NativeCliModulePathCount
	moveq #0, d0
	bra.s opforgeNativeCliRecordModulePathReturn

opforgeNativeCliRecordModulePathFail
	moveq #1, d0

opforgeNativeCliRecordModulePathReturn
	movem.l (sp)+, d1/a0-a1
	rts

opforgeNativeCliEmitModulePathRecords
	movem.l d0-d4/a0, -(sp)
	clr.w d4

opforgeNativeCliEmitModulePathLoop
	move.w NativeCliModulePathCount, d0
	cmp.w d0, d4
	bhs.s opforgeNativeCliEmitModulePathDone
	move.l #ModPathText, d1
	jsr opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d4, d0
	lsl.l #8, d0
	lea NativeCliModulePathTable, a0
	adda.l d0, a0
	move.l a0, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	addq.w #1, d4
	bra.s opforgeNativeCliEmitModulePathLoop

opforgeNativeCliEmitModulePathDone
	movem.l (sp)+, d0-d4/a0
	rts
	.bend  ; start
	.priv

	.endsection

	.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
	.endmodule
