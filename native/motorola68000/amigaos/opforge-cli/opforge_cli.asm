; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module main
	.cpu 68020
	.use opasm.amigaos.engine (opasmEngineImageByteCount)

	.use opforge.cli.constants (*)
	.use opforge.cli.state (*)
	.use opforge.cli.strings (*)
	.use opforge.cli.dos (*)
	.use opforge.cli.source_reader (*)
	.use opforge.cli.report (*)
	.use opforge.cli.args (*)
	.use opforge.cli.session_init (*)
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
	jsr opforgeNativeCliInitModuleUseState
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
	jsr opforgeNativeCliInitAssemblySession
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

	.bend  ; start
	.priv

	.endsection

	.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
	.endmodule
