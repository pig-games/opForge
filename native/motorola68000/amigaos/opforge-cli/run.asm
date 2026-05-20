; Native AmigaOS opForge CLI run orchestration.

	.module opforge.cli.run
	.cpu 68020

	.use opasm.amigaos.engine (opasmEngineGetImageByteCountV1)

	.use opforge.cli.constants (SYS_BASE, OPEN_LIBRARY, CLOSE_LIBRARY, GET_ARG_STR)
	.use opforge.cli.constants (RETURN_OK, RETURN_USAGE, RETURN_FILE_FAILURE)
	.use opforge.cli.constants (RETURN_RUNTIME_FAILURE, RETURN_NOT_IMPLEMENTED)
	.use opforge.cli.constants (NCLI_PARSE_HELP, NCLI_PARSE_VERSION)
	.use opforge.cli.constants (NATIVE_OUTPUT_FORMAT_HUNK)
	.use opforge.cli.state (NativeCliReturnCode, NativeCliDosBase)
	.use opforge.cli.state (NativeCliInputPath, NativeCliOutputFormat, NativeCliBinPath)
.ifdef OPFORGE_FS_UAE_SMOKE
	.use opforge.cli.strings (DosName, defaultFsUaeArgTail, HelpText, VersionText)
.else
	.use opforge.cli.strings (DosName, HelpText, VersionText)
.endif
	.use opforge.cli.strings (InputOpenErrorText, NewlineText, NativeHunkNotImplementedText)
	.use opforge.cli.strings (StubHeaderText, InputLabelText, BinLabelText)
	.use opforge.cli.strings (TokenizerFailureText, ParserOkText, NativePassFailureText)
	.use opforge.cli.strings (NativeOutputFailureText, NativeOutputOkText, EmitterStubText)
	.use opforge.cli.dos (opforgeNativeCliPutStr, opforgeNativeCliOpenInput, opforgeNativeCliClose)
	.use opforge.cli.source_reader (opforgeNativeCliTokenizeFrontend)
	.use opforge.cli.report (opforgeNativeCliReportParseError, opforgeNativeCliEmitAssemblySessionSummary)
	.use opforge.cli.args (opforgeNativeCliParseArgs, opforgeNativeCliEmitModulePathRecords)
	.use opforge.cli.session_init (opforgeNativeCliInitModuleUseState, opforgeNativeCliInitAssemblySession)
	.use opforge.cli.output (opforgeNativeCliWriteFlatOutput)
	.use opforge.cli.engine_callbacks (opforgeNativeCliRunTwoPassEngine)

	.section code, kind=code
	.pub

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
opforgeNativeCliRun	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #RETURN_USAGE, NativeCliReturnCode

	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.W, a6  ; first try the AmigaOS 2.x+ dos.library version expected by tests
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	bne.s haveDos

	lea DosName, a1
	moveq #0, d0
	movea.l SYS_BASE.W, a6  ; fallback keeps older emulator images usable for smoke runs
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done

haveDos
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
	beq.w help
	cmpi.w #NCLI_PARSE_VERSION, d0
	beq.w version
	tst.w d0
	beq.w parsed

	jsr opforgeNativeCliReportParseError
	move.l #RETURN_USAGE, NativeCliReturnCode
	bra.w closeDos

help
	move.l #HelpText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w closeDos

version
	move.l #VersionText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w closeDos

parsed
	lea NativeCliInputPath, a0
	jsr opforgeNativeCliOpenInput
	tst.l d0
	bne.s inputOpened
	move.l #InputOpenErrorText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliInputPath, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_FILE_FAILURE, NativeCliReturnCode
	bra.w closeDos

inputOpened
	move.l d0, d1
	jsr opforgeNativeCliClose
	cmpi.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	bne.s outputFormatReady
	move.l #NativeHunkNotImplementedText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_NOT_IMPLEMENTED, NativeCliReturnCode
	bra.w closeDos

outputFormatReady
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
	jsr opforgeNativeCliTokenizeFrontend
	tst.l d0
	beq.s tokenizerOk
	move.l #TokenizerFailureText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_RUNTIME_FAILURE, NativeCliReturnCode
	bra.w closeDos

tokenizerOk
	move.l #ParserOkText, d1
	jsr opforgeNativeCliPutStr
	jsr opforgeNativeCliRunTwoPassEngine
	tst.l d0
	beq.s passesOk
	move.l #NativePassFailureText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_RUNTIME_FAILURE, NativeCliReturnCode
	bra.w closeDos

passesOk
	jsr opforgeNativeCliEmitAssemblySessionSummary
	jsr opasmEngineGetImageByteCountV1
	tst.l d0
	beq.s emitStub
	jsr opforgeNativeCliWriteFlatOutput
	tst.l d0
	beq.s outputOk
	move.l #NativeOutputFailureText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_FILE_FAILURE, NativeCliReturnCode
	bra.w closeDos

outputOk
	move.l #NativeOutputOkText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w closeDos

emitStub
	move.l #EmitterStubText, d1
	jsr opforgeNativeCliPutStr
	move.l #RETURN_NOT_IMPLEMENTED, NativeCliReturnCode

closeDos
	move.l NativeCliDosBase, d0
	beq.s done
	movea.l SYS_BASE.W, a6
	movea.l d0, a1
	jsr CLOSE_LIBRARY(a6)
	clr.l NativeCliDosBase

done
	move.l NativeCliReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opforgeNativeCliRun

	.endsection
	.endmodule
