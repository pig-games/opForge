; Native AmigaOS opForge CLI run orchestration.

	.module opforge.cli.run
	.cpu 68020

	.use opasm.amigaos.engine

	.use opforge.cli.constants
	.use opforge.cli.state
.ifdef OPFORGE_FS_UAE_SMOKE
	.use opforge.cli.strings
.else
	.use opforge.cli.strings
.endif
	.use opforge.cli.dos
	.use opforge.cli.source_reader
	.use opforge.cli.report
	.use opforge.cli.args
	.use opforge.cli.session_init
	.use opforge.cli.output
	.use opforge.cli.engine_callbacks
	.use opforge.cli.preprocessor
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

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
; - normal textual OPFORGE-NATIVE report is written to stdout.
; - deterministic failure diagnostics are written to ErrorOutput.
; - flat `.bin` output is written when selected and image bytes exist.
;
; Clobbers: D0-D1/A0-A1/CCR; D2-D7/A2-A6 are preserved.
; CCR: unspecified on return.
; ---------------------------------------------------------------------------
opforgeNativeCliRun	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #constants.RETURN_USAGE, state.NativeCliReturnCode

	lea strings.DosName, a1
	moveq #36, d0
	movea.l constants.SYS_BASE.W, a6  ; first try the AmigaOS 2.x+ dos.library version expected by tests
	jsr constants.OPEN_LIBRARY(a6)
	tst.l d0
	bne.s haveDos

	lea strings.DosName, a1
	moveq #0, d0
	movea.l constants.SYS_BASE.W, a6  ; fallback keeps older emulator images usable for smoke runs
	jsr constants.OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done

haveDos
	move.l d0, state.NativeCliDosBase  ; all file/console calls below dispatch through dos.library base
	jsr session_init.opforgeNativeCliInitModuleUseState
	movea.l d0, a6
	jsr constants.GET_ARG_STR(a6)
.ifdef OPFORGE_FS_UAE_SMOKE
	lea strings.defaultFsUaeArgTail, a0
.else
	movea.l d0, a0
.endif
	jsr args.opforgeNativeCliParseArgs

	cmpi.w #constants.NCLI_PARSE_HELP, d0
	beq.w help
	cmpi.w #constants.NCLI_PARSE_VERSION, d0
	beq.w version
	tst.w d0
	beq.w parsed

	jsr report.opforgeNativeCliReportParseError
	move.l #constants.RETURN_USAGE, state.NativeCliReturnCode
	bra.w closeDos

help
	move.l #strings.HelpText, d1
	jsr dos.putStr
	move.l #constants.RETURN_OK, state.NativeCliReturnCode
	bra.w closeDos

version
	move.l #strings.VersionText, d1
	jsr dos.putStr
	move.l #constants.RETURN_OK, state.NativeCliReturnCode
	bra.w closeDos

parsed
	lea state.NativeCliInputPath, a0
	jsr dos.openInput
	tst.l d0
	bne.s inputOpened
	move.l #strings.InputOpenErrorText, d1
	jsr dos.putErrStr
	move.l #state.NativeCliInputPath, d1
	jsr dos.putErrStr
	move.l #strings.NewlineText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_FILE_FAILURE, state.NativeCliReturnCode
	bra.w closeDos

inputOpened
	move.l d0, d1
	jsr dos.close
	tst.w state.NativeCliOutputFormat
	bne.s maybeHunkRequested
	jsr source_reader.opforgeNativeCliBootstrapSourceOutputFromInput

maybeHunkRequested
	tst.w state.NativeCliOutputFormat
	beq.s outputFormatReady
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, state.NativeCliOutputFormat
	bne.s outputFormatReady
	move.l #strings.NativeHunkNotImplementedText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_NOT_IMPLEMENTED, state.NativeCliReturnCode
	bra.w closeDos

outputFormatReady
	tst.w state.NativeCliOutputFormat
	bne.s headerReady
	move.l #strings.HunkRequiredText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_USAGE, state.NativeCliReturnCode
	bra.w closeDos

headerReady
	tst.w state.NativeCliDebugEnabled
	beq.w tokenizerStage
.ifdef OPFORGE_DEBUG_CONTRACTS
	; Instrumentation point: CLI debug header emission.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A6.
	; SR/CCR preserved: CCR preserved exactly; supervisor state untouched.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the debug-enabled branch has
	; already resolved, and CCR is restored before the next instruction.
	; Removal/stabilization plan: stable passive event replacing this one
	; free-form debug header; retain while the native CLI debug flag exists.
	move.w ccr, -(sp)
	movem.l d1-d6, -(sp)
	moveq #0, d1
	moveq #1, d2
	moveq #0, d3
	move.w state.NativeCliDebugEnabled, d3
	moveq #0, d4
	move.w state.NativeCliOutputFormat, d4
	move.l #state.NativeCliInputPath, d5
	move.l #state.NativeCliBinPath, d6
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_CLI_DEBUG_HEADER
	movem.l (sp)+, d1-d6
	move.w (sp)+, ccr
.else
	move.l #strings.StubHeaderText, d1
	jsr dos.putStr
.endif
	move.l #strings.InputLabelText, d1
	jsr dos.putStr
	move.l #state.NativeCliInputPath, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.BinLabelText, d1
	jsr dos.putStr
	move.l #state.NativeCliBinPath, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr

tokenizerStage
	jsr session_init.opforgeNativeCliInitAssemblySession
	tst.w state.NativeCliDebugEnabled
	beq.s tokenizeFrontend
	jsr args.opforgeNativeCliEmitModulePathRecords

tokenizeFrontend
	jsr source_reader.opforgeNativeCliTokenizeFrontend
	beq.s tokenizerOk
	move.l #strings.TokenizerFailureText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_RUNTIME_FAILURE, state.NativeCliReturnCode
	bra.w closeDos

tokenizerOk
	; Macro definitions and invocation frames are frontend-only. The engine must
	; begin from a clean preprocessor frame after all source expansion is done.
	jsr preprocessor.opforgeNativeCliResetPreprocessorV1
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, state.NativeCliOutputFormat
	bne.s outputRequestReady
	move.l #strings.NativeHunkNotImplementedText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_NOT_IMPLEMENTED, state.NativeCliReturnCode
	bra.w closeDos

outputRequestReady
	tst.w state.NativeCliDebugEnabled
	beq.s runEngine
	move.l #strings.ParserStageText, d1
	jsr dos.putStr
	move.l #strings.SessionStageText, d1
	jsr dos.putStr

runEngine
	jsr engine_callbacks.opforgeNativeCliRunTwoPassEngine
	tst.l d0
	beq.s passesOk
	move.l #strings.NativePassFailureText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_RUNTIME_FAILURE, state.NativeCliReturnCode
	bra.w closeDos

passesOk
	tst.w state.NativeCliDebugEnabled
	beq.s checkImage
	jsr report.opforgeNativeCliEmitAssemblySessionSummary

checkImage
	jsr engine.opasmEngineGetImageByteCountV1
	tst.l d0
	beq.s emitStub
	jsr output.opforgeNativeCliWriteFlatOutput
	beq.s outputOk
	move.l #strings.NativeOutputFailureText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_FILE_FAILURE, state.NativeCliReturnCode
	bra.w closeDos

outputOk
	tst.w state.NativeCliDebugEnabled
	beq.s outputOkReturn
	move.l #strings.NativeOutputOkText, d1
	jsr dos.putStr
outputOkReturn
	move.l #constants.RETURN_OK, state.NativeCliReturnCode
	bra.w closeDos

emitStub
	move.l #strings.EmitterStubText, d1
	jsr dos.putErrStr
	move.l #constants.RETURN_NOT_IMPLEMENTED, state.NativeCliReturnCode

closeDos
	move.l state.NativeCliDosBase, d0
	beq.s done
	movea.l constants.SYS_BASE.W, a6
	movea.l d0, a1
	jsr constants.CLOSE_LIBRARY(a6)
	clr.l state.NativeCliDosBase

done
	move.l state.NativeCliReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opforgeNativeCliRun

	.endsection
	.endmodule
