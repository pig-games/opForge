; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.source_reader
	.cpu 68020

	.use opasm.amigaos.engine
	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.path
	.use tkpkg.amigaos.buffers

	.use tkpkg.amigaos.abi

	.use opforge.cli.package_pipeline
	.use opforge.cli.line_processor
	.use opforge.cli.include_use
	.use opforge.cli.directive_handlers
	.use opforge.cli.module_use
	.use opforge.cli.line_text
	.use opforge.cli.preprocessor
	.use opforge.cli.preprocessor_definitions
	.use opforge.cli.token_util
	.use opforge.cli.tkpkg_control_block

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Initialize package state, tokenize every source line, and run parser routing.
;
; Inputs:
; - none; uses parsed CLI state and shared tkpkg buffers.
;
; Outputs:
; - D0: 0 on success, nonzero on package/tokenization failure.
;
; Clobbers:
; - D0-D7/A0-A6/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
opforgeNativeCliTokenizeFrontend	.block
	movem.l d2-d7/a2-a6, -(sp)
	tst.b state.NativeCliCpuName
	bne.w bootstrapDone
	bsr.w opforgeNativeCliBootstrapSourceCpuNameFromInput
	bne.w bootstrapMiss
	bra.w bootstrapDone

bootstrapMiss

bootstrapDone
	clr.w state.NativeCliPackagePipelineReady
	bsr.w package_pipeline.opforgeNativeCliInitPackagePipeline
	beq.s packageReady
	cmpi.l #2, d0
	beq.s packageUnavailable
	bra.s return

packageReady
	move.w #1, state.NativeCliPackagePipelineReady
	lea buffers.ActiveCpuBuffer, a0
	jsr engine.setSessionCpuNameV1
	tst.w state.NativeCliDebugEnabled
	beq.s packageUnavailable
	move.l #strings.TokenizerOkText, d1
	jsr dos.putStr
packageUnavailable
	move.w #-1, state.NativeCliResolvedModuleId
	bsr.w opforgeNativeCliTokenizeFile
	bne.s return
	jsr preprocessor.opforgeNativeCliFinishConditionalsV1
	beq.s success
	move.l #strings.ConditionalFailureText, d1
	jsr dos.putErrStr
	moveq #1, d0
	bra.s return

success
	moveq #0, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opforgeNativeCliTokenizeFrontend

; Bootstrap one source-driven `.output` selection before the full tokenize pass.
; Inputs: state.NativeCliInputPath holds the primary source path.
; Outputs: D0 = 0 when a `.output` line was found and applied; 1 otherwise.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliBootstrapSourceOutputFromInput	.block
	lea state.NativeCliInputPath, a0
	jsr dos.openInput
	tst.l d0
	bne.s openOk
	moveq #1, d0
	rts

openOk
	move.l d0, d5
	clr.w state.NativeCliSourceLineLen
	clr.w state.NativeCliSawCr

loop
	lea state.NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr dos.readInput
	cmp.l #-1, d0
	beq.w closeFail
	tst.l d0
	beq.w fileEof

	move.b state.NativeCliInputChar, d0
	tst.w state.NativeCliSawCr
	beq.s checkBreak
	clr.w state.NativeCliSawCr
	cmpi.b #10, d0
	beq.w loop

checkBreak
	cmpi.b #10, d0
	beq.s lineDone
	cmpi.b #13, d0
	beq.s crDone

	move.w state.NativeCliSourceLineLen, d1
	cmpi.w #constants.SOURCE_LINE_BUFFER_CAPACITY, d1
	bhs.w closeFail
	lea state.NativeCliSourceLine, a1
	move.b d0, 0(a1, d1.W)
	addq.w #1, d1
	move.w d1, state.NativeCliSourceLineLen
	bra.w loop

crDone
	move.w #1, state.NativeCliSawCr
	bra.w lineDone

lineDone
	move.l d5, -(sp)
	bsr.w opforgeNativeCliBootstrapCurrentOutputLine
	move.l (sp)+, d5
	beq.s closeOk
	clr.w state.NativeCliSourceLineLen
	bra.w loop

fileEof
	tst.w state.NativeCliSourceLineLen
	beq.w closeFail
	move.l d5, -(sp)
	bsr.w opforgeNativeCliBootstrapCurrentOutputLine
	move.l (sp)+, d5
	bne.w closeFail

closeOk
	move.l d5, d1
	jsr dos.close
	move.w #1, state.NativeCliOutputBootstrapFromSource
	moveq #0, d0
	rts

closeFail
	move.l d5, d1
	jsr dos.close
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliBootstrapSourceOutputFromInput

	.priv

; Tokenize the primary input file path recorded by argument parsing.
; Inputs: state.NativeCliInputPath holds the requested source file path.
; Outputs: D0 = 0 on success; state.NativeCliCurrentPath updated for the active file.
; Clobbers: A0-A1/D0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliTokenizeFile	.block
	lea state.NativeCliInputPath, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	lea state.NativeCliInputPath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeFile

; Read and tokenize one AmigaDOS text file at A0, preserving logical line state.
; Inputs: A0 = path buffer for the source file to read.
; Outputs: D0 = 0 on success; source-line state and pending include/use work drained.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliTokenizeFileAtPath	.block
	jsr dos.openInput
	tst.l d0
	bne.s openOk
	moveq #1, d0
	rts

openOk
	move.l d0, d5
	tst.w state.NativeCliModuleResolveDepth
	beq.s initializeLineState
	moveq #0, d3
	move.w state.NativeCliModuleResolveDepth, d3
	subq.l #1, d3
	lsl.l #2, d3
	move.l state.NativeCliResolvedModuleEndOffset.l, d2
	sub.l state.NativeCliResolvedModuleStartOffset.l, d2
	bmi.w close
	lea state.NativeCliModuleReadRemaining.l, a1
	move.l d2, 0(a1, d3.l)
	move.l state.NativeCliResolvedModuleStartOffset.l, d4

skipResolvedPrefix
	tst.l d4
	beq.s initializeLineState
	lea state.NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr dos.readInput
	cmpi.l #1, d0
	bne.w close
	subq.l #1, d4
	bra.s skipResolvedPrefix

initializeLineState
	move.l #1, state.NativeCliSourceLineNum
	clr.w state.NativeCliSourceLineLen
	clr.w state.NativeCliSawCr

loop
	tst.w state.NativeCliModuleResolveDepth
	beq.s readNextChar
	moveq #0, d3
	move.w state.NativeCliModuleResolveDepth, d3
	subq.l #1, d3
	lsl.l #2, d3
	lea state.NativeCliModuleReadRemaining.l, a1
	tst.l 0(a1, d3.l)
	beq.w fileEof

readNextChar
	lea state.NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr dos.readInput
	cmp.l #-1, d0
	beq.w close
	tst.l d0
	beq.w fileEof
	tst.w state.NativeCliModuleResolveDepth
	beq.s haveInputChar
	moveq #0, d3
	move.w state.NativeCliModuleResolveDepth, d3
	subq.l #1, d3
	lsl.l #2, d3
	lea state.NativeCliModuleReadRemaining.l, a1
	subq.l #1, 0(a1, d3.l)

haveInputChar
	move.b state.NativeCliInputChar, d0
	tst.w state.NativeCliSawCr
	beq.s checkBreak
	clr.w state.NativeCliSawCr
	cmpi.b #10, d0
	beq.w loop

checkBreak
	cmpi.b #10, d0
	beq.s lineDone
	cmpi.b #13, d0
	beq.s crDone

	move.w state.NativeCliSourceLineLen, d1
	cmpi.w #constants.SOURCE_LINE_BUFFER_CAPACITY, d1
	bhs.w close
	lea state.NativeCliSourceLine, a1
	move.b d0, 0(a1, d1.W)
	addq.w #1, d1
	move.w d1, state.NativeCliSourceLineLen
	bra.w loop

crDone
	move.w #1, state.NativeCliSawCr

lineDone
	; The line processor and pending include/module drainers use D5 as scratch.
	; Keep this file's DOS handle intact so the next read continues the same
	; source stream rather than issuing Read against a clobbered handle.
	move.l d5, -(sp)
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.s lineDoneRestoreClose
	bsr.w opforgeNativeCliTokenizePendingInclude
	bne.s lineDoneRestoreClose
	bsr.w opforgeNativeCliTokenizePendingUseModule
	bne.s lineDoneRestoreClose
	move.l (sp)+, d5
	move.l state.NativeCliSourceLineNum, d0
	addq.l #1, d0
	move.l d0, state.NativeCliSourceLineNum
	clr.w state.NativeCliSourceLineLen
	bra.w loop

lineDoneRestoreClose
	move.l (sp)+, d5
	bra.w close

fileEof
	tst.w state.NativeCliSourceLineLen
	beq.s checkModuleDepth
	; See lineDone: the active source-file handle must survive every per-line
	; callback before the EOF close path consumes it.
	move.l d5, -(sp)
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.s fileEofRestoreClose
	bsr.w opforgeNativeCliTokenizePendingInclude
	bne.s fileEofRestoreClose
	bsr.w opforgeNativeCliTokenizePendingUseModule
	bne.s fileEofRestoreClose
	move.l (sp)+, d5
	bra.s checkModuleDepth

fileEofRestoreClose
	move.l (sp)+, d5
	bra.s close

checkModuleDepth
	jsr preprocessor_definitions.opforgeNativeCliFinishMacroDefinitionsV1
	bne.s close
	tst.w state.NativeCliIncludeDepth
	bne.s successClose
	tst.w state.NativeCliModuleResolveDepth
	bne.s successClose
	tst.w state.NativeCliModuleDepth
	beq.s successClose
	move.l #strings.ModuleDepthFailureText, d1
	jsr dos.putErrStr
	bra.s close

successClose
	move.l d5, d1
	jsr dos.close
	moveq #0, d0
	rts

close
	move.l d5, d1
	jsr dos.close
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeFileAtPath

; Drain one pending include request, tokenizing the staged file when present.
; Inputs: include pending state and saved-path state in opforge.cli.state.
; Outputs: D0 = 0 on success; include file tokenized when D1 from prepare step was nonzero.
; Clobbers: A0/D0-D1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliTokenizePendingInclude	.block
	movem.l d5, -(sp)
	bsr.w include_use.opforgeNativeCliPreparePendingInclude
	bne.s return
	tst.l d1
	beq.s return
	lea state.NativeCliIncludePath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	bsr.w include_use.opforgeNativeCliFinishPendingInclude

return
	movem.l (sp)+, d5
	rts
	.bend  ; opforgeNativeCliTokenizePendingInclude

; Drain one pending `.use` module import when parser routing resolved a module id.
; Inputs: state.NativeCliResolvedModuleId and saved module-path state.
; Outputs: D0 = 0 on success; resolved module reset and tokenized when one was pending.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliTokenizePendingUseModule	.block
	movem.l d5-d6, -(sp)
	cmpi.w #-1, state.NativeCliResolvedModuleId
	beq.s ok
	move.w state.NativeCliImportCount, d6
	subq.w #1, d6
	move.w state.NativeCliResolvedModuleId, d0
	cmp.w state.NativeCliModuleCount, d0
	blo.s loaded
	move.w #-1, state.NativeCliResolvedModuleId
	move.l d6, -(sp)
	bsr.w opforgeNativeCliTokenizeResolvedUseModule
	move.l d0, d5
	move.l (sp)+, d6
	tst.l d5
	bne.s return
	jsr module_use.opforgeNativeCliBindImportDefinitionsV1
	bra.s return

loaded
	move.w #-1, state.NativeCliResolvedModuleId
	jsr module_use.opforgeNativeCliBindImportDefinitionsV1
	bra.s return

ok
	moveq #0, d0

return
	movem.l (sp)+, d5-d6
	rts
	.bend  ; opforgeNativeCliTokenizePendingUseModule

; Bootstrap one source-driven `.cpu` selection before the full tokenize pass.
; Inputs: state.NativeCliInputPath holds the primary source path.
; Outputs: D0 = 0 when a `.cpu` line was found and applied; 1 otherwise.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliBootstrapSourceCpuNameFromInput	.block
	lea state.NativeCliInputPath, a0
	jsr dos.openInput
	tst.l d0
	bne.s openOk
	moveq #1, d0
	rts

openOk
	move.l d0, d5
	clr.w state.NativeCliSourceLineLen
	clr.w state.NativeCliSawCr

loop
	lea state.NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr dos.readInput
	cmp.l #-1, d0
	beq.w closeFail
	tst.l d0
	beq.w fileEof

	move.b state.NativeCliInputChar, d0
	tst.w state.NativeCliSawCr
	beq.s checkBreak
	clr.w state.NativeCliSawCr
	cmpi.b #10, d0
	beq.w loop

checkBreak
	cmpi.b #10, d0
	beq.s lineDone
	cmpi.b #13, d0
	beq.s crDone

	move.w state.NativeCliSourceLineLen, d1
	cmpi.w #constants.SOURCE_LINE_BUFFER_CAPACITY, d1
	bhs.w closeFail
	lea state.NativeCliSourceLine, a1
	move.b d0, 0(a1, d1.W)
	addq.w #1, d1
	move.w d1, state.NativeCliSourceLineLen
	bra.w loop

crDone
	move.w #1, state.NativeCliSawCr
	bra.w lineDone

lineDone
	move.l d5, -(sp)
	bsr.w opforgeNativeCliBootstrapCurrentCpuNameLine
	move.l (sp)+, d5
	beq.s closeOk
	clr.w state.NativeCliSourceLineLen
	bra.w loop

fileEof
	tst.w state.NativeCliSourceLineLen
	beq.w closeFail
	move.l d5, -(sp)
	bsr.w opforgeNativeCliBootstrapCurrentCpuNameLine
	move.l (sp)+, d5
	bne.w closeFail

closeOk
	move.l d5, d1
	jsr dos.close
	moveq #0, d0
	rts

closeFail
	move.l d5, d1
	jsr dos.close
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliBootstrapSourceCpuNameFromInput

; Try to parse the current source line as `.cpu`.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen hold one logical line.
; Outputs: D0 = 0 when `.cpu` matched and updated state.NativeCliCpuName; 1 otherwise.
; Clobbers: D0-D1/A0-A1/CCR.
opforgeNativeCliBootstrapCurrentCpuNameLine	.block
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s noMatch
	move.l d0, -(sp)
	move.l a0, -(sp)
	lea strings.CpuMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s noMatchRestore
	movea.l (sp)+, a0
	move.l (sp)+, d0
	addq.l #4, a0
	subq.l #4, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliArgToken, a1
	move.l d0, -(sp)
	bsr.w line_text.opforgeNativeCliCopyLineWord
	bne.s copyRestoreFail
	move.l (sp)+, d0
	sub.l d5, d0
	tst.b state.NativeCliArgToken
	beq.s fail
	move.l d0, -(sp)
	move.l a0, -(sp)
	jsr directive_handlers.opforgeNativeCliNormalizeQuotedCpuToken
	tst.l d0
	bne.s normalizeRestoreFail
	lea state.NativeCliArgToken, a0
	lea state.NativeCliCpuName, a1
	jsr token_util.opforgeNativeCliCanonicalizeCpuName
	movea.l (sp)+, a0
	move.l (sp)+, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.s ok
	cmpi.b #';', (a0)
	bne.s fail

ok
	moveq #0, d0
	rts

noMatchRestore
	addq.l #8, sp

noMatch
	moveq #1, d0
	rts

normalizeRestoreFail
	addq.l #8, sp
	bra.s fail

copyRestoreFail
	addq.l #4, sp

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliBootstrapCurrentCpuNameLine

; Try to parse the current source line as `.output`.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen hold one logical line.
; Outputs: D0 = 0 when `.output` matched and updated output state; 1 otherwise.
; Clobbers: D0-D1/A0-A1/CCR.
opforgeNativeCliBootstrapCurrentOutputLine	.block
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s noMatch
	lea strings.OutputDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s noMatch
	jsr directive_handlers.opforgeNativeCliParseOutputLine
	tst.l d0
	bne.s noMatch
	moveq #0, d0
	rts

noMatch
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliBootstrapCurrentOutputLine

; Tokenize the file for the currently resolved `.use` module and restore caller state.
; Inputs: state.NativeCliIncludePath and module-saved path/line state.
; Outputs: D0 = 0 on success; current path/line state restored after module tokenization.
; Clobbers: D0-D2/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliTokenizeResolvedUseModule	.block
	movem.l d1-d2/a0-a1, -(sp)
	moveq #0, d2
	move.w state.NativeCliModuleResolveDepth, d2
	cmpi.w #constants.NATIVE_MODULE_RESOLVE_DEPTH_LIMIT, d2
	bhs.w fail
	move.l d2, d1
	add.l d1, d1
	lea state.NativeCliModuleSavedLineLen, a1
	move.w state.NativeCliSourceLineLen, d0
	move.w d0, 0(a1, d1.l)
	lea state.NativeCliModuleSavedSawCr, a1
	move.w state.NativeCliSawCr, d0
	move.w d0, 0(a1, d1.l)
	move.l d2, d1
	lsl.l #2, d1
	lea state.NativeCliModuleSavedLineNum, a1
	move.l state.NativeCliSourceLineNum, d0
	move.l d0, 0(a1, d1.l)
	lea state.NativeCliCurrentPath, a0
	lea state.NativeCliModuleSavedPath, a1
	move.l d2, d1
	lsl.l #8, d1
	adda.l d1, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w fail
	lea state.NativeCliIncludePath, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w fail
	lea state.NativeCliIncludePath, a0
	addq.w #1, state.NativeCliModuleResolveDepth
	bsr.w opforgeNativeCliTokenizeFileAtPath
	subq.w #1, state.NativeCliModuleResolveDepth
	tst.l d0
	bne.s restoreFail
	moveq #0, d1
	bra.s restore

restoreFail
	moveq #1, d1

restore
	moveq #0, d2
	move.w state.NativeCliModuleResolveDepth, d2
	move.l d2, d0
	add.l d0, d0
	lea state.NativeCliModuleSavedLineLen, a0
	move.w 0(a0, d0.l), d0
	move.w d0, state.NativeCliSourceLineLen
	move.l d2, d0
	add.l d0, d0
	lea state.NativeCliModuleSavedSawCr, a0
	move.w 0(a0, d0.l), d0
	move.w d0, state.NativeCliSawCr
	move.l d2, d0
	lsl.l #2, d0
	lea state.NativeCliModuleSavedLineNum, a0
	move.l 0(a0, d0.l), d0
	move.l d0, state.NativeCliSourceLineNum
	lea state.NativeCliModuleSavedPath, a0
	move.l d2, d0
	lsl.l #8, d0
	adda.l d0, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	tst.l d1
	bne.s fail
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; opforgeNativeCliTokenizeResolvedUseModule

	.endsection
	.endmodule
