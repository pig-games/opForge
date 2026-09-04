; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.directive_handlers
	.cpu 68020

	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.line_text
	.use opforge.cli.module_use
	.use opforge.cli.path
	.use opforge.cli.package_pipeline
	.use opforge.cli.token_util
	.use opasm.amigaos.engine
	.use tkpkg.amigaos.buffers
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	.use debug.amigaos.platform_profile as platform_profile
.endif

	.section code, kind=code
	.pub

; Preserve one effective source artifact directive for post-assembly rendering.
; @opforge-owner: opforge.cli.output
; @opforge-slice: documentation/plans/slices/native-porting-slice-source-artifact-output.toml
; @opforge-role: delegation
; Inputs: D1.W = NATIVE_ARTIFACT_REQUEST_* kind; current source line is authoritative.
; Outputs: D0.L = 0 on success, 1 on invalid kind or request capacity exhaustion.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0.L on return.
opforgeNativeCliCaptureArtifactRequestLineV1	.block
	movem.l d1-d7/a0-a2, -(sp)
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_OUTPUT, d1
	blo.w fail
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_METADATA_BIN, d1
	bhi.w fail
	moveq #0, d7
	move.w state.NativeCliArtifactRequestCount, d7
	cmpi.w #constants.NATIVE_ARTIFACT_REQUEST_CAPACITY, d7
	bhs.w fail
	lea state.NativeCliArtifactRequestKinds, a0
	move.b d1, 0(a0, d7.l)
	move.l d7, d6
	add.l d6, d6
	lea state.NativeCliArtifactRequestLengths, a0
	move.w state.NativeCliSourceLineLen, d5
	move.w d5, 0(a0, d6.l)
	move.l d7, d6
	lsl.l #8, d6
	add.l d6, d6
	lea state.NativeCliArtifactRequestTexts, a1
	adda.l d6, a1
	lea state.NativeCliSourceLine, a0
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	moveq #0, d0
	move.w d5, d0
	jsr platform_profile.opforgePlatformProfileRangeStateV1
	jsr platform_profile.opforgePlatformProfileCopyRequestedV1
.endif
	moveq #0, d4
	move.w d5, d4
	beq.s stored
	subq.l #1, d4

copyLoop
	move.b (a0)+, (a1)+
	dbra d4, copyLoop

stored
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	moveq #0, d0
	move.w d5, d0
	jsr platform_profile.opforgePlatformProfileCopyCompletedV1
.endif
	addq.w #1, d7
	move.w d7, state.NativeCliArtifactRequestCount
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliCaptureArtifactRequestLineV1

; Build the parser-tail scratch buffer for `.module` / `.endmodule` / `.use`.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the current source line.
; Outputs: D0 = 0 on success, 1 on tail-buffer overflow; state.NativeCliParserTailBuffer/state.NativeCliParserTailLen updated on success.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliBuildParserTailBuffer	.block
	movem.l d1-d7/a0-a3, -(sp)
	bsr.w opforgeNativeCliParserTailFallbackEnd

haveEnd
	lea state.NativeCliParserTailBuffer, a1
	clr.w state.NativeCliParserTailLen
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	cmp.l d0, d6
	bhi.w fail

endOk
	lea state.NativeCliSourceLine, a0
	adda.l d6, a0
	sub.l d6, d0
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileRangeStateV1
	jsr platform_profile.opforgePlatformProfileCopyRequestedV1
.endif
	moveq #0, d5

copyLoop
	tst.l d0
	beq.w done
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY - 1, d5
	bhs.w copyFail
	move.b (a0)+, (a1)+
	addq.l #1, d5
	subq.l #1, d0
	bra.w copyLoop

done
	clr.b (a1)
	move.w d5, state.NativeCliParserTailLen
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.l d5, d0
	jsr platform_profile.opforgePlatformProfileCopyCompletedV1
.endif
	moveq #0, d0
	bra.s return

copyFail
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.l d5, d0
	jsr platform_profile.opforgePlatformProfileCopyCompletedV1
.endif
fail
	clr.b state.NativeCliParserTailBuffer
	clr.w state.NativeCliParserTailLen
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ;  opforgeNativeCliBuildParserTailBuffer

; Compute the fallback parser-tail offset when the table-backed parser did not provide one.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the current source line.
; Outputs: D6 = byte offset immediately after the recognized directive keyword, or 0 when no supported directive prefix matches.
; Clobbers: D0-D1/D5-D6/A0-A1/CCR.
; CCR: reflects the final directive-prefix comparison, not D6.
opforgeNativeCliParserTailFallbackEnd	.block
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	moveq #0, d5
	move.w state.NativeCliSourceLineLen, d5
	sub.l d0, d5
	lea strings.OutputDirectiveText, a1
	moveq #7, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.w output
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.w module
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.w endmodule
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.w use
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea strings.CpuMnemonicText, a1
	moveq #4, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.w cpu
	moveq #0, d6
	rts

module
	move.l d5, d6
	addq.l #7, d6
	rts

endmodule
	move.l d5, d6
	addi.l #10, d6
	rts

use
	move.l d5, d6
	addq.l #4, d6
	rts

cpu
	move.l d5, d6
	addq.l #4, d6
	rts

output
	move.l d5, d6
	addq.l #7, d6
	rts
	.bend  ; opforgeNativeCliParserTailFallbackEnd

; Return the parser-tail scratch buffer as a pointer/length pair.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the current source line.
; Outputs: D1 = 0 on success, 1 on failure; A0 = parser-tail buffer pointer on success; D0 = parser-tail byte length on success, otherwise build failure status.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D1 on return.
opforgeNativeCliParserTailPtr	.block
	bsr.w opforgeNativeCliBuildParserTailBuffer
	move.l d0, d1
	bne.s return
	lea state.NativeCliParserTailBuffer, a0
	moveq #0, d0
	move.w state.NativeCliParserTailLen, d0
	moveq #0, d1

return
	rts
	.bend  ; opforgeNativeCliParserTailPtr

; Parse one `.module` directive from the current source line.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on success, nonzero on parse/module-record failure.
; Clobbers: A0-A1/D1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseModuleLine	.block
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliArgToken, a1
	bsr.w line_text.opforgeNativeCliCopyLineWord
	bne.w fail
	tst.b state.NativeCliArgToken
	beq.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.s record
	cmpi.b #';', (a0)
	bne.w fail

record
	bsr.w module_use.opforgeNativeCliRecordModule
	bne.w fail
	moveq #0, d0
	move.w state.NativeCliCurrentModuleId, d0
	bsr.w module_use.opforgeNativeCliEmitModuleRecord
	moveq #0, d0
	move.w state.NativeCliCurrentModuleId, d0
	bsr.w module_use.opforgeNativeCliEmitModuleCompatibility
	moveq #0, d0
	rts

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putErrStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseModuleLine

; Parse one `.endmodule` directive from the current source line.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on success, nonzero on parse/module-depth failure.
; Clobbers: A0/D1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseEndmoduleLine	.block
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.s close
	cmpi.b #';', (a0)
	bne.w fail

close
	bsr.w module_use.opforgeNativeCliEmitCloseModule
	bne.w moduleDepthFail
	moveq #0, d0
	rts

moduleDepthFail
	move.l #strings.ModuleDepthFailureText, d1
	jsr dos.putErrStr
	moveq #1, d0
	rts

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putErrStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseEndmoduleLine

; Parse one `.use` directive from the current source line.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on success, nonzero failure status on parse/import errors; state.NativeCliResolvedModuleId and import tables updated for bare-module resolution when applicable.
; Clobbers: A0-A1/D1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseUseLine	.block
	movem.l d5-d6, -(sp)
	move.w #-1, state.NativeCliResolvedModuleId
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliArgToken, a1
	bsr.w line_text.opforgeNativeCliCopyUseToken
	tst.l d1
	bne.w fail
	tst.b state.NativeCliArgToken
	beq.w fail
	clr.b state.NativeCliIncludeTarget
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	bsr.w line_text.opforgeNativeCliParseUseOptionalAlias
	tst.l d1
	bne.w fail
	move.l d0, d5
	bsr.w module_use.opforgeNativeCliRecordImport
	bne.w fail
	bsr.w module_use.opforgeNativeCliResolveBareUseModule
	tst.l d1
	bne.w resolveFail
	moveq #0, d2
	move.w d4, d2
	add.w d2, d2
	lea state.NativeCliImportModuleTable, a1
	move.w d0, 0(a1, d2.l)

parseTail
	move.l d5, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.w bare
	cmpi.b #';', (a0)
	beq.w bare
	bsr.w module_use.opforgeNativeCliEmitImportRecord
	move.l d0, d6
	lea strings.MapKeywordText, a1
	moveq #3, d1
	move.l d4, -(sp)
	bsr.w line_text.opforgeNativeCliLineStartsWith
	move.l (sp)+, d4
	tst.l d0
	beq.s selective
	move.l d6, d0
	bsr.w line_text.opforgeNativeCliParseUseSectionMapsV1
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.w done
	cmpi.b #';', (a0)
	bne.w fail
	bra.w done
selective
	move.l d6, d0
	cmpi.b #'(', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bsr.w line_text.opforgeNativeCliParseUseItems
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.w done
	cmpi.b #';', (a0)
	bne.w fail
	bra.w done

bare
	bsr.w module_use.opforgeNativeCliEmitImportRecord

done
	moveq #0, d0
	movem.l (sp)+, d5-d6
	rts

resolveFail
	move.l #strings.ModuleResolveFailureText, d1
	jsr dos.putErrStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putErrStr
	move.l #strings.NewlineText, d1
	jsr dos.putErrStr
	moveq #1, d0
	movem.l (sp)+, d5-d6
	rts

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putErrStr
	moveq #1, d0
	movem.l (sp)+, d5-d6
	rts
	.bend  ; opforgeNativeCliParseUseLine

; Parse one `.cpu` directive from the current source line and switch the active native pipeline.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on success, nonzero on malformed or unavailable CPU selection.
; Clobbers: A0-A1/D1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseCpuLine	.block
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliArgToken, a1
	move.l d0, -(sp)
	bsr.w line_text.opforgeNativeCliCopyLineWord
	bne.w parseCpuCopyRestoreFail
	move.l (sp)+, d0
	sub.l d5, d0
	tst.b state.NativeCliArgToken
	beq.w fail
	move.l d0, -(sp)
	move.l a0, -(sp)
	bsr.w opforgeNativeCliNormalizeQuotedCpuToken
	tst.l d0
	bne.w parseCpuRestoreFail
	lea state.NativeCliArgToken, a0
	lea state.NativeCliCpuName, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer
	movea.l (sp)+, a0
	move.l (sp)+, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.s switchPipeline
	cmpi.b #';', (a0)
	bne.w fail

switchPipeline
	jsr package_pipeline.opforgeNativeCliApplyCurrentPipeline
	tst.l d0
	bne.w fail
	move.w #1, state.NativeCliPackagePipelineReady

updateSession
	lea buffers.ActiveCpuBuffer, a0
	jsr engine.setSessionCpuNameV1
	moveq #0, d0
	rts

parseCpuRestoreFail
	addq.l #8, sp
	bra.w fail

parseCpuCopyRestoreFail
	addq.l #4, sp

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putErrStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseCpuLine

; Strip one optional surrounding quote pair from state.NativeCliArgToken in place.
; Inputs: state.NativeCliArgToken contains one `.cpu` token.
; Outputs: D0 = 0 on success, 1 on malformed quoted token.
; Clobbers: D0-D1/A0-A1/CCR.
opforgeNativeCliNormalizeQuotedCpuToken	.block
	lea state.NativeCliArgToken, a0
	cmpi.b #'"', (a0)
	bne.s done
	jsr token_util.opforgeNativeCliTokenLen
	cmpi.w #2, d0
	blo.s fail
	lea state.NativeCliArgToken, a0
	movea.l a0, a1
	adda.l d0, a1
	subq.l #1, a1
	cmpi.b #'"', (a1)
	bne.s fail
	addq.l #1, a0
	lea state.NativeCliArgToken, a1

copyLoop
	move.b (a0)+, d1
	cmpi.b #'"', d1
	beq.s quotedDone
	move.b d1, (a1)+
	bra.s copyLoop

quotedDone
	clr.b (a1)

done
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliNormalizeQuotedCpuToken

; Parse one first-run `.output` directive for native `.bin`/`.prg`/`.hex` request selection.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on supported `format=bin`, `format=prg`, or `format=hex`; nonzero on malformed or unsupported output.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseOutputLine	.block
	movem.l d6-d7/a3, -(sp)
	moveq #0, d4
	moveq #0, d6
	clr.b state.NativeCliOutputPathScratch
	clr.w state.NativeCliPrgLoadAddrSet
	clr.l state.NativeCliPrgLoadAddr
	clr.w state.NativeCliSourceOutputSectionCount
	clr.w state.NativeCliSourceOutputSectionListActive
	clr.w state.NativeCliSourceOutputImageSet
	clr.w state.NativeCliSourceOutputFillSet
	move.w #1, state.NativeCliSourceOutputContiguous
	clr.b state.NativeCliSourceOutputFill
	clr.l state.NativeCliSourceOutputImageStart
	clr.l state.NativeCliSourceOutputImageEnd
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.w fail
	cmpi.b #'"', (a0)
	bne.s maybeOption
	lea state.NativeCliOutputPathScratch, a1
	bsr.w copyOutputQuotedPath
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.w fail
	cmpi.b #',', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bra.s optionLoop

maybeOption
	movea.l a0, a3
	move.l d0, d7
	bsr.w copyOutputOptionToken
	tst.l d1
	bne.w fail
	move.l d0, d5
	bsr.w classifyOutputOptionToken
	move.l d5, d0
	tst.l d1
	beq.s optionAfterToken
	cmpi.l #2, d1
	beq.w fail
	movea.l a3, a0
	move.l d7, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliOutputPathScratch, a1
	bsr.w copyOutputBarePath
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.w fail
	cmpi.b #',', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0

optionLoop
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.s finishOptions
	cmpi.b #';', (a0)
	beq.s finishOptions
	bsr.w copyOutputOptionToken
	tst.l d1
	bne.w fail
	move.l d0, d5
	bsr.w classifyOutputOptionToken
	move.l d5, d0
	cmpi.l #2, d1
	beq.w fail

optionAfterToken
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.s finishOptions
	cmpi.b #';', (a0)
	beq.s finishOptions
	bsr.w outputOptionDelimiter
	tst.l d1
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bra.w optionLoop

finishOptions
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_BIN, d6
	beq.s selectBin
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d6
	beq.s selectPrg
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HEX, d6
	beq.w selectHex
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_LST, d6
	beq.w selectLst
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, d6
	beq.w selectHunk
	bra.w fail

selectBin
	tst.b state.NativeCliOutputPathScratch
	beq.s defaultBinPath
	lea state.NativeCliOutputPathScratch, a0
	lea state.NativeCliBinPath, a1
	bsr.w opforgeNativeCliResolveOutputPath
	bra.s binPathReady

defaultBinPath
	tst.b state.NativeCliBinPath
	bne.s binPathReady
	tst.b state.NativeCliOutfileBase
	beq.w fail
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliBinPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer

binPathReady
	move.w #1, state.NativeCliBinRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_BIN, state.NativeCliOutputFormat
	moveq #0, d0
	movem.l (sp)+, d6-d7/a3
	rts

selectPrg
	tst.b state.NativeCliOutputPathScratch
	beq.s defaultPrgPath
	lea state.NativeCliOutputPathScratch, a0
	lea state.NativeCliPrgPath, a1
	bsr.w opforgeNativeCliResolveOutputPath
	bra.s prgPathReady

defaultPrgPath
	tst.b state.NativeCliPrgPath
	bne.s prgPathReady
	tst.b state.NativeCliOutfileBase
	beq.w fail
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliPrgPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer

prgPathReady
	move.w #1, state.NativeCliBinRequested
	move.w #1, state.NativeCliPrgRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_PRG, state.NativeCliOutputFormat
	moveq #0, d0
	movem.l (sp)+, d6-d7/a3
	rts

selectHex
	tst.b state.NativeCliOutputPathScratch
	beq.s defaultHexPath
	lea state.NativeCliOutputPathScratch, a0
	lea state.NativeCliHexPath, a1
	bsr.w opforgeNativeCliResolveOutputPath
	bra.s hexPathReady

defaultHexPath
	tst.b state.NativeCliHexPath
	bne.s hexPathReady
	tst.b state.NativeCliOutfileBase
	beq.w fail
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliHexPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer

hexPathReady
	move.w #1, state.NativeCliBinRequested
	move.w #1, state.NativeCliHexRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_HEX, state.NativeCliOutputFormat
	moveq #0, d0
	movem.l (sp)+, d6-d7/a3
	rts

selectLst
	tst.b state.NativeCliOutputPathScratch
	beq.s defaultLstPath
	lea state.NativeCliOutputPathScratch, a0
	lea state.NativeCliLstPath, a1
	bsr.w opforgeNativeCliResolveOutputPath
	bra.s lstPathReady

defaultLstPath
	tst.b state.NativeCliLstPath
	bne.s lstPathReady
	tst.b state.NativeCliOutfileBase
	beq.w fail
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliLstPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer

lstPathReady
	move.w #1, state.NativeCliBinRequested
	move.w #1, state.NativeCliLstRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_LST, state.NativeCliOutputFormat
	moveq #0, d0
	movem.l (sp)+, d6-d7/a3
	rts

selectHunk
	tst.b state.NativeCliOutputPathScratch
	beq.s defaultHunkPath
	lea state.NativeCliOutputPathScratch, a0
	lea state.NativeCliHunkPath, a1
	bsr.w opforgeNativeCliResolveOutputPath
	bra.s hunkPathReady
defaultHunkPath
	tst.b state.NativeCliHunkPath
	bne.s hunkPathReady
	tst.b state.NativeCliOutfileBase
	beq.w fail
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliHunkPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer
hunkPathReady
	move.w #1, state.NativeCliHunkRequested
	move.w #1, state.NativeCliBinRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, state.NativeCliOutputFormat
	moveq #0, d0
	movem.l (sp)+, d6-d7/a3
	rts

fail
	tst.w d4
	beq.s genericFailure
	move.l #strings.UnknownSrecOutputFormatText, d1
	bra.s emitFailure
genericFailure
	move.l #strings.ParserFailureText, d1
emitFailure
	jsr dos.putErrStr
	moveq #1, d0
	movem.l (sp)+, d6-d7/a3
	rts
	.bend  ; opforgeNativeCliParseOutputLine

; Resolve a source `.output` path against the current source file when relative.
; Inputs: A0 = source path token; A1 = destination path buffer.
; Outputs: D0 = 0 on success, 1 on capacity failure; destination is NUL-terminated.
; Clobbers: D0/D2/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliResolveOutputPath	.block
	movem.l d2/a2, -(sp)
	movea.l a0, a2
	jsr path.opforgeNativeCliPathHasVolumePrefix
	tst.l d0
	bne.s absolute
	tst.w state.NativeCliIncludeDepth
	bne.s resolveCurrentPath
	tst.w state.NativeCliModuleResolveDepth
	bne.s resolveCurrentPath
	; Rust leaves a root-source relative `.output` path relative to the process
	; working directory when no explicit out_dir exists.  Preserve the literal
	; path here; AmigaDOS resolves it against the guest's current directory.
	movea.l a2, a0
	jsr path.opforgeNativeCliCopyPathBuffer
	bra.s return

resolveCurrentPath
	lea state.NativeCliCurrentPath, a0

resolveBasePath
	jsr path.opforgeNativeCliCopyPathRoot
	bne.s fail
	movea.l a2, a0
	jsr path.opforgeNativeCliAppendPathSegmentBuffer
	bra.s return

absolute
	movea.l a2, a0
	jsr path.opforgeNativeCliCopyPathBuffer
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d2/a2
	rts
	.bend  ; opforgeNativeCliResolveOutputPath

; Classify one `.output` option token.
; Inputs: state.NativeCliArgToken contains the copied token.
; Outputs: D1 = 0 when recognized, 1 when ignored, 2 when malformed; D6 updated for format tokens.
; Clobbers: D0-D5/A0-A2/CCR.
classifyOutputOptionToken	.block
	bsr.w parseOutputFormatToken
	tst.l d1
	bne.s maybeLoadAddr
	rts

maybeLoadAddr
	bsr.w parseOutputLoadAddrToken
	tst.l d1
	beq.s return
	cmpi.l #2, d1
	beq.s return
	bsr.w parseOutputImageToken
	tst.l d1
	beq.s return
	cmpi.l #2, d1
	beq.s return
	bsr.w parseOutputFillToken
	tst.l d1
	beq.s return
	cmpi.l #2, d1
	beq.s return
	bsr.w parseOutputContiguousToken
	tst.l d1
	beq.s return
	cmpi.l #2, d1
	beq.s return
	bsr.w parseOutputSectionsToken
return
	rts
	.bend  ; classifyOutputOptionToken

; Parse `format=bin` or `format=prg` from the option token.
; Inputs: state.NativeCliArgToken contains one option token.
; Outputs: D1 = 0 when parsed, 1 when this is another option, 2 when malformed; D6 updated for recognized formats.
; Clobbers: D0-D1/A2/CCR.
parseOutputFormatToken	.block
	lea state.NativeCliArgToken, a2
	cmpi.b #'f', (a2)+
	bne.w unknown
	cmpi.b #'o', (a2)+
	bne.w unknown
	cmpi.b #'r', (a2)+
	bne.w unknown
	cmpi.b #'m', (a2)+
	bne.w unknown
	cmpi.b #'a', (a2)+
	bne.w unknown
	cmpi.b #'t', (a2)+
	bne.w unknown
	cmpi.b #'=', (a2)+
	bne.w unknown
	move.b (a2)+, d0
	cmpi.b #'b', d0
	beq.w maybeBin
	cmpi.b #'h', d0
	beq.w maybeHex
	cmpi.b #'l', d0
	beq.w maybeLst
	cmpi.b #'p', d0
	beq.w maybePrg
	cmpi.b #'s', d0
	beq.w maybeUnsupportedSrec
	bra.w malformed

maybeBin
	cmpi.b #'i', (a2)+
	bne.w malformed
	cmpi.b #'n', (a2)+
	bne.w malformed
	tst.b (a2)
	bne.w malformed
	move.w #constants.NATIVE_OUTPUT_FORMAT_BIN, d6
	moveq #0, d1
	rts

maybeHex
	move.b (a2)+, d0
	cmpi.b #'u', d0
	beq.s maybeHunk
	cmpi.b #'e', d0
	bne.w malformed
	cmpi.b #'x', (a2)+
	bne.w malformed
	tst.b (a2)
	bne.w malformed
	move.w #constants.NATIVE_OUTPUT_FORMAT_HEX, d6
	moveq #0, d1
	rts

maybeHunk
	cmpi.b #'n', (a2)+
	bne.w malformed
	cmpi.b #'k', (a2)+
	bne.w malformed
	tst.b (a2)
	bne.w malformed
	move.w #constants.NATIVE_OUTPUT_FORMAT_HUNK, d6
	moveq #0, d1
	rts

maybeLst
	cmpi.b #'s', (a2)+
	bne.w malformed
	cmpi.b #'t', (a2)+
	bne.w malformed
	tst.b (a2)
	bne.w malformed
	move.w #constants.NATIVE_OUTPUT_FORMAT_LST, d6
	moveq #0, d1
	rts

maybePrg
	cmpi.b #'r', (a2)+
	bne.s malformed
	cmpi.b #'g', (a2)+
	bne.s malformed
	tst.b (a2)
	bne.s malformed
	move.w #constants.NATIVE_OUTPUT_FORMAT_PRG, d6
	moveq #0, d1
	rts

maybeUnsupportedSrec
	cmpi.b #'r', (a2)+
	bne.s malformed
	cmpi.b #'e', (a2)+
	bne.s malformed
	cmpi.b #'c', (a2)+
	bne.s malformed
	tst.b (a2)
	bne.s malformed
	moveq #1, d4
	bra.s malformed

unknown
	moveq #1, d1
	rts

malformed
	moveq #2, d1
	rts
	.bend  ; parseOutputFormatToken

; Parse `loadaddr=$NNNN` from the option token.
; Inputs: state.NativeCliArgToken contains one option token.
; Outputs: D1 = 0 when parsed, 1 when this is another option, 2 when malformed/wide.
; Clobbers: D0-D5/A2/CCR.
parseOutputLoadAddrToken	.block
	lea state.NativeCliArgToken, a2
	cmpi.b #'l', (a2)+
	bne.w unknown
	cmpi.b #'o', (a2)+
	bne.w unknown
	cmpi.b #'a', (a2)+
	bne.w unknown
	cmpi.b #'d', (a2)+
	bne.w unknown
	cmpi.b #'a', (a2)+
	bne.w unknown
	cmpi.b #'d', (a2)+
	bne.w unknown
	cmpi.b #'d', (a2)+
	bne.w unknown
	cmpi.b #'r', (a2)+
	bne.w unknown
	cmpi.b #'=', (a2)+
	bne.w unknown
	cmpi.b #36, (a2)+
	bne.w malformed
	clr.l d2
	clr.l d3

loop
	move.b (a2)+, d0
	beq.s done
	bsr.w outputHexDigitToNibble
	tst.l d1
	bne.s malformed
	addq.l #1, d3
	cmpi.l #4, d3
	bhi.s malformed
	lsl.l #4, d2
	or.l d0, d2
	bra.s loop

done
	tst.l d3
	beq.s malformed
	move.l d2, state.NativeCliPrgLoadAddr
	move.w #1, state.NativeCliPrgLoadAddrSet
	moveq #0, d1
	rts

unknown
	moveq #1, d1
	rts

malformed
	moveq #2, d1
	rts
	.bend  ; parseOutputLoadAddrToken

; Parse `image="$START..$END"` from the current output option token.
; Outputs: D1 = 0 parsed, 1 unrelated, 2 malformed.
parseOutputImageToken	.block
	lea state.NativeCliArgToken, a2
	cmpi.b #'i', (a2)+
	bne.w unknown
	cmpi.b #'m', (a2)+
	bne.w unknown
	cmpi.b #'a', (a2)+
	bne.w unknown
	cmpi.b #'g', (a2)+
	bne.w unknown
	cmpi.b #'e', (a2)+
	bne.w unknown
	cmpi.b #'=', (a2)+
	bne.w unknown
	cmpi.b #'"', (a2)+
	bne.w malformed
	bsr.w parseOutputDollarHexAtA2
	bne.w malformed
	move.l d2, state.NativeCliSourceOutputImageStart
	cmpi.b #'.', (a2)+
	bne.w malformed
	cmpi.b #'.', (a2)+
	bne.w malformed
	bsr.w parseOutputDollarHexAtA2
	bne.w malformed
	move.l d2, state.NativeCliSourceOutputImageEnd
	cmpi.b #'"', (a2)+
	bne.w malformed
	tst.b (a2)
	bne.w malformed
	move.l state.NativeCliSourceOutputImageStart, d0
	cmp.l d0, d2
	blo.w malformed
	move.w #1, state.NativeCliSourceOutputImageSet
	clr.w state.NativeCliSourceOutputSectionListActive
	moveq #0, d1
	rts
unknown
	moveq #1, d1
	rts
malformed
	moveq #2, d1
	rts
	.bend  ; parseOutputImageToken

; Parse `fill=$NN` from the current output option token.
; Outputs: D1 = 0 parsed, 1 unrelated, 2 malformed.
parseOutputFillToken	.block
	lea state.NativeCliArgToken, a2
	cmpi.b #'f', (a2)+
	bne.s unknown
	cmpi.b #'i', (a2)+
	bne.s unknown
	cmpi.b #'l', (a2)+
	bne.s unknown
	cmpi.b #'l', (a2)+
	bne.s unknown
	cmpi.b #'=', (a2)+
	bne.s unknown
	bsr.w parseOutputDollarHexAtA2
	bne.s malformed
	tst.b (a2)
	bne.s malformed
	cmpi.l #$FF, d2
	bhi.s malformed
	move.b d2, state.NativeCliSourceOutputFill
	move.w #1, state.NativeCliSourceOutputFillSet
	clr.w state.NativeCliSourceOutputSectionListActive
	moveq #0, d1
	rts
unknown
	moveq #1, d1
	rts
malformed
	moveq #2, d1
	rts
	.bend  ; parseOutputFillToken

; Parse `contiguous=true|false` from the current output option token.
; Outputs: D1 = 0 parsed, 1 unrelated, 2 malformed.
parseOutputContiguousToken	.block
	lea state.NativeCliArgToken, a2
	lea OutputContiguousPrefix, a1
	moveq #10, d4
prefixLoop
	move.b (a1)+, d0
	cmp.b (a2)+, d0
	bne.s unknown
	dbra d4, prefixLoop
	cmpi.b #'f', (a2)
	beq.s falseValue
	cmpi.b #'t', (a2)
	bne.s malformed
	cmpi.b #'r', 1(a2)
	bne.s malformed
	cmpi.b #'u', 2(a2)
	bne.s malformed
	cmpi.b #'e', 3(a2)
	bne.s malformed
	tst.b 4(a2)
	bne.s malformed
	move.w #1, state.NativeCliSourceOutputContiguous
	bra.s parsed
falseValue
	cmpi.b #'a', 1(a2)
	bne.s malformed
	cmpi.b #'l', 2(a2)
	bne.s malformed
	cmpi.b #'s', 3(a2)
	bne.s malformed
	cmpi.b #'e', 4(a2)
	bne.s malformed
	tst.b 5(a2)
	bne.s malformed
	clr.w state.NativeCliSourceOutputContiguous
parsed
	clr.w state.NativeCliSourceOutputSectionListActive
	moveq #0, d1
	rts
unknown
	moveq #1, d1
	rts
malformed
	moveq #2, d1
	rts
	.bend  ; parseOutputContiguousToken

; Parse the first `sections=NAME` token and subsequent comma-delimited names.
; Outputs: D1 = 0 parsed, 1 unrelated, 2 malformed/capacity.
parseOutputSectionsToken	.block
	lea state.NativeCliArgToken, a2
	tst.w state.NativeCliSourceOutputSectionListActive
	bne.s copyName
	lea OutputSectionsPrefix, a1
	moveq #8, d4
prefixLoop
	move.b (a1)+, d0
	cmp.b (a2)+, d0
	bne.s unknown
	dbra d4, prefixLoop
	move.w #1, state.NativeCliSourceOutputSectionListActive
copyName
	tst.b (a2)
	beq.s malformed
	moveq #0, d3
	move.w state.NativeCliSourceOutputSectionCount, d3
	cmpi.w #constants.NATIVE_SOURCE_OUTPUT_SECTION_CAPACITY, d3
	bhs.s malformed
	move.l d3, d4
	lsl.l #5, d4
	lea state.NativeCliSourceOutputSectionNames, a1
	adda.l d4, a1
	moveq #constants.NATIVE_SOURCE_OUTPUT_SECTION_NAME_CAPACITY - 2, d4
nameLoop
	move.b (a2)+, d0
	beq.s nameDone
	tst.w d4
	beq.s malformed
	move.b d0, (a1)+
	subq.w #1, d4
	bra.s nameLoop
nameDone
	clr.b (a1)
	addq.w #1, d3
	move.w d3, state.NativeCliSourceOutputSectionCount
	moveq #0, d1
	rts
unknown
	moveq #1, d1
	rts
malformed
	moveq #2, d1
	rts
	.bend  ; parseOutputSectionsToken

; Parse a `$`-prefixed hexadecimal value at A2.
; Outputs: D0 = 0 success, 1 failure; D2 = value; A2 at first non-hex byte.
parseOutputDollarHexAtA2	.block
	cmpi.b #'$', (a2)+
	bne.s fail
	clr.l d2
	clr.l d3
loop
	move.b (a2), d0
	beq.s done
	bsr.w outputHexDigitToNibble
	tst.l d1
	bne.s done
	cmpi.l #8, d3
	bhs.s fail
	lsl.l #4, d2
	or.l d0, d2
	addq.l #1, a2
	addq.l #1, d3
	bra.s loop
done
	tst.l d3
	beq.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; parseOutputDollarHexAtA2

; Convert a hexadecimal ASCII byte to a nibble.
; Inputs: D0.B = ASCII character.
; Outputs: D0.L = nibble; D1 = 0 on success, 1 on non-hex.
; Clobbers: D0-D1/CCR.
outputHexDigitToNibble	.block
	cmpi.b #'0', d0
	blo.s maybeUpper
	cmpi.b #'9', d0
	bhi.s maybeUpper
	subi.b #'0', d0
	andi.l #$0000000F, d0
	moveq #0, d1
	rts

maybeUpper
	cmpi.b #'A', d0
	blo.s maybeLower
	cmpi.b #'F', d0
	bhi.s maybeLower
	subi.b #55, d0
	andi.l #$0000000F, d0
	moveq #0, d1
	rts

maybeLower
	cmpi.b #'a', d0
	blo.s fail
	cmpi.b #'f', d0
	bhi.s fail
	subi.b #87, d0
	andi.l #$0000000F, d0
	moveq #0, d1
	rts

fail
	moveq #1, d1
	rts
	.bend  ; outputHexDigitToNibble

; Copy a quoted `.output` path.
; Inputs: A0 = quote; D0 = remaining byte count; A1 = destination path buffer.
; Outputs: D0 = remaining byte count after closing quote; D1 = 0 success, 1 failure; A0 advanced.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D1 on return.
copyOutputQuotedPath	.block
	addq.l #1, a0
	subq.l #1, d0
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d2
	clr.l d3

loop
	tst.l d0
	beq.s fail
	cmpi.b #'"', (a0)
	beq.s close
	tst.l d2
	beq.s fail
	move.b (a0)+, (a1)+
	subq.l #1, d0
	subq.l #1, d2
	addq.l #1, d3
	bra.s loop

close
	tst.l d3
	beq.s fail
	clr.b (a1)
	addq.l #1, a0
	subq.l #1, d0
	moveq #0, d1
	rts

fail
	clr.b (a1)
	moveq #1, d1
	rts
	.bend  ; copyOutputQuotedPath

; Copy a bare `.output` path.
; Inputs: A0 = path text; D0 = remaining byte count; A1 = destination path buffer.
; Outputs: D0 = remaining byte count after path; D1 = 0 success, 1 failure; A0 advanced.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D1 on return.
copyOutputBarePath	.block
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d2
	clr.l d3

loop
	tst.l d0
	beq.s done
	cmpi.b #' ', (a0)
	beq.s done
	cmpi.b #9, (a0)
	beq.s done
	cmpi.b #',', (a0)
	beq.s done
	cmpi.b #';', (a0)
	beq.s done
	tst.l d2
	beq.s fail
	move.b (a0)+, (a1)+
	subq.l #1, d0
	subq.l #1, d2
	addq.l #1, d3
	bra.s loop

done
	tst.l d3
	beq.s fail
	clr.b (a1)
	moveq #0, d1
	rts

fail
	clr.b (a1)
	moveq #1, d1
	rts
	.bend  ; copyOutputBarePath

; Copy one comma-delimited `.output` option token.
; Inputs: A0 = option text; D0 = remaining byte count.
; Outputs: D1 = 0 success, 1 failure; A0/D0 advanced to option delimiter; state.NativeCliArgToken contains token.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D1 on return.
copyOutputOptionToken	.block
	lea state.NativeCliArgToken, a1
	move.l #constants.TOKEN_BUFFER_CAPACITY - 1, d2
	clr.l d3

loop
	tst.l d0
	beq.s done
	cmpi.b #' ', (a0)
	beq.s done
	cmpi.b #9, (a0)
	beq.s done
	cmpi.b #',', (a0)
	beq.s done
	cmpi.b #';', (a0)
	beq.s done
	tst.l d2
	beq.s fail
	move.b (a0)+, (a1)+
	subq.l #1, d0
	subq.l #1, d2
	addq.l #1, d3
	bra.s loop

done
	tst.l d3
	beq.s fail
	clr.b (a1)
	moveq #0, d1
	rts

fail
	clr.b (a1)
	moveq #1, d1
	rts
	.bend  ; copyOutputOptionToken

; Require a comma after a skipped `.output` option.
; Inputs: A0/D0 = current option delimiter.
; Outputs: D1 = 0 when current byte is comma, 1 otherwise.
; Clobbers: D1/CCR.
; CCR: reflects D1 on return.
outputOptionDelimiter	.block
	tst.l d0
	beq.s fail
	cmpi.b #',', (a0)
	bne.s fail
	moveq #0, d1
	rts

fail
	moveq #1, d1
	rts
	.bend  ; outputOptionDelimiter

; Accept end, comment, comma, or whitespace after `format=bin`.
; Inputs: A0/D0 = current option delimiter.
; Outputs: D1 = 0 on accepted delimiter, 1 otherwise.
; Clobbers: D1/CCR.
; CCR: reflects D1 on return.
outputOptionEndOk	.block
	tst.l d0
	beq.s ok
	cmpi.b #',', (a0)
	beq.s ok
	cmpi.b #';', (a0)
	beq.s ok
	cmpi.b #' ', (a0)
	beq.s ok
	cmpi.b #9, (a0)
	beq.s ok
	moveq #1, d1
	rts

ok
	moveq #0, d1
	rts
	.bend  ; outputOptionEndOk

	.endsection

	.section data, kind=data

OutputContiguousPrefix
	.byte "contiguous=", 0
OutputSectionsPrefix
	.byte "sections=", 0

	.endsection
	.endmodule
