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
	.use opforge.cli.token_util

	.section code, kind=code
	.pub

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
	moveq #0, d5

copyLoop
	tst.l d0
	beq.w done
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY - 1, d5
	bhs.w fail
	move.b (a0)+, (a1)+
	addq.l #1, d5
	subq.l #1, d0
	bra.w copyLoop

done
	clr.b (a1)
	move.w d5, state.NativeCliParserTailLen
	moveq #0, d0
	bra.s return

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
	bne.s output
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.s module
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.s endmodule
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	bsr.w line_text.opforgeNativeCliLineStartsWith
	bne.s use
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
	jsr dos.putStr
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
	jsr dos.putStr
	moveq #1, d0
	rts

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseEndmoduleLine

; Parse one `.use` directive from the current source line.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on success, nonzero failure status on parse/import errors; state.NativeCliResolvedModuleId and import tables updated for bare-module resolution when applicable.
; Clobbers: A0-A1/D1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseUseLine	.block
	movem.l d5, -(sp)
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
	tst.w state.NativeCliModuleResolveDepth
	bne.s parseTail
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
	movem.l (sp)+, d5
	rts

resolveFail
	move.l #strings.ModuleResolveFailureText, d1
	jsr dos.putStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	moveq #1, d0
	movem.l (sp)+, d5
	rts

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	movem.l (sp)+, d5
	rts
	.bend  ; opforgeNativeCliParseUseLine

; Parse one first-run `.output` directive for native `.bin` request selection.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on supported `format=bin`, nonzero on malformed or unsupported output.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseOutputLine	.block
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	beq.w fail
	cmpi.b #'"', (a0)
	bne.s maybeOption
	lea state.NativeCliBinPath, a1
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
	bsr.w copyOutputOptionToken
	tst.l d1
	bne.w fail
	movea.l a0, a2
	lea state.NativeCliArgToken, a0
	lea strings.OutputFormatBinOptionText, a1
	jsr token_util.opforgeNativeCliTokenEquals
	movea.l a2, a0
	bne.s haveBinFormatFromToken
	lea state.NativeCliParserTailBuffer, a0
	moveq #0, d0
	move.w state.NativeCliParserTailLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliBinPath, a1
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
	beq.w fail
	bsr.w copyOutputOptionToken
	tst.l d1
	bne.w fail
	movea.l a0, a2
	lea state.NativeCliArgToken, a0
	lea strings.OutputFormatBinOptionText, a1
	jsr token_util.opforgeNativeCliTokenEquals
	movea.l a2, a0
	bne.s haveBinFormatFromToken
	bsr.w outputOptionDelimiter
	tst.l d1
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bra.s optionLoop

haveBinFormatFromToken
	bsr.w outputOptionEndOk
	tst.l d1
	bne.w fail
	tst.b state.NativeCliBinPath
	bne.s pathReady
	tst.b state.NativeCliOutfileBase
	beq.w fail
	lea state.NativeCliOutfileBase, a0
	lea state.NativeCliBinPath, a1
	jsr token_util.opforgeNativeCliCopyTokenBuffer

pathReady
	move.w #1, state.NativeCliBinRequested
	move.w #constants.NATIVE_OUTPUT_FORMAT_BIN, state.NativeCliOutputFormat
	moveq #0, d0
	rts

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseOutputLine

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
	.endmodule
