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
; Clobbers: A0-A1/D1/D5/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseUseLine	.block
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
	tst.b state.NativeCliIncludeTarget
	bne.s bareEmit
	tst.w state.NativeCliModuleResolveDepth
	bne.s bareEmit
	bsr.w module_use.opforgeNativeCliResolveBareUseModule
	tst.l d1
	bne.w resolveFail
	moveq #0, d2
	move.w d4, d2
	add.w d2, d2
	lea state.NativeCliImportModuleTable, a1
	move.w d0, 0(a1, d2.l)

bareEmit
	bsr.w module_use.opforgeNativeCliEmitImportRecord

done
	moveq #0, d0
	rts

resolveFail
	move.l #strings.ModuleResolveFailureText, d1
	jsr dos.putStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	moveq #1, d0
	rts

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseUseLine

	.endsection
	.endmodule
