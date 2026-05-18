; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.directive_handlers
	.cpu 68020

	.use opforge.cli.state (NativeCliSourceLine, NativeCliCurrentModuleId, NativeCliArgToken, NativeCliIncludeTarget)
	.use opforge.cli.state (NativeCliSourceLineLen, NativeCliModuleResolveDepth, NativeCliResolvedModuleId)
	.use opforge.cli.state (NativeCliImportModuleTable)
	.use opforge.cli.state (NativeCliParserTailBuffer, NativeCliParserTailLen)
	.use opforge.cli.constants (SOURCE_LINE_BUFFER_CAPACITY)
	.use opforge.cli.strings (ParserFailureText, ModuleDirectiveText, EndmoduleDirectiveText, UseDirectiveText, NewlineText, ModuleResolveFailureText, ModuleDepthFailureText)
	.use opforge.cli.dos (opforgeNativeCliPutStr)
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace, opforgeNativeCliLineStartsWith, opforgeNativeCliCopyLineWord, opforgeNativeCliCopyUseToken, opforgeNativeCliParseUseOptionalAlias, opforgeNativeCliParseUseItems)
	.use opforge.cli.module_use (opforgeNativeCliRecordModule, opforgeNativeCliEmitModuleRecord, opforgeNativeCliEmitModuleCompatibility, opforgeNativeCliCloseModule, opforgeNativeCliRecordImport, opforgeNativeCliEmitImportRecord, opforgeNativeCliResolveBareUseModule)

	.section code, kind=code
	.pub

opforgeNativeCliBuildParserTailBuffer	.block
	movem.l d1-d7/a0-a3, -(sp)
	bsr.w opforgeNativeCliParserTailFallbackEnd

haveEnd
	lea NativeCliParserTailBuffer, a1
	clr.w NativeCliParserTailLen
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	cmp.l d0, d6
	bhi.w fail

endOk
	lea NativeCliSourceLine, a0
	adda.l d6, a0
	sub.l d6, d0
	moveq #0, d5

copyLoop
	tst.l d0
	beq.w done
	cmpi.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d5
	bhs.w fail
	move.b (a0)+, (a1)+
	addq.l #1, d5
	subq.l #1, d0
	bra.w copyLoop

done
	clr.b (a1)
	move.w d5, NativeCliParserTailLen
	moveq #0, d0
	bra.s return

fail
	clr.b NativeCliParserTailBuffer
	clr.w NativeCliParserTailLen
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ;  opforgeNativeCliBuildParserTailBuffer

opforgeNativeCliParserTailFallbackEnd	.block
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	moveq #0, d5
	move.w NativeCliSourceLineLen, d5
	sub.l d0, d5
	lea ModuleDirectiveText, a1
	moveq #7, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.s module
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea EndmoduleDirectiveText, a1
	moveq #10, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.s endmodule
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea UseDirectiveText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
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

opforgeNativeCliParserTailPtr	.block
	bsr.w opforgeNativeCliBuildParserTailBuffer
	move.l d0, d1
	tst.l d1
	bne.s return
	lea NativeCliParserTailBuffer, a0
	moveq #0, d0
	move.w NativeCliParserTailLen, d0
	moveq #0, d1

return
	rts
	.bend  ; opforgeNativeCliParserTailPtr

opforgeNativeCliParseModuleLine	.block
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyLineWord
	tst.l d0
	bne.w fail
	tst.b NativeCliArgToken
	beq.w fail
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s record
	cmpi.b #';', (a0)
	bne.w fail

record
	bsr.w opforgeNativeCliRecordModule
	tst.l d0
	bne.w fail
	moveq #0, d0
	move.w NativeCliCurrentModuleId, d0
	bsr.w opforgeNativeCliEmitModuleRecord
	moveq #0, d0
	move.w NativeCliCurrentModuleId, d0
	bsr.w opforgeNativeCliEmitModuleCompatibility
	moveq #0, d0
	rts

fail
	move.l #ParserFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseModuleLine

opforgeNativeCliParseEndmoduleLine	.block
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s close
	cmpi.b #';', (a0)
	bne.w fail

close
	bsr.w opforgeNativeCliCloseModule
	tst.l d0
	bne.w moduleDepthFail
	moveq #0, d0
	rts

moduleDepthFail
	move.l #ModuleDepthFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts

fail
	move.l #ParserFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseEndmoduleLine

opforgeNativeCliParseUseLine	.block
	move.w #-1, NativeCliResolvedModuleId
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w fail
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyUseToken
	tst.l d1
	bne.w fail
	tst.b NativeCliArgToken
	beq.w fail
	clr.b NativeCliIncludeTarget
	bsr.w opforgeNativeCliSkipLineWhitespace
	bsr.w opforgeNativeCliParseUseOptionalAlias
	tst.l d1
	bne.w fail
	move.l d0, d5
	bsr.w opforgeNativeCliRecordImport
	tst.l d0
	bne.w fail
	move.l d5, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w bare
	cmpi.b #';', (a0)
	beq.w bare
	bsr.w opforgeNativeCliEmitImportRecord
	cmpi.b #'(', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bsr.w opforgeNativeCliParseUseItems
	tst.l d1
	bne.w fail
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w done
	cmpi.b #';', (a0)
	bne.w fail
	bra.w done

bare
	tst.b NativeCliIncludeTarget
	bne.s bareEmit
	tst.w NativeCliModuleResolveDepth
	bne.s bareEmit
	bsr.w opforgeNativeCliResolveBareUseModule
	tst.l d1
	bne.w resolveFail
	moveq #0, d2
	move.w d4, d2
	add.w d2, d2
	lea NativeCliImportModuleTable, a1
	move.w d0, 0(a1, d2.l)

bareEmit
	bsr.w opforgeNativeCliEmitImportRecord

done
	moveq #0, d0
	rts

resolveFail
	move.l #ModuleResolveFailureText, d1
	jsr opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts

fail
	move.l #ParserFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseUseLine

	.endsection
	.endmodule
