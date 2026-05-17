; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.line_processor
	.cpu 68020

	.use opforge.cli.tkpkg_control_block (opforgeNativeCliWriteInputWindow, opforgeNativeCliReadOutputLen, opforgeNativeCliReadStatus)
	.use tkpkg.amigaos.abi (ENTRY_ORD_TOKENIZE_LINE)
	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_PTR_V1)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)
	.use opforge.cli.dos (opforgeNativeCliPutStr)

	.use opforge.cli.strings (NewlineText, ConditionalFailureText, NativeBadOrgText, ParserFailureText, IfdefDirectiveText, IfndefDirectiveText, ElseifDirectiveText,ElseDirectiveText, EndifDirectiveText,IfDirectiveText, IncludeDirectiveText, OrgMnemonicText)
	.use opforge.cli.constants (NCLI_PARSER_DIRECTIVE_MODULE, NCLI_PARSER_DIRECTIVE_ENDMODULE, NCLI_PARSER_DIRECTIVE_USE)

	.use opforge.cli.state (NativeCliSourceLine, NativeCliSourceLineLen, NativeCliSourceLineNum, NativeCliIncludeDepth, NativeCliLineRequestLen, NativeCliPrvmRouteStatus, NativeCliPrvmResultCount)

	.use opforge.cli.include_use (opforgeNativeCliParseIncludeLine)
	.use opforge.cli.directive_handlers (opforgeNativeCliParseModuleLine, opforgeNativeCliParseEndmoduleLine, opforgeNativeCliParseUseLine)
	.use opforge.cli.parser_route (opforgeNativeCliDispatchParseLineUntilReady, opforgeNativeCliParserDirectiveKind)

	.use opforge.cli.assembly_session (opforgeNativeCliRecordSourceLine, opforgeNativeCliRecordPrvmStatementLine)

	.use opforge.cli.report (opforgeNativeCliEmitIncludeLineRecord)
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace, opforgeNativeCliLineStartsWith)
	.use opforge.cli.copy (opforgeNativeCliCopyBytes)

	.section code, kind=code
	.pub

opforgeNativeCliTokenizeCurrentLine .block
	tst.w NativeCliIncludeDepth
	beq.s record
	jsr opforgeNativeCliEmitIncludeLineRecord

record
	jsr opforgeNativeCliRecordSourceLine
	bsr.w opforgeNativeCliPrepareLineServiceRequest
	tst.l d0
	bne.s fail

	lea ControlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliLineRequestLen, d1
	jsr opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_TOKENIZE_LINE, d0
	jsr tkpkgServiceDispatchV1
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadStatus
	tst.b d0
	bne.s fail
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadOutputLen
	tst.w d0
	beq.s ok
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.w)
	move.l #lastErrorBuffer, d1
	jsr opforgeNativeCliPutStr

ok
	bsr.w opforgeNativeCliParseCurrentLine
	tst.l d0
	bne.s fail
	moveq #0, d0
	rts

fail
	lea ControlBlockV1, a0
	jsr opforgeNativeCliReadOutputLen
	tst.w d0
	beq.s return
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	move.l #lastErrorBuffer, d1
	jsr opforgeNativeCliPutStr
	move.l #NewlineText, d1
	jsr opforgeNativeCliPutStr

return
	moveq #1, d0
	rts
	.bend ; opforgeNativeCliTokenizeCurrentLine


opforgeNativeCliParseCurrentLine .block
	movem.l d2-d7/a2-a4, -(sp)
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	jsr opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w done
	movea.l a0, a4
	move.l d0, d7

	lea IfdefDirectiveText, a1
	moveq #6, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea IfndefDirectiveText, a1
	moveq #7, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea ElseifDirectiveText, a1
	moveq #7, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea ElseDirectiveText, a1
	moveq #5, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea EndifDirectiveText, a1
	moveq #6, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea IfDirectiveText, a1
	moveq #3, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea IncludeDirectiveText, a1
	moveq #8, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	beq.s checkOrg
	jsr opforgeNativeCliParseIncludeLine
	bra.w return

checkOrg
	movea.l a4, a0
	move.l d7, d0
	lea OrgMnemonicText, a1
	moveq #4, d1
	jsr opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w badOrgLine

	bsr.w opforgeNativeCliRouteParserModuleUseLine
	cmpi.w #NCLI_PARSER_DIRECTIVE_MODULE, d0
	bne.s checkEndmodule
	jsr opforgeNativeCliParseModuleLine
	bra.w return

checkEndmodule
	cmpi.w #NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	bne.s checkUse
	jsr opforgeNativeCliParseEndmoduleLine
	bra.w return

checkUse
	cmpi.w #NCLI_PARSER_DIRECTIVE_USE, d0
	bne.s recordStatement
	jsr opforgeNativeCliParseUseLine
	bra.w return

recordStatement
	jsr opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail

done
	moveq #0, d0
	bra.w return

conditionalLine
	move.l #ConditionalFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	bra.w return

badOrgLine
	move.l #NativeBadOrgText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0
	bra.w return

fail
	move.l #ParserFailureText, d1
	jsr opforgeNativeCliPutStr
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a2-a4
	rts
	.bend ; opforgeNativeCliParseCurrentLine

opforgeNativeCliRouteParserModuleUseLine .block
	movem.l d1-d7/a0-a3, -(sp)
	clr.l NativeCliPrvmRouteStatus
	clr.w NativeCliPrvmResultCount
	jsr opforgeNativeCliDispatchParseLineUntilReady
	jsr opforgeNativeCliParserDirectiveKind
	
return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend ; opforgeNativeCliRouteParserModuleUseLine


; Build the tokenizer request payload: u32 line number plus source bytes.
opforgeNativeCliPrepareLineServiceRequest .block
	lea lastErrorBuffer, a2
	move.l NativeCliSourceLineNum, d2  ; line number is little-endian to match package fixtures
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lea NativeCliSourceLine, a1
	move.w NativeCliSourceLineLen, d0
	jsr opforgeNativeCliCopyBytes
	move.w NativeCliSourceLineLen, d1
	addq.w #4, d1
	move.w d1, NativeCliLineRequestLen
	moveq #0, d0
	rts
	.bend ; opforgeNativeCliPrepareLineServiceRequest


	.endsection
	.endmodule
