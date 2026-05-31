; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.line_processor
	.cpu 68020

	.use opforge.cli.tkpkg_control_block
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.service
	.use opforge.cli.dos

	.use opforge.cli.strings
	.use opforge.cli.constants

	.use opforge.cli.state

	.use opforge.cli.include_use
	.use opforge.cli.directive_handlers
	.use opforge.cli.prvm_bridge

	.use opforge.cli.assembly_session

	.use opforge.cli.report
	.use opforge.cli.line_text
	.use opforge.cli.copy

	.section code, kind=code
	.pub

opforgeNativeCliTokenizeCurrentLine	.block
	tst.w state.NativeCliIncludeDepth
	beq.s record
	jsr report.opforgeNativeCliEmitIncludeLineRecord

record
	jsr assembly_session.opforgeNativeCliRecordSourceLine
	bsr.w opforgeNativeCliPrepareLineServiceRequest
	tst.l d0
	bne.s fail

	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w state.NativeCliLineRequestLen, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_TOKENIZE_LINE, d0
	jsr service.dispatchV1
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadStatus
	bne.s fail
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadOutputLen
	beq.s ok
	lea buffers.lastErrorBuffer, a1
	clr.b 0(a1, d0.w)
	move.l #buffers.lastErrorBuffer, d1
	jsr dos.putStr

ok
	bsr.w opforgeNativeCliParseCurrentLine
	tst.l d0
	bne.s fail
	moveq #0, d0
	rts

fail
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadOutputLen
	beq.s return
	lea buffers.lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	move.l #buffers.lastErrorBuffer, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr

return
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeCurrentLine

opforgeNativeCliParseCurrentLine	.block
	movem.l d2-d7/a2-a4, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w done
	movea.l a0, a4
	move.l d0, d7

	lea strings.IfdefDirectiveText, a1
	moveq #6, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.IfndefDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.ElseifDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.ElseDirectiveText, a1
	moveq #5, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.EndifDirectiveText, a1
	moveq #6, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.IfDirectiveText, a1
	moveq #3, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.IncludeDirectiveText, a1
	moveq #8, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkOrg
	jsr include_use.opforgeNativeCliParseIncludeLine
	bra.w return

checkOrg
	movea.l a4, a0
	move.l d7, d0
	lea strings.OrgMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w badOrgLine

	bsr.w opforgeNativeCliRouteParserModuleUseLine
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_MODULE, d0
	bne.s checkEndmodule
	jsr directive_handlers.opforgeNativeCliParseModuleLine
	bra.w return

checkEndmodule
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	bne.s checkUse
	jsr directive_handlers.opforgeNativeCliParseEndmoduleLine
	bra.w return

checkUse
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_USE, d0
	bne.s recordStatement
	jsr directive_handlers.opforgeNativeCliParseUseLine
	bra.w return

recordStatement
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail

done
	moveq #0, d0
	bra.w return

conditionalLine
	move.l #strings.ConditionalFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	bra.w return

badOrgLine
	move.l #strings.NativeBadOrgText, d1
	jsr dos.putStr
	moveq #1, d0
	bra.w return

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putStr
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a2-a4
	rts
	.bend  ; opforgeNativeCliParseCurrentLine

opforgeNativeCliRouteParserModuleUseLine	.block
	movem.l d1-d7/a0-a3, -(sp)
	clr.l state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr prvm_bridge.opforgeNativeCliDispatchParseLineUntilReady
	jsr prvm_bridge.opforgeNativeCliParserDirectiveKind
	
return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; opforgeNativeCliRouteParserModuleUseLine

; Build the tokenizer request payload: u32 line number plus source bytes.
opforgeNativeCliPrepareLineServiceRequest	.block
	lea buffers.lastErrorBuffer, a2
	move.l state.NativeCliSourceLineNum, d2  ; line number is little-endian to match package fixtures
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lea state.NativeCliSourceLine, a1
	move.w state.NativeCliSourceLineLen, d0
	jsr copy.copyBytes
	move.w state.NativeCliSourceLineLen, d1
	addq.w #4, d1
	move.w d1, state.NativeCliLineRequestLen
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliPrepareLineServiceRequest

	.endsection
	.endmodule
