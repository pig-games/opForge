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
	.use opforge.cli.text_output
	.use opforge.cli.copy
	.use opforge.cli.preprocessor

	.section code, kind=code
	.pub

opforgeNativeCliTokenizeCurrentLine	.block
	jsr preprocessor.opforgeNativeCliCaptureMacroDefinitionLineV1
	tst.l d0
	beq.s preprocessPass
	bmi.w fail
	moveq #0, d0
	rts

preprocessPass
	jsr preprocessor.opforgeNativeCliParseMacroInvocationV1
	tst.l d0
	beq.s invocationPass
	bmi.w fail
	bsr.w opforgeNativeCliExpandActiveMacroV1
	rts

invocationPass
	tst.w state.NativeCliIncludeDepth
	beq.s record
	jsr report.opforgeNativeCliEmitIncludeLineRecord

record
	jsr assembly_session.opforgeNativeCliRecordSourceLine
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s checkPackage
	cmpi.b #';', (a0)
	beq.w commentOnly
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkPackage
	jsr directive_handlers.opforgeNativeCliParseUseLine
	rts

checkPackage
	tst.w state.NativeCliPackagePipelineReady
	beq.w parseOnly
	bsr.w opforgeNativeCliDispatchTokenizeLineEnvelope
	bne.w fail
	jsr prvm_bridge.opforgeNativeCliSampleActivePrvmLengthField
	move.l d0, state.NativeCliPrvmTokenizerDetail
	jsr prvm_bridge.opforgeNativeCliDispatchParseLineUntilReady
	bne.w fail

ok
	bsr.w opforgeNativeCliParseCurrentLine
	tst.l d0
	bne.w fail
commentOnly
	moveq #0, d0
	rts

parseOnly
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w parseOnlyOk
	movea.l a0, a4
	move.l d0, d7
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckModule
	jsr directive_handlers.opforgeNativeCliParseEndmoduleLine
	tst.l d0
	bne.w parseOnlyStatus
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w parseOnlyStatus

parseOnlyCheckModule
	movea.l a4, a0
	move.l d7, d0
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckUse
	jsr directive_handlers.opforgeNativeCliParseModuleLine
	tst.l d0
	bne.w parseOnlyStatus
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w parseOnlyStatus

parseOnlyCheckUse
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckCpu
	move.w state.NativeCliImportCount, d6
	jsr directive_handlers.opforgeNativeCliParseUseLine
	tst.l d0
	bne.w parseOnlyStatus
	cmp.w state.NativeCliImportCount, d6
	bne.w parseOnlyOk
	move.l #strings.ModuleResolveFailureText, d1
	jsr dos.putStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	moveq #1, d0

parseOnlyCheckCpu
	movea.l a4, a0
	move.l d7, d0
	lea strings.CpuMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckInclude
	jsr directive_handlers.opforgeNativeCliParseCpuLine
	tst.l d0
	bne.w parseOnlyStatus
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w parseOnlyStatus

parseOnlyCheckInclude
	movea.l a4, a0
	move.l d7, d0
	lea strings.IncludeDirectiveText, a1
	moveq #8, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckOutput
	jsr include_use.opforgeNativeCliParseIncludeLine
	bra.w parseOnlyStatus

parseOnlyCheckOutput
	movea.l a4, a0
	move.l d7, d0
	lea strings.OutputDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyRecordSourceStatement
	bra.w parseOnlyOk

parseOnlyRecordSourceStatement
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine

parseOnlyStatus
	tst.l d0
	bne.w return

parseOnlyOk
	moveq #0, d0
	rts

fail
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadLastErrorLen
	tst.w d0
	bne.w haveMessage
	tst.l state.NativeCliPrvmRouteStatus
	beq.w readOutput
	move.l #strings.PrvmRouteStatusText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmRouteStatus, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	tst.l state.NativeCliPrvmPipelineDetail
	beq.s maybeTokenizerDetail
	move.l #strings.PrvmPipelineDetailText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmPipelineDetail, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
maybeTokenizerDetail
	tst.l state.NativeCliPrvmTokenizerDetail
	beq.w maybeRouteDetail
	move.l #strings.PrvmTokenizerDetailText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmTokenizerDetail, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
maybeRouteDetail
	tst.l state.NativeCliPrvmRouteDetail
	beq.w readOutput
	move.l #strings.PrvmRouteDetailText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmRouteDetail, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr

readOutput
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadOutputLen
	beq.w return

haveMessage
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

; Re-enter the ordinary native line pipeline for one bounded expanded line.
; Inputs: A0/D0 = expansion text/length; current source line is the caller frame.
; Outputs: D0 = 0 on success, 1 on length/depth or pipeline failure.
; Clobbers: D0-D1/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliProcessExpandedLineV1	.block
	jsr preprocessor.opforgeNativeCliBeginExpandedLineV1
	bne.s fail
	bsr.w opforgeNativeCliTokenizeCurrentLine
	move.l d0, d1
	jsr preprocessor.opforgeNativeCliEndExpandedLineV1
	move.l d1, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliProcessExpandedLineV1

; Drain the active bounded macro frame through the ordinary CLI line path.
; Inputs: a complete preprocessor invocation frame is active.
; Outputs: D0 = 0 on success, 1 on substitution, recursion, or pipeline failure.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliExpandActiveMacroV1	.block
	tst.w state.NativeCliPreprocessInvocationDefinition
	bmi.w fail
	bsr.w emitMacroBlockStart
	bne.w fail
	lea state.NativeCliPreprocessExpansionLine, a0
	moveq #0, d0
	move.w state.NativeCliPreprocessExpansionLineLen, d0
	bsr.w opforgeNativeCliProcessExpandedLineV1
	bne.w fail
	clr.w state.NativeCliPreprocessInvocationBodyIndex

bodyLoop
	move.w state.NativeCliPreprocessInvocationDefinition, d2
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationBodyIndex, d0
	lea state.NativeCliPreprocessDefinitionBodyCount, a0
	moveq #0, d4
	add.w d2, d2
	move.w 0(a0, d2.w), d4
	cmp.w d4, d0
	bcc.s close
	jsr preprocessor.opforgeNativeCliSubstituteMacroBodyLineV1
	bne.w fail
	move.w d1, state.NativeCliPreprocessExpansionLineLen
	lea state.NativeCliPreprocessExpansionLine, a0
	move.l d1, d0
	bsr.w opforgeNativeCliProcessExpandedLineV1
	bne.w fail
	addq.w #1, state.NativeCliPreprocessInvocationBodyIndex
	bra.s bodyLoop

close
	bsr.w emitMacroBlockEnd
	bne.s fail
	lea state.NativeCliPreprocessExpansionLine, a0
	moveq #0, d0
	move.w state.NativeCliPreprocessExpansionLineLen, d0
	bsr.w opforgeNativeCliProcessExpandedLineV1
	bne.s fail
	move.w #-1, state.NativeCliPreprocessInvocationDefinition
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliExpandActiveMacroV1

; Build the Rust-compatible macro scope start line in ExpansionLine.
; A caller label attaches to `.block`; an indented call keeps its indentation.
emitMacroBlockStart	.block
	lea state.NativeCliPreprocessExpansionLine, a2
	moveq #0, d2
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationLabelLen, d0
	beq.s indent
	lea state.NativeCliPreprocessInvocationLabel, a1
	jsr copy.copyBytes
	move.w state.NativeCliPreprocessInvocationLabelLen, d2
	lea MacroBlockWithLabelText, a1
	moveq #7, d0
	jsr copy.copyBytes
	addq.w #7, d2
	bra.s done
indent
	lea state.NativeCliSourceLine, a1
indentLoop
	move.b 0(a1, d2.w), d0
	cmpi.b #' ', d0
	beq.s copyIndent
	cmpi.b #9, d0
	bne.s block
copyIndent
	move.b d0, 0(a2, d2.w)
	addq.w #1, d2
	bra.s indentLoop
block
	lea MacroBlockText, a1
	moveq #6, d0
	adda.l d2, a2
	jsr copy.copyBytes
	addq.w #6, d2
done
	move.w d2, state.NativeCliPreprocessExpansionLineLen
	moveq #0, d0
	rts
	.bend  ; emitMacroBlockStart

; Build the matching indented `.endblock` line in ExpansionLine.
emitMacroBlockEnd	.block
	lea state.NativeCliPreprocessExpansionLine, a2
	lea state.NativeCliSourceLine, a1
	clr.w d2
indentLoop
	move.b 0(a1, d2.w), d0
	cmpi.b #' ', d0
	beq.s copyIndent
	cmpi.b #9, d0
	bne.s endblock
copyIndent
	move.b d0, 0(a2, d2.w)
	addq.w #1, d2
	bra.s indentLoop
endblock
	lea MacroEndblockText, a1
	moveq #9, d0
	adda.l d2, a2
	jsr copy.copyBytes
	addq.w #9, d2
	move.w d2, state.NativeCliPreprocessExpansionLineLen
	moveq #0, d0
	rts
	.bend  ; emitMacroBlockEnd

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
	lea strings.OutputDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkUseDirective
	jsr directive_handlers.opforgeNativeCliParseOutputLine
	bra.w return

checkUseDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkCpuDirective
	jsr directive_handlers.opforgeNativeCliParseUseLine
	bra.w return

checkCpuDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.CpuMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkOrgDirective
	jsr directive_handlers.opforgeNativeCliParseCpuLine
	tst.l d0
	bne.w return
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail
	bra.w done

checkOrgDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.OrgMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith

	tst.w state.NativeCliPackagePipelineReady
	beq.w sourceDirectiveFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.s routeModuleDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.s routeModuleDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.s routeModuleDirective
	tst.w state.NativeCliModuleDepth
	bne.s recordActiveModuleSourceStatement

routeModuleDirective
	bsr.w opforgeNativeCliRouteParserModuleUseLine
	bra.s haveDirectiveKind

recordActiveModuleSourceStatement
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail
	bra.w done

sourceDirectiveFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkModuleFallback
	moveq #constants.NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	bra.s haveDirectiveKind

checkModuleFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkUseFallback
	moveq #constants.NCLI_PARSER_DIRECTIVE_MODULE, d0
	bra.s haveDirectiveKind

checkUseFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w done
	moveq #constants.NCLI_PARSER_DIRECTIVE_USE, d0

haveDirectiveKind
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_MODULE, d0
	bne.s checkEndmodule
	jsr directive_handlers.opforgeNativeCliParseModuleLine
	tst.l d0
	bne.w return
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w return

checkEndmodule
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	bne.s checkUse
	jsr directive_handlers.opforgeNativeCliParseEndmoduleLine
	tst.l d0
	bne.w return
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w return

checkUse
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_USE, d0
	bne.s recordStatement
	jsr directive_handlers.opforgeNativeCliParseUseLine
	bra.w return

recordStatement
	; `.org` is evaluated directly by opasm.  Preserve its source span rather
	; than a parser result from a preceding module transition so each module's
	; origin is evaluated from its own literal text.
	movea.l a4, a0
	move.l d7, d0
	lea strings.OrgMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s recordParsedStatement
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount

recordParsedStatement
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail

done
	moveq #0, d0
	bra.w return

conditionalLine
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail
	bra.w done

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

; Dispatch the shared tkpkg tokenizer service for the current logical line.
; Inputs: current line state is already staged in state.NativeCliSourceLine*.
; Outputs: D0 = tkpkg STATUS_*_V1.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliDispatchTokenizeLineEnvelope	.block
	bsr.w opforgeNativeCliPrepareLineServiceRequest
	bne.s done
	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w state.NativeCliLineRequestLen, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_TOKENIZE_LINE, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus

done
	rts
	.bend  ; opforgeNativeCliDispatchTokenizeLineEnvelope

; Build the tokenizer request payload: u32 line number plus source bytes.
; Build the tokenizer request payload: u32 line number plus source bytes.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen/state.NativeCliSourceLineNum describe the current logical line.
; Outputs: D0 = 0; D1 = request byte length; state.NativeCliLineRequestLen updated; buffers.lastErrorBuffer populated with the request payload.
; Clobbers: D0-D2/A1-A2/CCR.
; CCR: reflects D0 on return. This helper has no failure path with the current fixed-size request layout.
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

	.section data, kind=data
MacroBlockText
	.byte ".block", 0
MacroBlockWithLabelText
	.byte " .block", 0
MacroEndblockText
	.byte ".endblock", 0
	.endsection
	.endmodule
