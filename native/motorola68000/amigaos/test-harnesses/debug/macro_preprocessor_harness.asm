; FS-UAE runtime harness for native parameterized macro capture and dotted lookup.
; @opforge-evidence: level=D; role=focused-negative; authority=focused-contract; lifecycle=permanent

	.module macro.preprocessor.harness
	.cpu 68020

	.use opforge.cli.copy
	.use opforge.cli.constants
	.use opforge.cli.line_processor
	.use opforge.cli.module_use
	.use opforge.cli.preprocessor
	.use opforge.cli.preprocessor_definitions
	.use opforge.cli.preprocessor_expansion
	.use opforge.cli.preprocessor_substitution
	.use opforge.cli.preprocessor_invocation
	.use opforge.cli.state

HARNESS_FAIL = 20
	.section entry, kind=code
	.pub

start	.block
	move.w #2, state.NativeCliModuleCount
	lea ResolverAlphaText.l, a1
	lea state.NativeCliModuleNameTable, a2
	moveq #5, d0
	jsr copy.copyBytes
	lea ResolverBetaText.l, a1
	lea state.NativeCliModuleNameTable, a2
	adda.l #constants.TOKEN_BUFFER_CAPACITY, a2
	moveq #4, d0
	jsr copy.copyBytes
	move.w #1, state.NativeCliOrdinaryExportCount
	move.l #6, state.NativeCliOrdinaryExportNamePoolLen
	clr.w state.NativeCliOrdinaryExportOwnerTable
	move.l #1, state.NativeCliModuleOrdinaryExportHeadTable
	clr.l state.NativeCliOrdinaryExportNextTable
	clr.l state.NativeCliOrdinaryExportNameOffsetTable
	lea ResolverValueText.l, a1
	lea state.NativeCliOrdinaryExportNamePool, a2
	moveq #5, d0
	jsr copy.copyBytes
	cmpi.b #'V', state.NativeCliOrdinaryExportNamePool
	bne.w ordinaryImportExportTableFail
	move.w #1, state.NativeCliImportCount
	move.w #1, state.NativeCliImportOwnerModuleTable
	clr.w state.NativeCliImportModuleTable
	move.w #1, state.NativeCliImportSelectCount
	clr.w state.NativeCliImportSelectImportTable
	clr.w state.NativeCliImportSelectFlagsTable
	lea ResolverValueText.l, a1
	lea state.NativeCliImportSelectNameTable, a2
	moveq #5, d0
	jsr copy.copyBytes
	lea ResolverAliasText.l, a1
	lea state.NativeCliImportSelectAliasTable, a2
	moveq #1, d0
	jsr copy.copyBytes
	lea ResolverAliasText.l, a0
	moveq #1, d0
	lea ResolverBetaText.l, a1
	moveq #4, d1
	jsr module_use.opforgeNativeCliResolveImportedOrdinaryNameV1
	tst.l d1
	bne.w ordinaryImportResolverFail
	cmpi.l #11, d0
	bne.w ordinaryImportResolverLengthFail
	lea ResolverQualifiedText.l, a1
	movea.l a0, a2
	moveq #11, d0
verifyOrdinaryImportResolverLoop
	move.b (a1)+, d2
	cmp.b (a2)+, d2
	bne.w ordinaryImportResolverTextFail
	subq.l #1, d0
	bne.s verifyOrdinaryImportResolverLoop
	move.w #2, state.NativeCliImportSelectFlagsTable
	lea ResolverSecretText.l, a0
	moveq #6, d0
	lea ResolverBetaText.l, a1
	moveq #4, d1
	jsr module_use.opforgeNativeCliResolveImportedOrdinaryNameV1
	cmpi.l #1, d1
	bne.w ordinaryPrivateImportFail
	lea ResolverValueText.l, a0
	moveq #5, d0
	lea ResolverBetaText.l, a1
	moveq #4, d1
	jsr module_use.opforgeNativeCliResolveImportedOrdinaryNameV1
	tst.l d1
	bne.w ordinaryWildcardImportFail

	jsr preprocessor.opforgeNativeCliResetPreprocessorV1
	lea WrongDirectiveFirstHeaderText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #16, d0
	jsr copy.copyBytes
	move.w #16, state.NativeCliSourceLineLen
	jsr preprocessor_definitions.opforgeNativeCliCaptureMacroDefinitionLineV1
	cmpi.l #-1, d0
	bne.w wrongDirectiveFirstHeaderFail
	tst.w state.NativeCliPreprocessDefinitionCount
	bne.w wrongDirectiveFirstHeaderFail
	lea DirectiveFirstHeaderText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #24, d0
	jsr copy.copyBytes
	move.w #24, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w directiveFirstHeaderFail
	cmpi.w #1, state.NativeCliPreprocessDefinitionCount
	bne.w directiveFirstHeaderFail
	cmpi.w #23, state.NativeCliPreprocessDefinitionHeaderLen
	bne.w directiveFirstHeaderLengthFail
	lea DirectiveFirstCanonicalHeaderText.l, a1
	lea state.NativeCliPreprocessDefinitionHeader, a2
	moveq #23, d0
verifyDirectiveFirstHeaderLoop
	move.b (a1)+, d2
	cmp.b (a2)+, d2
	bne.w directiveFirstHeaderTextFail
	subq.l #1, d0
	bne.s verifyDirectiveFirstHeaderLoop
	lea DirectiveFirstBodyText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #20, d0
	jsr copy.copyBytes
	move.w #20, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w directiveFirstBodyFail
	lea EndmacroText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #9, d0
	jsr copy.copyBytes
	move.w #9, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w directiveFirstCloseFail
	lea DirectiveFirstInvocationText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #15, d0
	jsr copy.copyBytes
	move.w #15, state.NativeCliSourceLineLen
	jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1
	cmpi.l #1, d0
	bne.w directiveFirstInvocationFail
	tst.w state.NativeCliPreprocessInvocationDefinition
	bne.w directiveFirstInvocationFail
	cmpi.w #1, state.NativeCliPreprocessInvocationArgCount
	bne.w directiveFirstArgumentCountFail
	cmpi.w #1, state.NativeCliPreprocessInvocationArgLen
	bne.w directiveFirstArgumentLengthFail
	moveq #0, d0
	jsr preprocessor_substitution.opforgeNativeCliSubstituteMacroBodyLineV1
	bne.w directiveFirstSubstitutionFail
	cmpi.l #15, d1
	bne.w directiveFirstSubstitutionLengthFail
	lea DirectiveFirstExpandedText.l, a1
	lea state.NativeCliPreprocessExpansionLine, a2
	moveq #15, d0
verifyDirectiveFirstExpansionLoop
	move.b (a1)+, d2
	cmp.b (a2)+, d2
	bne.w directiveFirstSubstitutionTextFail
	subq.l #1, d0
	bne.s verifyDirectiveFirstExpansionLoop

	jsr preprocessor.opforgeNativeCliResetPreprocessorV1
	lea HeaderText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #20, d0
	jsr copy.copyBytes
	move.w #20, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w headerFail
	cmpi.w #1, state.NativeCliPreprocessDefinitionCount
	bne.w definitionCountFail

	lea BodyText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #16, d0
	jsr copy.copyBytes
	move.w #16, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w bodyCaptureFail

	lea EndmacroText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #9, d0
	jsr copy.copyBytes
	move.w #9, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w closeFail
	tst.w state.NativeCliPreprocessActiveDefinition
	bpl.w activeDefinitionFail
	lea PairHeaderText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #18, d0
	jsr copy.copyBytes
	move.w #18, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w headerFail
	lea PairBodyText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #20, d0
	jsr copy.copyBytes
	move.w #20, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w bodyCaptureFail
	moveq #0, d2
	move.w state.NativeCliSourceLineLen, d2
	cmpi.l #20, d2
	bne.w pairInputLengthFail
	lea state.NativeCliPreprocessDefinitionBodyLen, a0
	moveq #0, d2
	move.w 16(a0), d2
	cmpi.l #20, d2
	bne.w pairCapturedLengthFail
	lea EndmacroText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #9, d0
	jsr copy.copyBytes
	move.w #9, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w closeFail

	lea TextHeaderText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #15, d0
	jsr copy.copyBytes
	move.w #15, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w headerFail
	lea TextByteBodyText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #16, d0
	jsr copy.copyBytes
	move.w #16, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w bodyCaptureFail
	lea state.NativeCliPreprocessDefinitionBodyLen, a0
	moveq #0, d2
	move.w 16(a0), d2
	cmpi.l #20, d2
	bne.w pairOverwrittenByTextFail
	lea TextWordBodyText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #16, d0
	jsr copy.copyBytes
	move.w #16, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w bodyCaptureFail
	lea EndmacroText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #9, d0
	jsr copy.copyBytes
	move.w #9, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w closeFail
	lea LocalHeaderText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #12, d0
	jsr copy.copyBytes
	move.w #12, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w headerFail
	cmpi.w #3, state.NativeCliPreprocessActiveDefinition
	bne.w localDefinitionFail
	lea LocalBodyText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #16, d0
	jsr copy.copyBytes
	move.w #16, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w bodyCaptureFail
	lea state.NativeCliPreprocessDefinitionBodyLen, a0
	moveq #0, d2
	move.w 16(a0), d2
	cmpi.l #20, d2
	bne.w pairOverwrittenByLocalBodyFail
	lea EndmacroText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #9, d0
	jsr copy.copyBytes
	move.w #9, state.NativeCliSourceLineLen
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.w closeFail
	cmpi.w #4, state.NativeCliPreprocessDefinitionCount
	bne.w definitionCountFail
	lea state.NativeCliPreprocessDefinitionBodyLen, a0
	moveq #0, d2
	move.w 16(a0), d2
	cmpi.l #20, d2
	bne.w pairOverwrittenBeforeInvocationFail
	lea BareLocalInvocationText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #6, d0
	jsr copy.copyBytes
	move.w #6, state.NativeCliSourceLineLen
	jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1
	cmpi.l #1, d0
	bne.w bareLocalInvocationFail
	cmpi.w #3, state.NativeCliPreprocessInvocationDefinition
	bne.w bareLocalInvocationFail
	jsr preprocessor.opforgeNativeCliEndMacroInvocationFrameV1
	bne.w bareLocalInvocationFail
	lea LocalInvocationText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #10, d0
	jsr copy.copyBytes
	move.w #10, state.NativeCliSourceLineLen
	jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1
	cmpi.l #1, d0
	beq.s localInvocationStatusOk
	tst.l d0
	bmi.w localInvocationMalformedFail
	bra.w localInvocationStatusFail
localInvocationStatusOk
	cmpi.w #3, state.NativeCliPreprocessInvocationDefinition
	bne.w localInvocationDefinitionFail
	cmpi.w #3, state.NativeCliPreprocessInvocationLabelLen
	bne.w localInvocationLabelFail
	jsr preprocessor.opforgeNativeCliEndMacroInvocationFrameV1
	bne.w localInvocationDefinitionFail
	lea PairInvocationText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #8, d0
	jsr copy.copyBytes
	move.w #8, state.NativeCliSourceLineLen
	jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1
	cmpi.l #1, d0
	bne.w pairInvocationFail
	cmpi.w #1, state.NativeCliPreprocessInvocationDefinition
	bne.w pairDefinitionFail
	cmpi.w #2, state.NativeCliPreprocessInvocationArgCount
	bne.w pairDefaultBindingFail
	moveq #0, d2
	lea state.NativeCliPreprocessInvocationArgLen, a0
	move.w 2(a0), d2
	cmpi.l #1, d2
	bne.w pairDefaultBindingFail
	lea state.NativeCliPreprocessDefinitionBodyCount, a0
	moveq #0, d2
	move.w 2(a0), d2
	cmpi.l #1, d2
	bne.w pairBodyCountFail
	lea state.NativeCliPreprocessDefinitionBodyLen, a0
	moveq #0, d2
	move.w 16(a0), d2
	cmpi.l #20, d2
	bne.w pairBodyLengthFail
	lea state.NativeCliPreprocessDefinitionBody, a0
	adda.l #(constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY * constants.SOURCE_LINE_BUFFER_CAPACITY), a0
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	bne.w pairBodyPayloadFail
	moveq #0, d0
	jsr preprocessor_substitution.opforgeNativeCliSubstituteMacroBodyLineV1
	tst.l d0
	bne.w pairSubstitutionFail
	cmpi.l #18, d1
	bne.w pairSubstitutionLengthFail
	lea ExpandedPairText.l, a1
	lea state.NativeCliPreprocessExpansionLine, a2
	moveq #18, d0
verifyPairSubstitutionLoop
	move.b (a1)+, d2
	cmp.b (a2)+, d2
	bne.w pairSubstitutionTextFail
	subq.l #1, d0
	bne.s verifyPairSubstitutionLoop
	jsr preprocessor.opforgeNativeCliEndMacroInvocationFrameV1
	bne.w pairInvocationFail

	lea InvocationText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #15, d0
	jsr copy.copyBytes
	move.w #15, state.NativeCliSourceLineLen
	jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1
	tst.l d0
	bmi.w invocationMalformed
	cmpi.l #1, d0
	bne.w invocationParseFail
	cmpi.w #3, state.NativeCliPreprocessInvocationArgLen
	bne.w invocationArgumentLengthFail
	moveq #0, d0
	jsr preprocessor_substitution.opforgeNativeCliSubstituteMacroBodyLineV1
	tst.l d0
	bne.w substitutionFail
	cmpi.l #15, d1
	bne.w substitutionLengthFail
	lea ExpandedCopyText.l, a1
	lea state.NativeCliPreprocessExpansionLine, a2
	moveq #15, d0
verifySubstitutionLoop
	move.b (a1)+, d2
	cmp.b (a2)+, d2
	bne.w substitutionTextFail
	subq.l #1, d0
	bne.s verifySubstitutionLoop
	; A substituted nested invocation must occupy its own bounded frame, then
	; restore the COPY caller and its logical source line when popped.
	lea NestedInvocationText.l, a0
	moveq #7, d0
	jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1
	bne.w nestedInvocationAcceptedFail
	jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1
	cmpi.l #1, d0
	bne.w nestedInvocationAcceptedFail
	cmpi.w #2, state.NativeCliPreprocessInvocationDepth
	bne.w nestedFrameCorruptFail
	cmpi.w #1, state.NativeCliPreprocessInvocationDefinition
	bne.w nestedFrameCorruptFail
	jsr preprocessor.opforgeNativeCliEndMacroInvocationFrameV1
	bne.w nestedFrameCorruptFail
	cmpi.w #0, state.NativeCliPreprocessInvocationDefinition
	bne.w nestedFrameCorruptFail
	cmpi.w #3, state.NativeCliPreprocessInvocationArgLen
	bne.w nestedFrameCorruptFail
	jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1
	bne.w nestedCallerRestoreFail
	cmpi.w #15, state.NativeCliSourceLineLen
	bne.w nestedCallerRestoreFail
	lea InvocationText.l, a1
	lea state.NativeCliSourceLine, a2
	moveq #15, d0
verifyNestedCallerRestoreLoop
	move.b (a1)+, d2
	cmp.b (a2)+, d2
	bne.w nestedCallerRestoreFail
	subq.l #1, d0
	bne.s verifyNestedCallerRestoreLoop
	jsr preprocessor.opforgeNativeCliEndMacroInvocationFrameV1
	bne.w nestedFrameCorruptFail
	moveq #0, d0
	rts
fail
	moveq #HARNESS_FAIL, d0
	rts
headerFail
	moveq #21, d0
	rts
bodyCaptureFail
	moveq #27, d0
	rts
invocationParseFail
	moveq #29, d0
	rts
invocationMalformed
	moveq #34, d0
	rts
invocationArgumentLengthFail
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationArgLen, d0
	addi.l #60, d0
	rts
substitutionFail
	moveq #31, d0
	rts
substitutionTextFail
	moveq #15, d1
	sub.l d0, d1
	addi.l #36, d1
	move.l d1, d0
	rts
substitutionLengthFail
	move.l d1, d0
	addi.l #36, d0
	rts
nestedInvocationAcceptedFail
	moveq #47, d0
	rts
nestedFrameCorruptFail
	moveq #48, d0
	rts
nestedCallerRestoreFail
	moveq #51, d0
	rts
closeFail
	moveq #22, d0
	rts
definitionCountFail
	moveq #24, d0
	rts
activeDefinitionFail
	moveq #25, d0
	rts
pairInvocationFail
	moveq #49, d0
	rts
pairSubstitutionFail
	moveq #50, d0
	rts
pairDefinitionFail
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationDefinition, d0
	addi.l #80, d0
	rts
pairDefaultBindingFail
	moveq #63, d0
	rts
pairBodyLengthFail
	move.l d2, d0
	addi.l #90, d0
	rts
pairBodyCountFail
	move.l d2, d0
	addi.l #110, d0
	rts
pairBodyPayloadFail
	move.l d2, d0
	addi.l #120, d0
	rts
pairInputLengthFail
	move.l d2, d0
	addi.l #140, d0
	rts
pairCapturedLengthFail
	move.l d2, d0
	addi.l #160, d0
	rts
pairOverwrittenByTextFail
	move.l d2, d0
	addi.l #180, d0
	rts
localDefinitionFail
	moveq #0, d0
	move.w state.NativeCliPreprocessActiveDefinition, d0
	addi.l #200, d0
	rts
localInvocationStatusFail
	moveq #62, d0
	rts
localInvocationMalformedFail
	moveq #65, d0
	rts
bareLocalInvocationFail
	moveq #61, d0
	rts
localInvocationDefinitionFail
	moveq #63, d0
	rts
localInvocationLabelFail
	moveq #64, d0
	rts
pairOverwrittenBeforeInvocationFail
	move.l d2, d0
	addi.l #220, d0
	rts
pairOverwrittenByLocalBodyFail
	moveq #61, d0
	rts
pairSubstitutionLengthFail
	move.l d1, d0
	addi.l #70, d0
	rts
pairSubstitutionTextFail
	moveq #18, d1
	sub.l d0, d1
	addi.l #100, d1
	move.l d1, d0
	rts
directiveFirstHeaderFail
	moveq #66, d0
	rts
ordinaryImportResolverFail
	moveq #62, d0
	rts
ordinaryImportExportTableFail
	moveq #61, d0
	rts
ordinaryImportResolverLengthFail
	moveq #63, d0
	rts
ordinaryImportResolverTextFail
	moveq #64, d0
	rts
ordinaryPrivateImportFail
	moveq #74, d0
	rts
ordinaryWildcardImportFail
	moveq #75, d0
	rts
wrongDirectiveFirstHeaderFail
	moveq #65, d0
	rts
directiveFirstHeaderLengthFail
	moveq #67, d0
	rts
directiveFirstHeaderTextFail
	moveq #68, d0
	rts
directiveFirstBodyFail
	moveq #69, d0
	rts
directiveFirstCloseFail
	moveq #70, d0
	rts
directiveFirstInvocationFail
	moveq #71, d0
	rts
directiveFirstArgumentCountFail
	moveq #72, d0
	rts
directiveFirstArgumentLengthFail
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationArgLen, d0
	addi.l #100, d0
	rts
directiveFirstSubstitutionFail
	moveq #73, d0
	rts
directiveFirstSubstitutionLengthFail
	move.l d1, d0
	addi.l #74, d0
	rts
directiveFirstSubstitutionTextFail
	moveq #15, d1
	sub.l d0, d1
	addi.l #90, d1
	move.l d1, d0
	rts
	.bend  ; start
	.endsection
	.section data, kind=data
HeaderText
	.byte "COPY .macro src, dst"
BodyText
	.byte "        lda .src"
PairHeaderText
	.byte "PAIR .macro a, b=2"
PairBodyText
	.byte "        .byte .a, .b"
TextHeaderText
	.byte "TEXT .macro msg"
TextByteBodyText
	.byte "        .byte @1"
TextWordBodyText
	.byte "        .word .@"
LocalHeaderText
	.byte "LOCAL .macro"
LocalBodyText
	.byte "local   .const 9"
EndmacroText
	.byte ".endmacro"
BareLocalInvocationText
	.byte ".LOCAL"
LocalInvocationText
	.byte "foo .LOCAL"
InvocationText
	.byte 9, ".COPY $12, $34"
PairInvocationText
	.byte 9, ".PAIR 1"
NestedInvocationText
	.byte ".PAIR 1"
ExpandedCopyText
	.byte "        lda $12"
ExpandedPairText
	.byte "        .byte 1, 2"
DirectiveFirstHeaderText
	.byte ".macro FILL.PART$(value)"
DirectiveFirstCanonicalHeaderText
	.byte "FILL.PART$ .macro value"
DirectiveFirstBodyText
	.byte "        .byte .value"
DirectiveFirstInvocationText
	.byte 9, ".FILL.PART$(3)"
DirectiveFirstExpandedText
	.byte "        .byte 3"
WrongDirectiveFirstHeaderText
	.byte ".foo .macro NAME"
ResolverAlphaText
	.byte "alpha", 0
ResolverBetaText
	.byte "beta", 0
ResolverValueText
	.byte "VALUE", 0
ResolverAliasText
	.byte "V", 0
ResolverQualifiedText
	.byte "alpha.VALUE"
ResolverSecretText
	.byte "SECRET", 0
	.endsection
	.output "build/macro_preprocessor_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
