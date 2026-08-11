; Bounded native macro-definition capture owner.

	.module opforge.cli.preprocessor_definitions
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.line_text
	.use opforge.cli.state
	.use opforge.cli.preprocessor_scan
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

	.section code, kind=code
	.pub

; Consume one `.macro` or `.segment` definition line before tokenizer dispatch.
; Inputs: state.NativeCliSourceLine contains the logical source line.
; Outputs: D0 = 0 passthrough, 1 consumed, -1 malformed/capacity failure.
; Clobbers: D0-D4/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliCaptureMacroDefinitionLineV1	.block
	tst.w state.NativeCliPreprocessActiveDefinition
	bmi.w checkOpen
	move.w state.NativeCliPreprocessActiveDefinition, d2
	lea state.NativeCliPreprocessDefinitionKind, a2
	move.b 0(a2, d2.w), d2
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT, d2
	beq.w checkSegmentClose
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT, d2
	beq.w checkStatementClose
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr preprocessor_scan.lineStartsWithEndmacroDirective
	bne.w close
	bra.s rejectWrongClose
checkSegmentClose
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndsegmentText.l, a1
	moveq #11, d1
	jsr preprocessor_scan.lineStartsWithDirective
	bne.w close
	bra.s rejectWrongClose
checkStatementClose
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndstatementText.l, a1
	moveq #13, d1
	jsr preprocessor_scan.lineStartsWithDirective
	bne.w close
rejectWrongClose
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr preprocessor_scan.lineStartsWithEndmacroDirective
	bne.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndsegmentText.l, a1
	moveq #11, d1
	jsr preprocessor_scan.lineStartsWithDirective
	bne.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr preprocessor_scan.lineContainsMacroDirective
	beq.s captureNoNestedMacro
	bra.w fail
captureNoNestedMacro
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea SegmentText.l, a1
	moveq #8, d1
	jsr preprocessor_scan.lineContainsDirective
	bne.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea StatementText.l, a1
	moveq #10, d1
	jsr preprocessor_scan.lineContainsDirective
	bne.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndstatementText.l, a1
	moveq #13, d1
	jsr preprocessor_scan.lineStartsWithDirective
	beq.s captureNoEndstatement
	bra.w fail
captureNoEndstatement
	bsr.w appendBodyLine
	tst.l d0
	beq.s captured
	bra.w fail
captured
	moveq #1, d0
	rts
close
	moveq #-1, d0
	move.w d0, state.NativeCliPreprocessActiveDefinition
	moveq #1, d0
	rts
checkOpen
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr preprocessor_scan.lineStartsWithEndmacroDirective
	beq.s noUnexpectedEnd
	bra.w fail
noUnexpectedEnd
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndsegmentText.l, a1
	moveq #11, d1
	jsr preprocessor_scan.lineStartsWithDirective
	bne.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndstatementText.l, a1
	moveq #13, d1
	jsr preprocessor_scan.lineStartsWithDirective
	bne.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr preprocessor_scan.lineContainsMacroDirective
	bne.s openMacro
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea SegmentText.l, a1
	moveq #8, d1
	jsr preprocessor_scan.lineContainsDirective
	bne.s openSegment
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea StatementText.l, a1
	moveq #10, d1
	jsr preprocessor_scan.lineContainsDirective
	beq.w pass
	moveq #constants.NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT, d4
	bra.s validateOpen
openSegment
	moveq #constants.NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT, d4
	bra.s validateOpen
openMacro
	moveq #constants.NATIVE_PREPROCESS_DEFINITION_KIND_MACRO, d4
validateOpen
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT, d4
	beq.s validateStatement
	move.l d4, -(sp)
	lea MacroText.l, a1
	moveq #6, d1
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT, d4
	bne.s validateMacroOrSegment
	lea SegmentText.l, a1
	moveq #8, d1
validateMacroOrSegment
	jsr preprocessor_scan.macroHeaderHasName
	move.l (sp)+, d4
	tst.l d0
	beq.w fail
	bra.s validated
validateStatement
	move.l d4, -(sp)
	jsr preprocessor_scan.statementHeaderHasKeyword
	move.l (sp)+, d4
	tst.l d0
	beq.w fail
validated
	moveq #0, d2
	move.w state.NativeCliPreprocessDefinitionCount, d2
	cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d2
	bcc.w fail
	lea state.NativeCliPreprocessDefinitionHeader, a2
	mulu #constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY, d2
	adda.l d2, a2
	bsr.w storeDefinitionHeader
	tst.l d0
	bne.w fail
	moveq #0, d2
	move.w state.NativeCliPreprocessDefinitionCount, d2
	add.l d2, d2
	lea state.NativeCliPreprocessDefinitionHeaderLen, a2
	move.w d3, 0(a2, d2.l)
	move.w state.NativeCliPreprocessDefinitionCount, d0
	move.w d0, state.NativeCliPreprocessActiveDefinition
	lea state.NativeCliPreprocessDefinitionKind, a2
	move.b d4, 0(a2, d0.w)
	moveq #-1, d1
	tst.w state.NativeCliModuleDepth
	beq.s storeOwner
	moveq #0, d1
	move.w state.NativeCliCurrentModuleId, d1
storeOwner
	move.l d0, d2
	add.l d2, d2
	lea state.NativeCliPreprocessDefinitionOwner, a2
	move.w d1, 0(a2, d2.l)
	moveq #0, d1
	move.w state.NativeCliPreprocessCurrentVisibility, d1
	lea state.NativeCliPreprocessDefinitionVisibility, a2
	move.b d1, 0(a2, d0.w)
	addq.w #1, state.NativeCliPreprocessDefinitionCount
.ifdef OPFORGE_DEBUG_CONTRACTS
	move.w ccr, -(sp)
	movem.l d1-d6/a0, -(sp)
	moveq #0, d1
	moveq #6, d2
	moveq #0, d3
	move.w state.NativeCliPreprocessActiveDefinition, d3
	moveq #0, d4
	move.l d3, -(sp)
	move.l d3, d4
	move.w state.NativeCliPreprocessDefinitionHeaderLen, d4
	move.l (sp)+, d3
	moveq #0, d5
	moveq #0, d6
	move.w state.NativeCliPreprocessDefinitionCount, d6
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_MACRO_DEFINITION
	movem.l (sp)+, d1-d6/a0
	move.w (sp)+, ccr
.endif
	moveq #1, d0
	rts
pass
	moveq #0, d0
	rts
fail
	moveq #-1, d0
	rts
	.bend  ; opforgeNativeCliCaptureMacroDefinitionLineV1

; Verify that no macro or segment definition remained open at end of the source stream.
; Inputs: none.
; Outputs: D0 = 0 when complete, 1 when a matching end directive is missing.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliFinishMacroDefinitionsV1	.block
	tst.w state.NativeCliPreprocessActiveDefinition
	bmi.s complete
	moveq #1, d0
	rts
complete
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliFinishMacroDefinitionsV1

; Store one validated definition header, canonicalizing directive-first syntax.
; Inputs: A2 = destination slot; D4.B = definition kind; source line in state.
; Outputs: D0 = 0 success/1 malformed or overflow; D3 = stored byte length.
; Clobbers: D0-D3/A0-A2/CCR.
; CCR: reflects D0 on return.
storeDefinitionHeader	.block
	movem.l d5-d7/a3, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	movea.l a2, a3
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w fail
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_STATEMENT, d4
	beq.s copyOriginalHeader
	cmpi.b #'.', (a0)
	beq.s directiveFirst
copyOriginalHeader
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	cmpi.l #constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY, d0
	bhi.w fail
	move.l d0, d3
	lea state.NativeCliSourceLine, a1
	movea.l a3, a2
	jsr copy.copyBytes
	bra.w success

directiveFirst
	moveq #6, d5
	lea MacroText.l, a1
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT, d4
	bne.s validateDirective
	moveq #8, d5
	lea SegmentText.l, a1
validateDirective
	cmp.l d5, d0
	bls.w fail
	movem.l d4/a3, -(sp)
	move.l d5, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	movem.l (sp)+, d4/a3
	tst.l d0
	beq.w fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	adda.l d5, a0
	sub.l d5, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w fail
	move.b (a0), d1
	bsr.w isHeaderIdentifierStart
	tst.l d2
	beq.w fail
	movea.l a3, a2
	moveq #0, d3

copyName
	tst.l d0
	beq.s nameDone
	move.b (a0), d1
	bsr.w isHeaderIdentifierContinue
	tst.l d2
	beq.s nameDone
	bsr.w appendHeaderByte
	tst.l d2
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bra.s copyName

nameDone
	moveq #' ', d1
	bsr.w appendHeaderByte
	tst.l d2
	bne.w fail
	lea MacroText.l, a1
	moveq #6, d7
	cmpi.b #constants.NATIVE_PREPROCESS_DEFINITION_KIND_SEGMENT, d4
	bne.s copyDirective
	lea SegmentText.l, a1
	moveq #8, d7
copyDirective
	moveq #0, d6
copyDirectiveByte
	cmp.l d7, d6
	bcc.s directiveDone
	move.b 0(a1, d6.l), d1
	bsr.w appendHeaderByte
	tst.l d2
	bne.w fail
	addq.l #1, d6
	bra.s copyDirectiveByte

directiveDone
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w success
	cmpi.b #';', (a0)
	beq.w success
	moveq #' ', d1
	bsr.w appendHeaderByte
	tst.l d2
	bne.w fail
	cmpi.b #'(', (a0)
	beq.s parenthesized

copyUnparenthesized
	tst.l d0
	beq.w trimAndSucceed
	move.b (a0)+, d1
	subq.l #1, d0
	cmpi.b #';', d1
	beq.w trimAndSucceed
	bsr.w appendHeaderByte
	tst.l d2
	bne.w fail
	bra.s copyUnparenthesized

parenthesized
	addq.l #1, a0
	subq.l #1, d0
	moveq #1, d5
	moveq #0, d6

parenLoop
	tst.l d0
	beq.w fail
	move.b (a0)+, d1
	subq.l #1, d0
	tst.b d6
	beq.s outsideQuote
	cmpi.b #'\\', d1
	beq.s escapedQuoteByte
	cmp.b d6, d1
	bne.s copyParenByte
	moveq #0, d6
	bra.s copyParenByte

escapedQuoteByte
	bsr.w appendHeaderByte
	tst.l d2
	bne.w fail
	tst.l d0
	beq.w fail
	move.b (a0)+, d1
	subq.l #1, d0
	bra.s copyParenByte

outsideQuote
	cmpi.b #'\'', d1
	beq.s enterQuote
	cmpi.b #'"', d1
	beq.s enterQuote
	cmpi.b #'(', d1
	beq.s nestedOpen
	cmpi.b #')', d1
	beq.s nestedClose
	bra.s copyParenByte

enterQuote
	move.b d1, d6
	bra.s copyParenByte

nestedOpen
	addq.l #1, d5
	bra.s copyParenByte

nestedClose
	subq.l #1, d5
	beq.s parenthesizedDone

copyParenByte
	bsr.w appendHeaderByte
	tst.l d2
	bne.w fail
	bra.w parenLoop

parenthesizedDone
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s trimAndSucceed
	cmpi.b #';', (a0)
	bne.s fail

trimAndSucceed
	cmpi.l #1, d3
	bls.s success
	move.b -1(a2), d1
	cmpi.b #' ', d1
	beq.s trimOne
	cmpi.b #9, d1
	bne.s success
trimOne
	subq.l #1, a2
	subq.l #1, d3
	bra.s trimAndSucceed

success
	moveq #0, d0
	movem.l (sp)+, d5-d7/a3
	rts
fail
	moveq #1, d0
	movem.l (sp)+, d5-d7/a3
	rts
	.bend  ; storeDefinitionHeader

; Append D1.B to the current header destination when capacity permits.
; Inputs: A2 = destination cursor; D1.B = byte; D3 = current length.
; Outputs: D2 = 0 success/1 overflow; A2/D3 advanced on success.
; Clobbers: D2/CCR.
appendHeaderByte	.block
	cmpi.l #constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY, d3
	bcc.s full
	move.b d1, (a2)+
	addq.l #1, d3
	moveq #0, d2
	rts
full
	moveq #1, d2
	rts
	.bend  ; appendHeaderByte

; Classify one definition-name first byte.
; Inputs: D1.B = byte. Outputs: D2 = 1 valid/0 invalid.
; Clobbers: D2/CCR.
isHeaderIdentifierStart	.block
	moveq #0, d2
	cmpi.b #'_', d1
	beq.s yes
	cmpi.b #'A', d1
	bcs.s no
	cmpi.b #'Z', d1
	bls.s yes
	cmpi.b #'a', d1
	bcs.s no
	cmpi.b #'z', d1
	bhi.s no
yes
	moveq #1, d2
no
	rts
	.bend  ; isHeaderIdentifierStart

; Classify one definition-name continuation byte.
; Inputs: D1.B = byte. Outputs: D2 = 1 valid/0 invalid.
; Clobbers: D2/CCR.
isHeaderIdentifierContinue	.block
	bsr.s isHeaderIdentifierStart
	tst.l d2
	bne.s done
	cmpi.b #'.', d1
	beq.s yes
	cmpi.b #'$', d1
	beq.s yes
	cmpi.b #'0', d1
	bcs.s done
	cmpi.b #'9', d1
	bhi.s done
yes
	moveq #1, d2
done
	rts
	.bend  ; isHeaderIdentifierContinue

appendBodyLine	.block
	moveq #0, d2
	move.w state.NativeCliPreprocessActiveDefinition, d2
	lea state.NativeCliPreprocessDefinitionBodyCount, a2
	add.l d2, d2
	adda.l d2, a2
	moveq #0, d3
	move.w (a2), d3
	cmpi.w #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d3
	bcc.s fail
	moveq #0, d2
	move.w state.NativeCliPreprocessActiveDefinition, d2
	mulu #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d2
	add.l d3, d2
	mulu #constants.NATIVE_PREPROCESS_BODY_LINE_TEXT_CAPACITY, d2
	lea state.NativeCliPreprocessDefinitionBody, a2
	adda.l d2, a2
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	move.l d0, d4
	movea.l a0, a1
	jsr copy.copyBytes
	moveq #0, d2
	move.w state.NativeCliPreprocessActiveDefinition, d2
	mulu #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d2
	add.l d3, d2
	add.l d2, d2
	lea state.NativeCliPreprocessDefinitionBodyLen, a2
	moveq #0, d4
	move.w state.NativeCliSourceLineLen, d4
	adda.l d2, a2
	move.w d4, (a2)
	addq.w #1, d3
	move.w state.NativeCliPreprocessActiveDefinition, d2
	lea state.NativeCliPreprocessDefinitionBodyCount, a2
	add.l d2, d2
	move.w d3, 0(a2, d2.l)
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; appendBodyLine

	.endsection
	.section data, kind=data
EndsegmentText
	.byte ".endsegment", 0
MacroText
	.byte ".macro", 0
SegmentText
	.byte ".segment", 0
StatementText
	.byte ".statement", 0
EndstatementText
	.byte ".endstatement", 0
	.endsection
	.endmodule
