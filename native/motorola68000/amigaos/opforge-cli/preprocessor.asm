; Bounded source-preprocessor state for native macro and statement expansion.

	.module opforge.cli.preprocessor
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.state
	.use opforge.cli.line_text
	.use opforge.cli.strings
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

	.section code, kind=code
	.pub

; Clear all per-run preprocessor state before the frontend reads source.
; Inputs: none.
; Outputs: D0 = 0.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliResetPreprocessorV1	.block
	lea state.NativeCliPreprocessStateStart, a0
	move.l #state.NATIVE_CLI_PREPROCESS_STATE_BYTES, d0
	jsr copy.clearBytes
	move.w #-1, state.NativeCliPreprocessActiveDefinition
	move.w #-1, state.NativeCliPreprocessInvocationDefinition
	move.w #constants.NATIVE_PREPROCESS_VISIBILITY_PRIVATE, state.NativeCliPreprocessCurrentVisibility
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliResetPreprocessorV1

; Record one repeatable Rust-compatible `-D/--define NAME[=VALUE]` token.
; Inputs: A0 = NUL-terminated token (bounded by TOKEN_BUFFER_CAPACITY).
; Outputs: D0 = 0 on success, 1 on malformed token or fixed-table overflow.
opforgeNativeCliRecordCommandLineDefineV1	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a4
	tst.b (a4)
	beq.w fail
	cmpi.b #'=', (a4)
	beq.w fail
	clr.w d4
findExisting
	cmp.w state.NativeCliPreprocessCliDefineCount, d4
	bhs.s append
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea state.NativeCliPreprocessCliDefines, a1
	adda.l d0, a1
	movea.l a4, a0
	bsr.w defineNamesEqual
	tst.l d0
	bne.s success
	addq.w #1, d4
	bra.s findExisting

append
	cmpi.w #constants.NATIVE_PREPROCESS_CLI_DEFINE_CAPACITY, d4
	bhs.s fail
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea state.NativeCliPreprocessCliDefines, a1
	adda.l d0, a1
	movea.l a4, a0
	moveq #constants.TOKEN_BUFFER_CAPACITY - 1, d1
copy
	move.b (a0)+, d2
	move.b d2, (a1)+
	beq.s copied
	dbra d1, copy
	bra.s fail
copied
	addq.w #1, state.NativeCliPreprocessCliDefineCount
success
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d7/a0-a4
	rts
	.bend  ; opforgeNativeCliRecordCommandLineDefineV1

; Consume conditional directives before any macro/include/statement routing.
; Outputs: D0 = 0 ordinary active line, 1 consumed directive,
;          2 ordinary inactive line, -1 malformed/over-capacity conditional.
opforgeNativeCliRouteConditionalLineV1	.block
	movem.l d1-d7/a0-a4, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w ordinary
	movea.l a0, a4
	move.l d0, d7

	lea strings.IfdefDirectiveText, a1
	moveq #6, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w ifdef
	movea.l a4, a0
	move.l d7, d0
	lea strings.IfndefDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w ifndef
	movea.l a4, a0
	move.l d7, d0
	lea strings.ElseifDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w elseif
	movea.l a4, a0
	move.l d7, d0
	lea strings.ElseDirectiveText, a1
	moveq #5, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w else
	movea.l a4, a0
	move.l d7, d0
	lea strings.EndifDirectiveText, a1
	moveq #6, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w endif
	bra.w ordinary

ifdef
	moveq #6, d6
	bsr.w stageConditionalName
	tst.l d0
	bne.w malformed
	tst.b state.NativeCliPreprocessConditionalName
	beq.w malformed
	lea state.NativeCliPreprocessConditionalName, a0
	bsr.w isCommandLineDefine
	move.l d0, d5
	bsr.w pushConditionalFrame
	tst.l d0
	bne.w malformed
	bra.w consumed

ifndef
	moveq #7, d6
	bsr.w stageConditionalName
	tst.l d0
	bne.w malformed
	tst.b state.NativeCliPreprocessConditionalName
	beq.w malformed
	lea state.NativeCliPreprocessConditionalName, a0
	bsr.w isCommandLineDefine
	move.l d0, d5
	eori.b #1, d5
	andi.l #1, d5
	bsr.w pushConditionalFrame
	tst.l d0
	bne.w malformed
	bra.w consumed

elseif
	moveq #7, d6
	bra.s routeElse
else
	moveq #5, d6
routeElse
	tst.w state.NativeCliPreprocessConditionalDepth
	beq.w ordinary
	bsr.w stageConditionalName
	tst.l d0
	bne.w malformed
	bsr.w updateConditionalElse
	tst.l d0
	bne.w malformed
	bra.s consumed

endif
	tst.w state.NativeCliPreprocessConditionalDepth
	beq.w ordinary
	subq.w #1, state.NativeCliPreprocessConditionalDepth
consumed
	moveq #1, d0
	bra.s return

ordinary
	moveq #0, d1
	move.w state.NativeCliPreprocessConditionalDepth, d1
	beq.s active
	subq.w #1, d1
	lea state.NativeCliPreprocessConditionalActive, a0
	tst.b 0(a0, d1.w)
	bne.s active
	moveq #2, d0
	bra.s return
active
	moveq #0, d0
	bra.s return
malformed
	moveq #-1, d0
return
	movem.l (sp)+, d1-d7/a0-a4
	rts
	.bend  ; opforgeNativeCliRouteConditionalLineV1

; Reject a source whose conditional stack did not close before frontend EOF.
opforgeNativeCliFinishConditionalsV1	.block
	moveq #0, d0
	tst.w state.NativeCliPreprocessConditionalDepth
	beq.s done
	moveq #1, d0
done
	rts
	.bend  ; opforgeNativeCliFinishConditionalsV1

; Consume and track Rust-compatible `.pub`/`.priv` before definition capture.
; Inputs: current source line and module state.
; Outputs: D0 = 1 when a visibility directive was consumed, 0 otherwise.
opforgeNativeCliTrackVisibilityV1	.block
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s noDirective
	lea PubText.l, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkPrivate
	move.w #constants.NATIVE_PREPROCESS_VISIBILITY_PUBLIC, d0
	bra.s update
checkPrivate
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s noDirective
	lea PrivText.l, a1
	moveq #5, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s noDirective
	move.w #constants.NATIVE_PREPROCESS_VISIBILITY_PRIVATE, d0
update
	move.w d0, state.NativeCliPreprocessCurrentVisibility
	tst.w state.NativeCliModuleDepth
	beq.s consumed
	moveq #0, d1
	move.w state.NativeCliCurrentModuleId, d1
	add.w d1, d1
	lea state.NativeCliModuleVisibilityTable, a0
	move.w d0, 0(a0, d1.l)
consumed
	moveq #1, d0
	rts
noDirective
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliTrackVisibilityV1

; Reserve one bounded macro invocation frame for a captured definition.  A
; nested invocation suspends the complete caller frame before binding its own
; arguments, matching Rust's recursive expansion order.
; Inputs: D0 = zero-based definition index.
; Outputs: D0 = 0 on success, 1 when the index or bounded depth is out of range.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliBeginMacroInvocationFrameV1	.block
	cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d0
	bcc.s fail
	cmpi.w #constants.NATIVE_PREPROCESS_INVOCATION_DEPTH_LIMIT, state.NativeCliPreprocessInvocationDepth
	bcc.s fail
	movem.l d1-d7/a0-a2, -(sp)
	move.l d0, d6
	tst.w state.NativeCliPreprocessInvocationDefinition
	bmi.s install
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationDepth, d0
	subq.w #1, d0
	bsr.w saveInvocationFrame
install
	move.w d6, state.NativeCliPreprocessInvocationDefinition
	clr.w state.NativeCliPreprocessInvocationArgCount
	clr.w state.NativeCliPreprocessInvocationBodyIndex
	clr.w state.NativeCliPreprocessInvocationFullArgsLen
	clr.w state.NativeCliPreprocessInvocationLabelLen
	addq.w #1, state.NativeCliPreprocessInvocationDepth
	movem.l (sp)+, d1-d7/a0-a2
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliBeginMacroInvocationFrameV1

; Close the current invocation and restore its suspended caller, if any.
; Outputs: D0 = 0 on success, 1 with no active frame.
opforgeNativeCliEndMacroInvocationFrameV1	.block
	tst.w state.NativeCliPreprocessInvocationDepth
	beq.s fail
	movem.l d1-d7/a0-a2, -(sp)
	subq.w #1, state.NativeCliPreprocessInvocationDepth
	tst.w state.NativeCliPreprocessInvocationDepth
	beq.s clear
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationDepth, d0
	subq.w #1, d0
	bsr.w restoreInvocationFrame
	bra.s success
clear
	move.w #-1, state.NativeCliPreprocessInvocationDefinition
	clr.w state.NativeCliPreprocessInvocationArgCount
	clr.w state.NativeCliPreprocessInvocationBodyIndex
	clr.w state.NativeCliPreprocessInvocationFullArgsLen
	clr.w state.NativeCliPreprocessInvocationLabelLen
success
	movem.l (sp)+, d1-d7/a0-a2
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliEndMacroInvocationFrameV1

	.priv

; Save the complete current invocation in suspended slot D0.
saveInvocationFrame	.block
	move.l d0, d7
	add.l d0, d0
	lea state.NativeCliPreprocessSavedInvocationDefinition, a0
	move.w state.NativeCliPreprocessInvocationDefinition, d1
	move.w d1, 0(a0, d0.l)
	lea state.NativeCliPreprocessSavedInvocationArgCount, a0
	move.w state.NativeCliPreprocessInvocationArgCount, d1
	move.w d1, 0(a0, d0.l)
	lea state.NativeCliPreprocessSavedInvocationBodyIndex, a0
	move.w state.NativeCliPreprocessInvocationBodyIndex, d1
	move.w d1, 0(a0, d0.l)
	lea state.NativeCliPreprocessSavedInvocationFullArgsLen, a0
	move.w state.NativeCliPreprocessInvocationFullArgsLen, d1
	move.w d1, 0(a0, d0.l)
	lea state.NativeCliPreprocessSavedInvocationLabelLen, a0
	move.w state.NativeCliPreprocessInvocationLabelLen, d1
	move.w d1, 0(a0, d0.l)
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_ARG_LENGTH_BYTES, d0
	lea state.NativeCliPreprocessInvocationArgLen, a1
	lea state.NativeCliPreprocessSavedInvocationArgLen, a2
	adda.l d0, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_ARG_LENGTH_BYTES, d0
	jsr copy.copyBytes
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_ARGS_BYTES, d0
	lea state.NativeCliPreprocessInvocationArgs, a1
	lea state.NativeCliPreprocessSavedInvocationArgs, a2
	adda.l d0, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_ARGS_BYTES, d0
	jsr copy.copyBytes
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY, d0
	lea state.NativeCliPreprocessInvocationFullArgs, a1
	lea state.NativeCliPreprocessSavedInvocationFullArgs, a2
	adda.l d0, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY, d0
	jsr copy.copyBytes
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY, d0
	lea state.NativeCliPreprocessInvocationLabel, a1
	lea state.NativeCliPreprocessSavedInvocationLabel, a2
	adda.l d0, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY, d0
	jsr copy.copyBytes
	rts
	.bend  ; saveInvocationFrame

; Restore the complete current invocation from suspended slot D0.
restoreInvocationFrame	.block
	move.l d0, d7
	add.l d0, d0
	lea state.NativeCliPreprocessSavedInvocationDefinition, a0
	move.w 0(a0, d0.l), d1
	move.w d1, state.NativeCliPreprocessInvocationDefinition
	lea state.NativeCliPreprocessSavedInvocationArgCount, a0
	move.w 0(a0, d0.l), d1
	move.w d1, state.NativeCliPreprocessInvocationArgCount
	lea state.NativeCliPreprocessSavedInvocationBodyIndex, a0
	move.w 0(a0, d0.l), d1
	move.w d1, state.NativeCliPreprocessInvocationBodyIndex
	lea state.NativeCliPreprocessSavedInvocationFullArgsLen, a0
	move.w 0(a0, d0.l), d1
	move.w d1, state.NativeCliPreprocessInvocationFullArgsLen
	lea state.NativeCliPreprocessSavedInvocationLabelLen, a0
	move.w 0(a0, d0.l), d1
	move.w d1, state.NativeCliPreprocessInvocationLabelLen
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_ARG_LENGTH_BYTES, d0
	lea state.NativeCliPreprocessSavedInvocationArgLen, a1
	adda.l d0, a1
	lea state.NativeCliPreprocessInvocationArgLen, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_ARG_LENGTH_BYTES, d0
	jsr copy.copyBytes
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_ARGS_BYTES, d0
	lea state.NativeCliPreprocessSavedInvocationArgs, a1
	adda.l d0, a1
	lea state.NativeCliPreprocessInvocationArgs, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_ARGS_BYTES, d0
	jsr copy.copyBytes
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY, d0
	lea state.NativeCliPreprocessSavedInvocationFullArgs, a1
	adda.l d0, a1
	lea state.NativeCliPreprocessInvocationFullArgs, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_FULL_ARGS_CAPACITY, d0
	jsr copy.copyBytes
	move.l d7, d0
	mulu #constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY, d0
	lea state.NativeCliPreprocessSavedInvocationLabel, a1
	adda.l d0, a1
	lea state.NativeCliPreprocessInvocationLabel, a2
	move.l #constants.NATIVE_PREPROCESS_INVOCATION_LABEL_CAPACITY, d0
	jsr copy.copyBytes
	rts
	.bend  ; restoreInvocationFrame

; Macro substitution lives in opforge.cli.preprocessor_substitution.

; Macro scanning lives in opforge.cli.preprocessor_scan.

; Expanded-line staging lives in opforge.cli.preprocessor_expansion.

; Compare the define-name portions (before '=' or NUL) case-insensitively.
; Inputs: A0/A1 = define tokens. Outputs: D0 = 1 equal, 0 different.
defineNamesEqual	.block
compare
	moveq #0, d2
	moveq #0, d3
	move.b (a0)+, d2
	move.b (a1)+, d3
	cmpi.b #'=', d2
	beq.s leftEnd
	tst.b d2
	beq.s leftEnd
	cmpi.b #'=', d3
	beq.s different
	tst.b d3
	beq.s different
	cmpi.b #'a', d2
	bcs.s foldRight
	cmpi.b #'z', d2
	bhi.s foldRight
	subi.b #32, d2
foldRight
	cmpi.b #'a', d3
	bcs.s folded
	cmpi.b #'z', d3
	bhi.s folded
	subi.b #32, d3
folded
	cmp.b d3, d2
	bne.s different
	bra.s compare
leftEnd
	cmpi.b #'=', d3
	beq.s equal
	tst.b d3
	beq.s equal
different
	moveq #0, d0
	rts
equal
	moveq #1, d0
	rts
	.bend  ; defineNamesEqual

; Copy the directive operand (without trailing whitespace/comment) to scratch.
; Inputs: A4/D7 = trimmed line pointer/length, D6 = directive token length.
stageConditionalName	.block
	movea.l a4, a0
	adda.w d6, a0
	move.l d7, d0
	sub.l d6, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliPreprocessConditionalName, a1
	moveq #constants.TOKEN_BUFFER_CAPACITY - 1, d1
copy
	tst.l d0
	beq.s done
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s done
	cmpi.b #9, d2
	beq.s done
	cmpi.b #';', d2
	beq.s done
	tst.l d1
	beq.s fail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	subq.l #1, d1
	bra.s copy
done
	clr.b (a1)
	moveq #0, d0
	rts
fail
	clr.b state.NativeCliPreprocessConditionalName
	moveq #1, d0
	rts
	.bend  ; stageConditionalName

; Test whether the staged conditional name exists in the CLI define table.
; Inputs: A0 = NUL-terminated name. Outputs: D0 = 1 defined, 0 absent.
isCommandLineDefine	.block
	movem.l d1-d4/a1/a4, -(sp)
	movea.l a0, a4
	clr.w d4
loop
	cmp.w state.NativeCliPreprocessCliDefineCount, d4
	bhs.s absent
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea state.NativeCliPreprocessCliDefines, a1
	adda.l d0, a1
	movea.l a4, a0
	bsr.w defineNamesEqual
	tst.l d0
	bne.s return
	addq.w #1, d4
	bra.s loop
absent
	moveq #0, d0
return
	movem.l (sp)+, d1-d4/a1/a4
	rts
	.bend  ; isCommandLineDefine

; Push a Rust-compatible conditional frame.
; Inputs: D5 = raw condition (0/1). Outputs: D0 = 0 success, 1 overflow.
pushConditionalFrame	.block
	moveq #0, d2
	move.w state.NativeCliPreprocessConditionalDepth, d2
	cmpi.w #constants.NATIVE_PREPROCESS_CONDITIONAL_DEPTH_CAPACITY, d2
	bhs.s fail
	moveq #1, d3
	tst.w d2
	beq.s haveParent
	move.w d2, d4
	subq.w #1, d4
	lea state.NativeCliPreprocessConditionalActive, a0
	moveq #0, d3
	move.b 0(a0, d4.w), d3
haveParent
	lea state.NativeCliPreprocessConditionalAnyTrue, a0
	move.b d5, 0(a0, d2.w)
	lea state.NativeCliPreprocessConditionalInElse, a0
	clr.b 0(a0, d2.w)
	and.b d5, d3
	lea state.NativeCliPreprocessConditionalActive, a0
	move.b d3, 0(a0, d2.w)
	addq.w #1, state.NativeCliPreprocessConditionalDepth
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; pushConditionalFrame

; Apply `.else`, `.else NAME`, or `.elseif NAME` to the current frame.
updateConditionalElse	.block
	moveq #0, d2
	move.w state.NativeCliPreprocessConditionalDepth, d2
	beq.w fail
	subq.w #1, d2
	lea state.NativeCliPreprocessConditionalInElse, a0
	tst.b 0(a0, d2.w)
	bne.w fail
	moveq #1, d3
	tst.w d2
	beq.s haveParent
	move.w d2, d4
	subq.w #1, d4
	lea state.NativeCliPreprocessConditionalActive, a0
	moveq #0, d3
	move.b 0(a0, d4.w), d3
haveParent
	tst.b state.NativeCliPreprocessConditionalName
	bne.s named
	lea state.NativeCliPreprocessConditionalAnyTrue, a0
	moveq #0, d4
	move.b 0(a0, d2.w), d4
	eori.b #1, d4
	and.b d3, d4
	lea state.NativeCliPreprocessConditionalActive, a0
	move.b d4, 0(a0, d2.w)
	lea state.NativeCliPreprocessConditionalAnyTrue, a0
	move.b #1, 0(a0, d2.w)
	lea state.NativeCliPreprocessConditionalInElse, a0
	move.b #1, 0(a0, d2.w)
	bra.s success
named
	lea state.NativeCliPreprocessConditionalAnyTrue, a0
	tst.b 0(a0, d2.w)
	bne.s namedInactive
	lea state.NativeCliPreprocessConditionalName, a0
	bsr.w isCommandLineDefine
	tst.l d0
	beq.s namedInactive
	tst.b d3
	beq.s namedInactive
	lea state.NativeCliPreprocessConditionalActive, a0
	move.b #1, 0(a0, d2.w)
	lea state.NativeCliPreprocessConditionalAnyTrue, a0
	move.b #1, 0(a0, d2.w)
	bra.s success
namedInactive
	lea state.NativeCliPreprocessConditionalActive, a0
	clr.b 0(a0, d2.w)
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; updateConditionalElse

	.endsection

	.section data, kind=data
MacroText
	.byte ".macro", 0
EndmacroText
	.byte ".endmacro", 0
EndsegmentText
	.byte ".endsegment", 0
EndstatementText
	.byte ".endstatement", 0
PubText
	.byte ".pub", 0
PrivText
	.byte ".priv", 0
	.endsection
	.endmodule
