; Bounded source-preprocessor state for native macro and statement expansion.

	.module opforge.cli.preprocessor
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.state
	.use opforge.cli.line_text

	.section code, kind=code
	.pub

; Clear all per-run preprocessor state before the frontend reads source.
; Inputs: none.
; Outputs: D0 = 0.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliResetPreprocessorV1	.block
	lea state.NativeCliPreprocessStateStart, a0
	move.l #constants.NATIVE_PREPROCESS_STATE_BYTES, d0
	jsr copy.clearBytes
	move.w #-1, state.NativeCliPreprocessActiveDefinition
	move.w #-1, state.NativeCliPreprocessInvocationDefinition
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliResetPreprocessorV1

; Reserve the one bounded macro invocation frame for a captured definition.
; Inputs: D0 = zero-based definition index.
; Outputs: D0 = 0 on success, 1 when the index is out of range or a frame is active.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliBeginMacroInvocationFrameV1	.block
	cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d0
	bcc.s fail
	tst.w state.NativeCliPreprocessInvocationDefinition
	bpl.s fail
	move.w d0, state.NativeCliPreprocessInvocationDefinition
	clr.w state.NativeCliPreprocessInvocationArgCount
	clr.w state.NativeCliPreprocessInvocationBodyIndex
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliBeginMacroInvocationFrameV1

; Parse one captured macro invocation before source recording/tokenization.
; Inputs: state.NativeCliSourceLine contains the logical source line.
; Outputs: D0 = 0 passthrough, 1 consumed with a complete frame, -1 malformed/capacity failure.
; Clobbers: D0-D7/A0-A4/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseMacroInvocationV1	.block
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w pass
	clr.w state.NativeCliPreprocessInvocationLabelLen
	cmpi.b #'.', (a0)
	beq.s directive
	bsr.w captureInvocationLabel
	bne.w malformed
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w pass

directive
	cmpi.b #'.', (a0)
	bne.w pass
	addq.l #1, a0
	subq.l #1, d0
	beq.w pass
	movea.l a0, a3
	move.l d0, d3
	bsr.w takeInvocationName
	bne.w malformed
	move.l d0, d4
	movea.l a3, a0
	move.l d3, d0
	bsr.w findCapturedMacroDefinition
	tst.w d0
	bmi.w pass
	bsr.w opforgeNativeCliBeginMacroInvocationFrameV1
	bne.w malformed
	movea.l a3, a0
	move.l d3, d0
	add.l d4, a0
	sub.l d4, d0
	bsr.w parseInvocationArguments
	bne.w clearFrameAndFail
	moveq #1, d0
	rts

clearFrameAndFail
	move.w #-1, state.NativeCliPreprocessInvocationDefinition
malformed
	moveq #-1, d0
	rts

pass
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliParseMacroInvocationV1

; Capture an optional label before a dotted macro call.
; Inputs: A0 = first non-whitespace byte; D0 = remaining bytes.
; Outputs: D0 = 0, A0/D0 advanced to the byte after the label; nonzero on malformed/capacity.
; Clobbers: D1-D3/A1/CCR.
captureInvocationLabel	.block
	lea state.NativeCliPreprocessInvocationLabel, a1
	move.l #constants.SOURCE_LINE_BUFFER_CAPACITY - 1, d2
	clr.w d3
loop
	tst.l d0
	beq.s done
	move.b (a0), d1
	cmpi.b #':', d1
	beq.s colon
	cmpi.b #' ', d1
	beq.s done
	cmpi.b #9, d1
	beq.s done
	cmpi.b #';', d1
	beq.s fail
	cmpi.b #'.', d1
	beq.s fail
	tst.w d3
	bne.s copy
	cmpi.b #'A', d1
	bcs.s fail
	cmpi.b #'Z', d1
	bls.s copy
	cmpi.b #'a', d1
	bcs.s fail
	cmpi.b #'z', d1
	bhi.s fail
copy
	tst.l d2
	beq.s fail
	move.b d1, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d3
	subq.l #1, d2
	bra.s loop
colon
	addq.l #1, a0
	subq.l #1, d0
done
	tst.w d3
	beq.s fail
	clr.b (a1)
	move.w d3, state.NativeCliPreprocessInvocationLabelLen
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; captureInvocationLabel

; Take the dotted invocation name, leaving its pointer in A3 and byte count in D0.
; Inputs: A0 = first name byte; D0 = remaining bytes.
; Outputs: D0 = name length, D1 = 0 on success / 1 on malformed.
; Clobbers: D0-D2/CCR.
takeInvocationName	.block
	clr.l d2
loop
	tst.l d0
	beq.s done
	move.b 0(a0, d2.l), d1
	cmpi.b #'A', d1
	bcs.s maybeDigit
	cmpi.b #'Z', d1
	bls.s next
	cmpi.b #'a', d1
	bcs.s maybeDigit
	cmpi.b #'z', d1
	bls.s next
maybeDigit
	cmpi.b #'_', d1
	beq.s next
	cmpi.b #'0', d1
	bcs.s done
	cmpi.b #'9', d1
	bhi.s done
next
	addq.l #1, d2
	cmp.l d0, d2
	bcs.s loop
done
	tst.l d2
	beq.s fail
	move.l d2, d0
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; takeInvocationName

; Look up A3/D4's dotted name in captured label-attached macro headers.
; Inputs: A3 = invocation-name bytes; D4 = invocation-name length.
; Outputs: D0 = definition index, -1 when no captured macro matches.
; Clobbers: D0-D2/D5-D7/A0-A2/CCR.
findCapturedMacroDefinition	.block
	moveq #0, d7
loop
	cmp.w state.NativeCliPreprocessDefinitionCount, d7
	bcc.s no
	move.l d7, d5
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d5
	lea state.NativeCliPreprocessDefinitionHeader, a0
	adda.l d5, a0
	move.l d7, d6
	add.l d6, d6
	lea state.NativeCliPreprocessDefinitionHeaderLen, a1
	moveq #0, d5
	move.w 0(a1, d6.l), d5
	move.l d4, d0
	cmp.l d5, d0
	bhi.s next
	moveq #0, d1
compare
	cmp.l d4, d1
	beq.s matched
	move.b 0(a0, d1.l), d2
	move.b 0(a3, d1.l), d5
	cmpi.b #'A', d2
	bcs.s headerFolded
	cmpi.b #'Z', d2
	bhi.s headerFolded
	addi.b #32, d2
headerFolded
	cmpi.b #'A', d5
	bcs.s invocationFolded
	cmpi.b #'Z', d5
	bhi.s invocationFolded
	addi.b #32, d5
invocationFolded
	cmp.b d5, d2
	bne.s next
	addq.l #1, d1
	bra.s compare
matched
	move.b 0(a0, d4.l), d2
	cmpi.b #' ', d2
	beq.s yes
	cmpi.b #9, d2
	beq.s yes
	cmpi.b #':', d2
	beq.s yes
next
	addq.w #1, d7
	bra.s loop
yes
	move.w d7, d0
	rts
no
	moveq #-1, d0
	rts
	.bend  ; findCapturedMacroDefinition

; Parse and bind the argument text after a recognized invocation.
; Inputs: A0 = byte after the invocation name; D0 = remaining bytes.
; Outputs: D0 = 0 on success, 1 on malformed/overflow.
; Clobbers: D0-D7/A0-A2/CCR.
parseInvocationArguments	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s empty
	cmpi.b #'(', (a0)
	beq.s paren
	cmpi.b #',', (a0)
	bne.s list
	addq.l #1, a0
	subq.l #1, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s fail
list
	moveq #0, d7
	bsr.w splitInvocationArgumentList
	bne.s fail
	bra.s bind
paren
	addq.l #1, a0
	subq.l #1, d0
	moveq #1, d7
	bsr.w splitInvocationArgumentList
	bne.s fail
	cmpi.b #')', (a0)
	bne.s fail
	addq.l #1, a0
	subq.l #1, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	bne.s fail
bind
	bsr.w bindMacroParameterDefaults
	bne.s fail
	moveq #0, d0
	rts
empty
	clr.w state.NativeCliPreprocessInvocationArgCount
	clr.w state.NativeCliPreprocessInvocationFullArgsLen
	clr.b state.NativeCliPreprocessInvocationFullArgs
	bsr.w bindMacroParameterDefaults
	bne.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; parseInvocationArguments

; Split a macro argument list while preserving quoted and nested commas.
; Inputs: A0 = first argument byte; D0 = remaining bytes; D7 = 1 for a
; parenthesized list, 0 for a line-rest list.
; Outputs: A0/D0 stop at the closing ')' or end; D0 = 0 on success, 1 on failure.
; Clobbers: D0-D7/A0-A2/CCR.
splitInvocationArgumentList	.block
	lea state.NativeCliPreprocessInvocationArgs, a1
	lea state.NativeCliPreprocessInvocationFullArgs, a2
	clr.l d1
	clr.l d2
	clr.l d3
	clr.l d4
	clr.l d5
	clr.l d6
loop
	tst.l d0
	beq.s endOfLine
	move.b (a0), d3
	tst.l d4
	bne.s quoted
	cmpi.b #'\'', d3
	beq.s singleQuote
	cmpi.b #'"', d3
	beq.s doubleQuote
	tst.l d7
	beq.s structural
	cmpi.b #')', d3
	bne.s structural
	tst.l d5
	bne.s structural
	tst.l d6
	bne.s structural
	tst.w state.NativeCliPreprocessInvocationArgCount
	beq.s closeEmpty
	tst.l d1
	beq.s fail
	bsr.w finishInvocationArgument
	bne.s fail
	bra.s close
closeEmpty
	tst.l d1
	bne.s finishClose
	bra.s close
finishClose
	bsr.w finishInvocationArgument
	bne.s fail
close
	clr.b 0(a2, d2.l)
	move.w d2, state.NativeCliPreprocessInvocationFullArgsLen
	moveq #0, d0
	rts

structural
	cmpi.b #'(', d3
	bne.s closeParen
	addq.l #1, d5
	bra.s copy
closeParen
	cmpi.b #')', d3
	bne.s openBracket
	tst.l d5
	beq.s fail
	subq.l #1, d5
	bra.s copy
openBracket
	cmpi.b #'[', d3
	bne.s closeBracket
	addq.l #1, d6
	bra.s copy
closeBracket
	cmpi.b #']', d3
	bne.s openBrace
	tst.l d6
	beq.s fail
	subq.l #1, d6
	bra.s copy
openBrace
	cmpi.b #'{', d3
	bne.s closeBrace
	addq.l #1, d6
	bra.s copy
closeBrace
	cmpi.b #'}', d3
	bne.s comma
	tst.l d6
	beq.s fail
	subq.l #1, d6
	bra.s copy
comma
	cmpi.b #',', d3
	bne.s copy
	tst.l d5
	bne.s copy
	tst.l d6
	bne.s copy
	tst.l d4
	bne.s copy
	bsr.w finishInvocationArgument
	bne.s fail
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d2
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY - 1, d2
	bcc.s fail
	move.b #',', -1(a2, d2.l)
	moveq #0, d1
	bra.s loop

singleQuote
	moveq #1, d4
	bra.s copy
doubleQuote
	moveq #2, d4
	bra.s copy
quoted
	cmpi.b #'\'', d3
	bne.s quotedDouble
	cmpi.l #1, d4
	bne.s copy
	clr.l d4
	bra.s copy
quotedDouble
	cmpi.b #'"', d3
	bne.s copy
	cmpi.l #2, d4
	bne.s copy
	clr.l d4

copy
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY - 1, d1
	bcc.s fail
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY - 1, d2
	bcc.s fail
	move.b d3, 0(a1, d1.l)
	move.b d3, 0(a2, d2.l)
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d1
	addq.l #1, d2
	bra.w loop

endOfLine
	tst.l d7
	bne.s fail
	tst.l d4
	bne.s fail
	tst.l d5
	bne.s fail
	tst.l d6
	bne.s fail
	tst.w state.NativeCliPreprocessInvocationArgCount
	beq.s emptyList
	tst.l d1
	beq.s fail
	bsr.w finishInvocationArgument
	bne.s fail
	emptylist
	clr.b 0(a2, d2.l)
	move.w d2, state.NativeCliPreprocessInvocationFullArgsLen
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; splitInvocationArgumentList

; Commit one trimmed positional argument into the selected invocation frame.
; Inputs: A1 = current argument buffer; D1 = raw byte length.
; Outputs: D0 = 0 on success, 1 on empty/overflow; A1 = next slot base.
; Clobbers: D0-D3/A0-A2/CCR.
finishInvocationArgument	.block
	bsr.w trimInvocationArgument
	tst.l d1
	beq.s fail
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationArgCount, d0
	cmpi.w #constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY, d0
	bcc.s fail
	move.l d0, d2
	add.l d2, d2
	lea state.NativeCliPreprocessInvocationArgLen, a0
	move.w d1, 0(a0, d2.l)
	addq.w #1, state.NativeCliPreprocessInvocationArgCount
	lea state.NativeCliPreprocessInvocationArgs, a1
	moveq #0, d3
	move.w state.NativeCliPreprocessInvocationArgCount, d3
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d3
	adda.l d3, a1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; finishInvocationArgument

; Remove leading/trailing spaces and tabs from one in-place argument buffer.
; Inputs: A1 = buffer; D1 = raw byte length.
; Outputs: D1 = trimmed length; buffer contains the trimmed bytes.
; Clobbers: D0-D3/A0-A2/CCR.
trimInvocationArgument	.block
	movea.l a1, a0
leading
	tst.l d1
	beq.s trailing
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.s skipLeading
	cmpi.b #9, d0
	bne.s moveLeading
skipLeading
	addq.l #1, a0
	subq.l #1, d1
	bra.s leading
moveLeading
	cmpa.l a0, a1
	beq.s trailing
	movea.l a0, a2
	movea.l a1, a0
	move.l d1, d2
moveLoop
	move.b (a2)+, (a0)+
	subq.l #1, d2
	bne.s moveLoop
trailing
	tst.l d1
	beq.s done
	movea.l a1, a0
	add.l d1, a0
trimLoop
	subq.l #1, a0
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.s trimOne
	cmpi.b #9, d0
	bne.s done
trimOne
	subq.l #1, d1
	bne.s trimLoop
done
	clr.b 0(a1, d1.l)
	rts
	.bend  ; trimInvocationArgument

; Bind omitted positional slots to the definition's declared defaults.
; The captured definition header is the native source of parameter names and
; defaults; later substitution resolves named references against the same order.
; Inputs: state.NativeCliPreprocessInvocationDefinition selects a header.
; Outputs: D0 = 0 on success, 1 for an over-capacity parameter declaration.
; Clobbers: D0-D7/A0-A3/CCR.
bindMacroParameterDefaults	.block
	moveq #0, d7
	move.w state.NativeCliPreprocessInvocationDefinition, d7
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d7
	lea state.NativeCliPreprocessDefinitionHeader, a0
	adda.l d7, a0
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationDefinition, d7
	add.w d7, d7
	lea state.NativeCliPreprocessDefinitionHeaderLen, a1
	move.w 0(a1, d7.w), d0
	; Skip the label-attached macro name and the `.macro` directive.
findDirective
	tst.l d0
	beq.s done
	cmpi.b #'.', (a0)
	bne.s nextByte
	cmpi.l #6, d0
	bcs.s nextByte
	move.b 1(a0), d1
	cmpi.b #'A', d1
	bcs.s directiveNext
	cmpi.b #'Z', d1
	bhi.s directiveNext
	addi.b #32, d1
directiveNext
	cmpi.b #'m', d1
	bne.s nextByte
	move.b 2(a0), d1
	cmpi.b #'A', d1
	bcs.s directiveA
	cmpi.b #'Z', d1
	bhi.s directiveA
	addi.b #32, d1
directiveA
	cmpi.b #'a', d1
	bne.s nextByte
	move.b 3(a0), d1
	cmpi.b #'A', d1
	bcs.s directiveC
	cmpi.b #'Z', d1
	bhi.s directiveC
	addi.b #32, d1
directiveC
	cmpi.b #'c', d1
	bne.s nextByte
	move.b 4(a0), d1
	cmpi.b #'A', d1
	bcs.s directiveR
	cmpi.b #'Z', d1
	bhi.s directiveR
	addi.b #32, d1
directiveR
	cmpi.b #'r', d1
	bne.s nextByte
	move.b 5(a0), d1
	cmpi.b #'A', d1
	bcs.s directiveO
	cmpi.b #'Z', d1
	bhi.s directiveO
	addi.b #32, d1
directiveO
	cmpi.b #'o', d1
	bne.s nextByte
	move.b 6(a0), d1
	cmpi.b #'A', d1
	bcs.s directiveDone
	cmpi.b #'Z', d1
	bhi.s directiveDone
	addi.b #32, d1
directiveDone
	cmpi.b #' ', d1
	bne.s nextByte
	addq.l #6, a0
	subq.l #6, d0
	bra.s parameters
nextByte
	addq.l #1, a0
	subq.l #1, d0
	bra.s findDirective

parameters
	clr.w d7
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s done
	movea.l a0, a2
	moveq #-1, d6
	clr.l d5
parameterLoop
	tst.l d0
	beq.s parameterEnd
	move.b (a0), d1
	cmpi.b #',', d1
	beq.s parameterEnd
	cmpi.b #'=', d1
	bne.s parameterNext
	movea.l a0, a3
	addq.l #1, a3
	moveq #0, d6
parameterNext
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	bra.s parameterLoop
parameterEnd
	moveq #0, d1
	move.w state.NativeCliPreprocessInvocationArgCount, d1
	cmp.w d1, d7
	bcs.s existing
	cmpi.w #constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY, d7
	bcc.s fail
	tst.l d6
	bmi.s advance
	move.l d7, d2
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d2
	lea state.NativeCliPreprocessInvocationArgs, a2
	adda.l d2, a2
	move.l a0, d3
	sub.l a3, d3
	movea.l a3, a1
	move.l d3, d0
	jsr copy.copyBytes
	movea.l a2, a1
	move.l d3, d1
	bsr.w trimInvocationArgument
	move.l d7, d2
	add.l d2, d2
	lea state.NativeCliPreprocessInvocationArgLen, a1
	move.w d1, 0(a1, d2.l)
advance
	move.w d7, d1
	addq.w #1, d1
	move.w d1, state.NativeCliPreprocessInvocationArgCount
existing
	addq.w #1, d7
	tst.l d0
	beq.s done
	addq.l #1, a0
	subq.l #1, d0
	bra.w parameters
done
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; bindMacroParameterDefaults

; Consume one `.macro` definition line before tokenizer dispatch.
; Outputs: D0 = 0 passthrough, 1 consumed, -1 malformed/capacity failure.
; Clobbers: D0-D4/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliCaptureMacroDefinitionLineV1	.block
	tst.w state.NativeCliPreprocessActiveDefinition
	bmi.s checkOpen
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndmacroText, a1
	moveq #9, d1
	jsr lineStartsWithDirective
	bne.s close
	lea MacroText, a1
	moveq #6, d1
	jsr lineContainsDirective
	bne.s fail
	lea EndsegmentText, a1
	moveq #11, d1
	jsr lineStartsWithDirective
	bne.s fail
	lea EndstatementText, a1
	moveq #13, d1
	jsr lineStartsWithDirective
	bne.s fail
	bsr.w appendBodyLine
	bne.s fail
	moveq #1, d0
	rts

close
	move.w #-1, state.NativeCliPreprocessActiveDefinition
	moveq #1, d0
	rts

checkOpen
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea EndmacroText, a1
	moveq #9, d1
	jsr lineStartsWithDirective
	bne.s fail
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	lea MacroText, a1
	moveq #6, d1
	jsr lineContainsDirective
	beq.s pass
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w macroHeaderHasName
	beq.s fail
	moveq #0, d2
	move.w state.NativeCliPreprocessDefinitionCount, d2
	cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d2
	bcc.s fail
	lea state.NativeCliPreprocessDefinitionHeader, a2
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d2
	adda.l d2, a2
	lea state.NativeCliSourceLine, a1
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	move.l d0, d3
	jsr copy.copyBytes
	moveq #0, d2
	move.w state.NativeCliPreprocessDefinitionCount, d2
	add.l d2, d2
	lea state.NativeCliPreprocessDefinitionHeaderLen, a2
	move.w d3, 0(a2, d2.l)
	move.w state.NativeCliPreprocessDefinitionCount, state.NativeCliPreprocessActiveDefinition
	addq.w #1, state.NativeCliPreprocessDefinitionCount
	moveq #1, d0
	rts

pass
	moveq #0, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; opforgeNativeCliCaptureMacroDefinitionLineV1

; Verify that no macro definition remained open at end of the source stream.
; Outputs: D0 = 0 when complete, 1 when an `.endmacro` is missing.
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
	mulu #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d2
	add.l d3, d2
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d2
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
	move.w d4, 0(a2, d2.l)
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

lineStartsWithDirective	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	jsr line_text.opforgeNativeCliLineStartsWith
	rts
	.bend  ; lineStartsWithDirective

; Require the first non-whitespace token of a macro header to be a name.
; Inputs: A0 = line bytes; D0 = line length.
; Outputs: D0 = 1 when a non-directive name token begins the header, else 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
macroHeaderHasName	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s no
	cmpi.b #'.', (a0)
	beq.s no
	cmpi.b #';', (a0)
	beq.s no
	moveq #1, d0
	rts

no
	moveq #0, d0
	rts
	.bend  ; macroHeaderHasName

lineContainsDirective	.block
	movem.l d5/a3, -(sp)
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d3
	cmp.l d3, d0
	bcs.s no
	sub.l d3, d0
	move.l d0, d5
	clr.l d4

scan
	cmp.l d5, d4
	bhi.s no
	move.b 0(a2, d4.l), d0
	cmpi.b #';', d0
	beq.s no
	tst.l d4
	beq.s compare
	move.b -1(a2, d4.l), d0
	cmpi.b #' ', d0
	beq.s compare
	cmpi.b #9, d0
	bne.s next

compare
	moveq #0, d0

compareLoop
	cmp.l d3, d0
	beq.s boundary
	move.b 0(a2, d4.l), d1
	add.l d0, d4
	move.b 0(a2, d4.l), d1
	sub.l d0, d4
	move.b 0(a3, d0.l), d2
	cmpi.b #'A', d1
	bcs.s compareByte
	cmpi.b #'Z', d1
	bhi.s compareByte
	addi.b #32, d1

compareByte
	cmp.b d2, d1
	bne.s next
	addq.l #1, d0
	bra.s compareLoop

boundary
	cmp.l d5, d4
	beq.s yes
	add.l d3, d4
	move.b 0(a2, d4.l), d0
	sub.l d3, d4
	cmpi.b #' ', d0
	beq.s yes
	cmpi.b #9, d0
	beq.s yes
	cmpi.b #';', d0
	beq.s yes

next
	addq.l #1, d4
	bra.s scan

yes
	movem.l (sp)+, d5/a3
	moveq #1, d0
	rts

no
	movem.l (sp)+, d5/a3
	moveq #0, d0
	rts
	.bend  ; lineContainsDirective

; Stage one expanded source line while preserving the caller's logical line.
; Inputs: A0 = expansion bytes; D0 = expansion length.
; Outputs: D0 = 0 on success, 1 when nesting or length exceeds this slice.
; Clobbers: D0-D3/A0-A3/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliBeginExpandedLineV1	.block
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY, d0
	bcc.s fail
	tst.w state.NativeCliPreprocessExpansionDepth
	bne.s fail
	movea.l a0, a3
	move.l d0, d3
	lea state.NativeCliSourceLine, a1
	lea state.NativeCliPreprocessSavedLine, a2
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	move.l d0, d2
	jsr copy.copyBytes
	move.w d2, state.NativeCliPreprocessSavedLineLen
	movea.l a3, a1
	lea state.NativeCliPreprocessExpansionLine, a2
	move.l d3, d0
	jsr copy.copyBytes
	lea state.NativeCliPreprocessExpansionLine, a1
	lea state.NativeCliSourceLine, a2
	move.l d3, d0
	jsr copy.copyBytes
	move.w d3, state.NativeCliSourceLineLen
	move.w #1, state.NativeCliPreprocessExpansionDepth
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliBeginExpandedLineV1

; Restore the logical source line saved by BeginExpandedLineV1.
; Inputs: a successful BeginExpandedLineV1 call is active.
; Outputs: D0 = 0 on success, 1 with no active expansion.
; Clobbers: D0-D3/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliEndExpandedLineV1	.block
	tst.w state.NativeCliPreprocessExpansionDepth
	beq.s fail
	lea state.NativeCliPreprocessSavedLine, a1
	lea state.NativeCliSourceLine, a2
	moveq #0, d0
	move.w state.NativeCliPreprocessSavedLineLen, d0
	move.l d0, d3
	jsr copy.copyBytes
	move.w d3, state.NativeCliSourceLineLen
	clr.w state.NativeCliPreprocessExpansionDepth
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliEndExpandedLineV1

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
	.endsection
	.endmodule
