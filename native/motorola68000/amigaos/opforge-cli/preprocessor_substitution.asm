; Bounded macro body substitution for the native CLI preprocessor.

	.module opforge.cli.preprocessor_substitution
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.line_text

	.section code, kind=code
	.pub

; Substitute one captured body line into the bounded expansion buffer.
; Inputs: D0 = zero-based body index for the active invocation frame.
; Outputs: D0 = 0 on success, 1 on an unknown binding, malformed sigil, or overflow;
;          D1 = expansion byte length on success.
; Clobbers: D0-D7/A0-A4/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliSubstituteMacroBodyLineV1	.block
	moveq #0, d2
	move.w state.NativeCliPreprocessInvocationDefinition, d2
	bmi.w fail
	cmpi.w #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d0
	bcc.w fail
	move.l d2, d3
	mulu #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d3
	add.l d0, d3
	add.l d3, d3
	lea state.NativeCliPreprocessDefinitionBodyLen, a0
	moveq #0, d6
	move.w 0(a0, d3.l), d6
	move.l d2, d3
	mulu #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d3
	add.l d0, d3
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d3
	lea state.NativeCliPreprocessDefinitionBody, a0
	adda.l d3, a0
	lea state.NativeCliPreprocessExpansionLine, a1
	clr.l d5
scan
	tst.l d6
	bne.s substitutionHasInput
	bra.w complete
substitutionHasInput
	move.b (a0), d4
	; Captured source lines are NUL-padded storage. A source NUL terminates
	; substitution even if a stale length slot extends into the padding.
	tst.b d4
	beq.w complete
	cmpi.b #'.', d4
	bne.s substitutionNotDot
	bra.w dot
substitutionNotDot
	cmpi.b #'@', d4
	bne.s substitutionLiteral
	bra.w at
substitutionLiteral
	bsr.w appendExpansionByte
	bne.w fail
	addq.l #1, a0
	subq.l #1, d6
	bra.s scan
dot
	cmpi.l #1, d6
	bne.s substitutionHasDotTail
	bra.w literal
substitutionHasDotTail
	move.b 1(a0), d4
	cmpi.b #'@', d4
	bne.s substitutionNotFullList
	bra.w fullList
substitutionNotFullList
	cmpi.b #'1', d4
	bcs.s bracedOrNamed
	cmpi.b #'9', d4
	bhi.s bracedOrNamed
	subi.b #'1', d4
	bsr.w appendInvocationPositional
	bne.w fail
	addq.l #2, a0
	subq.l #2, d6
	bra.s scan
fullList
	bsr.w appendInvocationFullList
	bne.w fail
	addq.l #2, a0
	subq.l #2, d6
	bra.s scan
bracedOrNamed
	cmpi.b #'{', d4
	bne.s named
	addq.l #2, a0
	subq.l #2, d6
	movea.l a0, a2
	clr.l d0
bracedScan
	tst.l d6
	beq.w fail
	move.b (a0), d4
	cmpi.b #'}', d4
	beq.s bracedDone
	addq.l #1, a0
	subq.l #1, d6
	addq.l #1, d0
	bra.s bracedScan
bracedDone
	movea.l a2, a4
	move.l d0, -(sp)
	bsr.w appendInvocationNamed
	tst.l d0
	beq.s bracedBound
	addq.l #4, sp
	bra.w fail
bracedBound
	move.l (sp)+, d3
	movea.l a4, a0
	adda.l d3, a0
	addq.l #1, a0
	sub.l d3, d6
	subq.l #1, d6
	bra.w scan
named
	; Skip the sigil, then capture the complete identifier and retain its
	; length for appendInvocationNamed's header comparison.
	addq.l #1, a0
	subq.l #1, d6
	bsr.w copyInvocationIdentifier
	bne.w literal
	movea.l a0, a4
	move.l d0, d3
	move.l d0, d7
	move.l d6, -(sp)
	move.l d3, -(sp)
	bsr.w appendInvocationNamed
	tst.l d0
	beq.s namedBound
	bmi.s namedLiteral
	adda.l #8, sp
	bra.w fail
namedLiteral
	move.l (sp)+, d3
	move.l (sp)+, d6
	move.b #'.', d4
	bsr.w appendExpansionByte
	bne.w fail
	movea.l a4, a2
	; appendExpansionBytes consumes D3 as its loop counter. Keep the identifier
	; width so the input cursor advances beyond a literal dotted directive.
	move.l d3, -(sp)
	bsr.w appendExpansionBytes
	tst.l d0
	bne.s namedLiteralAppendFail
	move.l (sp)+, d3
	movea.l a4, a0
	adda.l d3, a0
	sub.l d3, d6
	bra.w scan
namedLiteralAppendFail
	adda.l #4, sp
	bra.w fail
namedBound
	move.l (sp)+, d3
	move.l (sp)+, d6
	movea.l a4, a0
	adda.l d3, a0
	sub.l d3, d6
	bra.w scan
at
	cmpi.l #1, d6
	beq.s literal
	move.b 1(a0), d4
	cmpi.b #'1', d4
	bcs.s literal
	cmpi.b #'9', d4
	bhi.s literal
	subi.b #'1', d4
	bsr.w appendInvocationPositional
	bne.w fail
	addq.l #2, a0
	subq.l #2, d6
	bra.w scan
literal
	move.b (a0), d4
	bsr.w appendExpansionByte
	bne.s fail
	addq.l #1, a0
	subq.l #1, d6
	bra.w scan
complete
	clr.b 0(a1, d5.l)
	move.l d5, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliSubstituteMacroBodyLineV1

; Append D4 to the active bounded expansion line.
appendExpansionByte	.block
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY - 1, d5
	bcc.s fail
	move.b d4, 0(a1, d5.l)
	addq.l #1, d5
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; appendExpansionByte

; Append positional slot D4 (zero-based) to the active expansion line.
appendInvocationPositional	.block
	moveq #0, d0
	move.b d4, d0
	cmpi.w #constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY, d0
	bcc.s fail
	add.l d0, d0
	lea state.NativeCliPreprocessInvocationArgLen, a2
	moveq #0, d3
	move.w 0(a2, d0.l), d3
	lea state.NativeCliPreprocessInvocationArgs, a2
	moveq #0, d0
	move.b d4, d0
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d0
	adda.l d0, a2
	bra.w appendExpansionBytes
fail
	moveq #1, d0
	rts
	.bend  ; appendInvocationPositional

; Append the canonical invocation argument list.
appendInvocationFullList	.block
	lea state.NativeCliPreprocessInvocationFullArgs, a2
	moveq #0, d3
	move.w state.NativeCliPreprocessInvocationFullArgsLen, d3
	bra.w appendExpansionBytes
	.bend  ; appendInvocationFullList

; Append D3 bytes at A2 to the active expansion buffer A1/D5.
appendExpansionBytes	.block
	tst.l d3
	beq.s success
	move.l d5, d0
	add.l d3, d0
	cmpi.l #constants.SOURCE_LINE_BUFFER_CAPACITY, d0
	bcc.s fail
loop
	move.b (a2)+, d4
	move.b d4, 0(a1, d5.l)
	addq.l #1, d5
	subq.l #1, d3
	bne.s loop
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; appendExpansionBytes

; Copy one identifier after a `.` for named lookup.
copyInvocationIdentifier	.block
	clr.l d0
loop
	cmp.l d6, d0
	bcc.s done
	move.b 0(a0, d0.l), d4
	cmpi.b #'A', d4
	bcs.s digit
	cmpi.b #'Z', d4
	bls.s copy
	cmpi.b #'a', d4
	bcs.s digit
	cmpi.b #'z', d4
	bls.s copy
digit
	cmpi.b #'_', d4
	beq.s copy
	cmpi.b #'0', d4
	bcs.s done
	cmpi.b #'9', d4
	bhi.s done
copy
	addq.l #1, d0
	bra.s loop
done
	tst.l d0
	beq.s fail
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; copyInvocationIdentifier

; Resolve and append a named parameter from the captured macro header.
appendInvocationNamed	.block
	movea.l a0, a3
	move.l d0, d7
	move.w state.NativeCliPreprocessInvocationDefinition, d2
	mulu #constants.SOURCE_LINE_BUFFER_CAPACITY, d2
	lea state.NativeCliPreprocessDefinitionHeader, a0
	adda.l d2, a0
	moveq #0, d6
	move.w state.NativeCliPreprocessInvocationDefinition, d2
	add.w d2, d2
	lea state.NativeCliPreprocessDefinitionHeaderLen, a2
	move.w 0(a2, d2.w), d6
findMacro
	tst.l d6
	bne.s namedHeaderRemaining
	bra.w fail
namedHeaderRemaining
	cmpi.b #'.', (a0)
	bne.s next
	cmpi.l #6, d6
	bcs.s next
	move.b 1(a0), d4
	cmpi.b #'m', d4
	bne.s next
	addq.l #6, a0
	subq.l #6, d6
	bra.s initParams
next
	addq.l #1, a0
	subq.l #1, d6
	bra.s findMacro
initParams
	clr.l d2
params
	move.l d6, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	move.l d0, d6
paramStart
	tst.l d6
	bne.s namedParameterRemaining
	bra.w fail
namedParameterRemaining
	movea.l a0, a2
	clr.l d3
paramName
	tst.l d6
	beq.s compare
	move.b (a0), d4
	cmpi.b #'=', d4
	beq.s compare
	cmpi.b #',', d4
	beq.s compare
	cmpi.b #' ', d4
	beq.s compare
	cmpi.b #9, d4
	beq.s compare
	addq.l #1, a0
	subq.l #1, d6
	addq.l #1, d3
	bra.s paramName
compare
	cmp.l d7, d3
	bne.s skip
	moveq #0, d1
compareLoop
	cmp.l d7, d1
	beq.s found
	move.b 0(a2, d1.l), d4
	move.b 0(a3, d1.l), d0
	cmpi.b #'A', d4
	bcs.s headerByte
	cmpi.b #'Z', d4
	bhi.s headerByte
	addi.b #32, d4
headerByte
	cmpi.b #'A', d0
	bcs.s namedByte
	cmpi.b #'Z', d0
	bhi.s namedByte
	addi.b #32, d0
namedByte
	cmp.b d0, d4
	bne.s skip
	addq.l #1, d1
	bra.s compareLoop
found
	move.b d2, d4
	bra.w appendInvocationPositional
skip
	tst.l d6
	beq.s fail
skipToComma
	tst.l d6
	beq.s fail
	move.b (a0), d4
	addq.l #1, a0
	subq.l #1, d6
	cmpi.b #',', d4
	bne.s skipToComma
	addq.l #1, d2
	cmpi.w #constants.NATIVE_PREPROCESS_MACRO_ARG_CAPACITY, d2
	bcc.s fail
	bra.w params
fail
	moveq #-1, d0
	rts
	.bend  ; appendInvocationNamed

	.endsection
	.endmodule
