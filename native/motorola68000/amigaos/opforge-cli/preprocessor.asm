; Bounded source-preprocessor state for native macro and statement expansion.

	.module opforge.cli.preprocessor
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.state
	.use opforge.cli.line_text
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
	; Captured source lines are NUL-padded storage.  A source NUL terminates
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
	; width so the input cursor advances beyond a literal dotted directive (for
	; example `.byte`) instead of rescanning its first identifier byte.
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
; Inputs: A1 = expansion base; D5 = output length; D4 = byte.
; Outputs: D0 = 0 on success, 1 on overflow; D5 advanced on success.
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

; Copy one identifier after a `.` into SavedLine for named lookup.
; Inputs: A0 = first identifier byte; D6 = remaining input.
; Outputs: D0 = identifier length, D1 = 0 on success / 1 otherwise.
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

; Resolve and append a named parameter. The captured header keeps parameter
; names in definition order, so the result is the corresponding positional slot.
; Inputs: A0 = name bytes; D0 = name length; A1/D5 = expansion output.
; Outputs: D0 = 0 on success, -1 when the named binding is absent.
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
	; Find the macro directive, then compare each comma-delimited parameter name.
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

lineStartsWithDirective	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	jsr line_text.opforgeNativeCliLineStartsWith
	rts
	.bend  ; lineStartsWithDirective

; Match `.endmacro` as the first non-whitespace directive.
lineStartsWithEndmacroDirective	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	cmpi.l #9, d0
	bcs.s no
	cmpi.b #'.', (a0)
	bne.s no
	move.b 1(a0), d1
	ori.b #32, d1
	cmpi.b #'e', d1
	bne.s no
	move.b 2(a0), d1
	ori.b #32, d1
	cmpi.b #'n', d1
	bne.s no
	move.b 3(a0), d1
	ori.b #32, d1
	cmpi.b #'d', d1
	bne.s no
	move.b 4(a0), d1
	ori.b #32, d1
	cmpi.b #'m', d1
	bne.s no
	move.b 5(a0), d1
	ori.b #32, d1
	cmpi.b #'a', d1
	bne.s no
	move.b 6(a0), d1
	ori.b #32, d1
	cmpi.b #'c', d1
	bne.s no
	move.b 7(a0), d1
	ori.b #32, d1
	cmpi.b #'r', d1
	bne.s no
	move.b 8(a0), d1
	ori.b #32, d1
	cmpi.b #'o', d1
	bne.s no
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; lineStartsWithEndmacroDirective

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

; Match a standalone `.macro` directive without relying on a data-section
; pointer. Inputs are the source line in A0/D0; D0 is 1 on match, else 0.
lineContainsMacroDirective	.block
	movem.l d1-d4/a0, -(sp)
	cmpi.l #6, d0
	bcs.w no
	move.l d0, d3
	subi.l #6, d3
	clr.l d2
scan
	cmp.l d3, d2
	bhi.w no
	tst.l d2
	beq.s candidate
	move.b -1(a0, d2.l), d1
	cmpi.b #' ', d1
	beq.s candidate
	cmpi.b #9, d1
	bne.w next
candidate
	cmpi.b #'.', 0(a0, d2.l)
	bne.w next
	move.b 1(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'m', d1
	bne.w next
	move.b 2(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'a', d1
	bne.w next
	move.b 3(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'c', d1
	bne.w next
	move.b 4(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'r', d1
	bne.w next
	move.b 5(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'o', d1
	bne.w next
	cmp.l d3, d2
	beq.s yes
	move.b 6(a0, d2.l), d1
	cmpi.b #' ', d1
	beq.s yes
	cmpi.b #9, d1
	beq.s yes
	cmpi.b #';', d1
	beq.s yes
next
	addq.l #1, d2
	bra.w scan
yes
	movem.l (sp)+, d1-d4/a0
	moveq #1, d0
	rts
no
	movem.l (sp)+, d1-d4/a0
	moveq #0, d0
	rts
	.bend  ; lineContainsMacroDirective

lineContainsDirective	.block
	movem.l d5/a3, -(sp)
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d3
	cmp.l d3, d0
	bcc.s containsLongEnough
	bra.w no
containsLongEnough
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
