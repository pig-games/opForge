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
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliResetPreprocessorV1

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
