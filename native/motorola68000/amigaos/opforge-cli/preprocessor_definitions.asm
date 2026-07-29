; Bounded native macro-definition capture owner.

	.module opforge.cli.preprocessor_definitions
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.state
	.use opforge.cli.preprocessor_scan
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

	.section code, kind=code
	.pub

; Consume one `.macro` definition line before tokenizer dispatch.
; Inputs: state.NativeCliSourceLine contains the logical source line.
; Outputs: D0 = 0 passthrough, 1 consumed, -1 malformed/capacity failure.
; Clobbers: D0-D4/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliCaptureMacroDefinitionLineV1	.block
	tst.w state.NativeCliPreprocessActiveDefinition
	bmi.w checkOpen
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr preprocessor_scan.lineStartsWithEndmacroDirective
	bne.s close
	jsr preprocessor_scan.lineContainsMacroDirective
	beq.s captureNoNestedMacro
	bra.w fail
captureNoNestedMacro
	lea EndsegmentText.l, a1
	moveq #11, d1
	jsr preprocessor_scan.lineStartsWithDirective
	beq.s captureNoEndsegment
	bra.w fail
captureNoEndsegment
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
	jsr preprocessor_scan.lineContainsMacroDirective
	beq.w pass
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr preprocessor_scan.macroHeaderHasName
	beq.w fail
	moveq #0, d2
	move.w state.NativeCliPreprocessDefinitionCount, d2
	cmpi.w #constants.NATIVE_PREPROCESS_DEFINITION_CAPACITY, d2
	bcc.w fail
	lea state.NativeCliPreprocessDefinitionHeader, a2
	mulu #constants.NATIVE_PREPROCESS_DEFINITION_HEADER_CAPACITY, d2
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
	move.w state.NativeCliPreprocessDefinitionCount, d0
	move.w d0, state.NativeCliPreprocessActiveDefinition
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

; Verify that no macro definition remained open at end of the source stream.
; Inputs: none.
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
EndstatementText
	.byte ".endstatement", 0
	.endsection
	.endmodule
