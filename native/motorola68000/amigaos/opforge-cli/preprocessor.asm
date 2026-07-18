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

; Macro substitution lives in opforge.cli.preprocessor_substitution.

; Macro scanning lives in opforge.cli.preprocessor_scan.

; Expanded-line staging lives in opforge.cli.preprocessor_expansion.

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
