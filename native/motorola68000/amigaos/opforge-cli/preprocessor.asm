; Bounded source-preprocessor state for native macro and statement expansion.

	.module opforge.cli.preprocessor
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.state

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
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliResetPreprocessorV1

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
	.endmodule
