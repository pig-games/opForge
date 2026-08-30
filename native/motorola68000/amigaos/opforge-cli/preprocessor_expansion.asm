; Expanded-line staging for the native CLI preprocessor.

	.module opforge.cli.preprocessor_expansion
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.copy
	.use opforge.cli.state

	.section code, kind=code
	.pub

; Stage one expanded source line while preserving the caller's logical line in
; the bounded frame selected by ExpansionDepth.
; Inputs: A0 = expansion bytes; D0 = expansion length.
; Outputs: D0 = 0 on success, 1 when nesting or length exceeds this slice.
; Clobbers: D0-D3/A0-A3/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliBeginExpandedLineV1	.block
	cmpi.l #constants.NATIVE_PREPROCESS_EXPANSION_LINE_CAPACITY, d0
	bcc.w fail
	cmpi.w #constants.NATIVE_PREPROCESS_EXPANSION_DEPTH_LIMIT, state.NativeCliPreprocessExpansionDepth
	bcc.s fail
	movea.l a0, a3
	move.l d0, d3
	lea state.NativeCliSourceLine, a1
	lea state.NativeCliPreprocessSavedLine, a2
	moveq #0, d1
	move.w state.NativeCliPreprocessExpansionDepth, d1
	mulu #constants.NATIVE_PREPROCESS_SAVED_LINE_CAPACITY, d1
	adda.l d1, a2
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	move.l d0, d2
	jsr copy.copyBytes
	moveq #0, d1
	move.w state.NativeCliPreprocessExpansionDepth, d1
	add.w d1, d1
	lea state.NativeCliPreprocessSavedLineLen, a0
	move.w d2, 0(a0, d1.l)
	movea.l a3, a1
	lea state.NativeCliPreprocessExpansionLine, a2
	move.l d3, d0
	jsr copy.copyBytes
	lea state.NativeCliPreprocessExpansionLine, a1
	lea state.NativeCliSourceLine, a2
	move.l d3, d0
	jsr copy.copyBytes
	move.w d3, state.NativeCliSourceLineLen
	addq.w #1, state.NativeCliPreprocessExpansionDepth
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
	subq.w #1, state.NativeCliPreprocessExpansionDepth
	lea state.NativeCliPreprocessSavedLine, a1
	moveq #0, d1
	move.w state.NativeCliPreprocessExpansionDepth, d1
	mulu #constants.NATIVE_PREPROCESS_SAVED_LINE_CAPACITY, d1
	adda.l d1, a1
	lea state.NativeCliSourceLine, a2
	moveq #0, d0
	move.w state.NativeCliPreprocessExpansionDepth, d0
	add.w d0, d0
	lea state.NativeCliPreprocessSavedLineLen, a0
	move.w 0(a0, d0.l), d0
	move.l d0, d3
	jsr copy.copyBytes
	move.w d3, state.NativeCliSourceLineLen
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliEndExpandedLineV1

; Force restoration of the caller line after a failed normal cleanup.
; Inputs: the saved-line slot produced by BeginExpandedLineV1.
; Outputs: D0 = 0.
; Clobbers: D0-D3/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliAbortExpandedLineV1	.block
	tst.w state.NativeCliPreprocessExpansionDepth
	beq.s fail
	subq.w #1, state.NativeCliPreprocessExpansionDepth
	lea state.NativeCliPreprocessSavedLine, a1
	moveq #0, d1
	move.w state.NativeCliPreprocessExpansionDepth, d1
	mulu #constants.NATIVE_PREPROCESS_SAVED_LINE_CAPACITY, d1
	adda.l d1, a1
	lea state.NativeCliSourceLine, a2
	moveq #0, d0
	move.w state.NativeCliPreprocessExpansionDepth, d0
	add.w d0, d0
	lea state.NativeCliPreprocessSavedLineLen, a0
	move.w 0(a0, d0.l), d0
	move.l d0, d3
	jsr copy.copyBytes
	move.w d3, state.NativeCliSourceLineLen
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliAbortExpandedLineV1

	.endsection
	.endmodule
