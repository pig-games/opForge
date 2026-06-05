; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.include_use
	.cpu 68020

	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.line_text
	.use opforge.cli.path

	.section code, kind=code
	.pub

; Parse one `.include` directive from the current source line.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen contain the line text.
; Outputs: D0 = 0 on success; state.NativeCliIncludePending/NativeCliIncludeTarget updated.
; Clobbers: A0-A1/D0-D2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliParseIncludeLine	.block
	clr.w state.NativeCliIncludePending
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	addq.l #8, a0
	subq.l #8, d0
	bsr.w line_text.opforgeNativeCliSkipLineWhitespace
	lea state.NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyIncludeTarget
	bne.w fail
	tst.b state.NativeCliIncludeTarget
	beq.w fail
	move.w #1, state.NativeCliIncludePending
	moveq #0, d0
	rts

fail
	move.l #strings.IncludeFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseIncludeLine

; Resolve and stage the pending include path before tokenization enters the include.
; Inputs: state.NativeCliIncludePending and current/saved path state.
; Outputs: D0 = 0 on success; D1 = 1 when an include was staged, 0 when none was pending.
; Clobbers: A0-A1/D0-D1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliPreparePendingInclude	.block
	tst.w state.NativeCliIncludePending
	beq.w none
	clr.w state.NativeCliIncludePending
	tst.w state.NativeCliIncludeDepth
	bne.w fail
	bsr.w opforgeNativeCliResolveIncludePath
	bne.w fail

	move.w state.NativeCliSourceLineLen, d0
	move.w d0, state.NativeCliSavedLineLen
	move.w state.NativeCliSawCr, d0
	move.w d0, state.NativeCliSavedSawCr
	move.l state.NativeCliSourceLineNum, d0
	move.l d0, state.NativeCliSavedLineNum
	lea state.NativeCliCurrentPath, a0
	lea state.NativeCliSavedPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w fail

	move.l #strings.IncludeStageText, d1
	jsr dos.putStr
	move.l #strings.IncludeRootText, d1
	jsr dos.putStr
	move.l #state.NativeCliIncludeRootPath, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.IncludeFileText, d1
	jsr dos.putStr
	move.l #state.NativeCliIncludePath, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	move.l #strings.IncludeEnterText, d1
	jsr dos.putStr
	move.l #state.NativeCliCurrentPath, d1
	jsr dos.putStr
	move.l #strings.SpaceText, d1
	jsr dos.putStr
	move.l #state.NativeCliIncludePath, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr

	move.w #constants.NATIVE_INCLUDE_DEPTH_LIMIT, state.NativeCliIncludeDepth
	lea state.NativeCliIncludePath, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	moveq #1, d1
	moveq #0, d0
	rts

none
	moveq #0, d1
	moveq #0, d0
	rts

fail
	move.l #strings.IncludeFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliPreparePendingInclude

opforgeNativeCliFinishPendingInclude	.block
	movem.l d1/a0-a1, -(sp)
	tst.l d0
	bne.s restoreFail
	move.l #strings.IncludeLeaveText, d1
	jsr dos.putStr
	move.l #strings.IncludeOkText, d1
	jsr dos.putStr
	moveq #0, d1
	bra.s restore

restoreFail
	move.l #strings.IncludeFailureText, d1
	jsr dos.putStr
	moveq #1, d1

restore
	move.w state.NativeCliSavedLineLen, d0
	move.w d0, state.NativeCliSourceLineLen
	move.w state.NativeCliSavedSawCr, d0
	move.w d0, state.NativeCliSawCr
	move.l state.NativeCliSavedLineNum, d0
	move.l d0, state.NativeCliSourceLineNum
	lea state.NativeCliSavedPath, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	clr.w state.NativeCliIncludeDepth
	move.l d1, d0
	bra.s return

fail
	clr.w state.NativeCliIncludeDepth
	moveq #1, d0

return
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; opforgeNativeCliFinishPendingInclude

; Resolve the pending include target into NativeCliIncludePath.
; Inputs: state.NativeCliCurrentPath, NativeCliIncludeTarget.
; Outputs: D0 = 0 on success; state.NativeCliIncludeRootPath/NativeCliIncludePath updated.
; Clobbers: A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliResolveIncludePath	.block
	lea state.NativeCliCurrentPath, a0
	lea state.NativeCliIncludeRootPath, a1
	jsr path.opforgeNativeCliCopyPathRoot
	bne.w fail
	lea state.NativeCliIncludeTarget, a0
	jsr path.opforgeNativeCliPathHasVolumePrefix
	beq.s appendFromRoot
	lea state.NativeCliIncludeTarget, a0
	lea state.NativeCliIncludePath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	rts

appendFromRoot
	lea state.NativeCliIncludeRootPath, a0
	lea state.NativeCliIncludePath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	lea state.NativeCliIncludeTarget, a0
	lea state.NativeCliIncludePath, a1
	jsr path.opforgeNativeCliAppendPathBuffer
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliResolveIncludePath

opforgeNativeCliCopyIncludeTarget	.block
	tst.l d0
	beq.w fail
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #'"', d2
	beq.s quoted
	cmpi.b #39, d2
	beq.s quoted
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d6
	clr.l d5

bareLoop
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
	tst.l d6
	beq.s fail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s bareLoop

quoted
	move.b d2, d4
	addq.l #1, a0
	subq.l #1, d0
	move.l #constants.PATH_BUFFER_CAPACITY - 1, d6
	clr.l d5

quotedLoop
	tst.l d0
	beq.s fail
	moveq #0, d2
	move.b (a0), d2
	cmp.b d4, d2
	beq.s done
	tst.l d6
	beq.s fail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s quotedLoop

done
	tst.l d5
	beq.s fail
	clr.b (a1)
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliCopyIncludeTarget

	.endsection
	.endmodule
