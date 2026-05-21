; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.include_use
	.cpu 68020

	.use opforge.cli.state (NativeCliSourceLine, NativeCliSourceLineLen, NativeCliSawCr)
	.use opforge.cli.state (NativeCliSourceLineNum, NativeCliCurrentPath)
	.use opforge.cli.state (NativeCliIncludeDepth, NativeCliIncludePending, NativeCliIncludeTarget)
	.use opforge.cli.state (NativeCliIncludePath, NativeCliIncludeRootPath)
	.use opforge.cli.state (NativeCliSavedLineLen, NativeCliSavedSawCr, NativeCliSavedLineNum, NativeCliSavedPath)
	.use opforge.cli.constants (NATIVE_INCLUDE_DEPTH_LIMIT, PATH_BUFFER_CAPACITY)
	.use opforge.cli.strings (IncludeStageText, IncludeRootText, IncludeFileText, IncludeEnterText)
	.use opforge.cli.strings (IncludeLeaveText, IncludeOkText, IncludeFailureText, SpaceText, NewlineText)
	.use opforge.cli.dos
	.use opforge.cli.line_text (opforgeNativeCliSkipLineWhitespace)
	.use opforge.cli.path (opforgeNativeCliCopyPathBuffer, opforgeNativeCliCopyPathRoot)
	.use opforge.cli.path (opforgeNativeCliPathIsAbsolute, opforgeNativeCliAppendPathBuffer)

	.section code, kind=code
	.pub

opforgeNativeCliParseIncludeLine	.block
	clr.w NativeCliIncludePending
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	addq.l #8, a0
	subq.l #8, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyIncludeTarget
	tst.l d0
	bne.w fail
	tst.b NativeCliIncludeTarget
	beq.w fail
	move.w #1, NativeCliIncludePending
	moveq #0, d0
	rts

fail
	move.l #IncludeFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliParseIncludeLine

opforgeNativeCliPreparePendingInclude	.block
	tst.w NativeCliIncludePending
	beq.w none
	clr.w NativeCliIncludePending
	tst.w NativeCliIncludeDepth
	bne.w fail
	bsr.w opforgeNativeCliResolveIncludePath
	tst.l d0
	bne.w fail

	move.w NativeCliSourceLineLen, d0
	move.w d0, NativeCliSavedLineLen
	move.w NativeCliSawCr, d0
	move.w d0, NativeCliSavedSawCr
	move.l NativeCliSourceLineNum, d0
	move.l d0, NativeCliSavedLineNum
	lea NativeCliCurrentPath, a0
	lea NativeCliSavedPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w fail

	move.l #IncludeStageText, d1
	jsr dos.putStr
	move.l #IncludeRootText, d1
	jsr dos.putStr
	move.l #NativeCliIncludeRootPath, d1
	jsr dos.putStr
	move.l #NewlineText, d1
	jsr dos.putStr
	move.l #IncludeFileText, d1
	jsr dos.putStr
	move.l #NativeCliIncludePath, d1
	jsr dos.putStr
	move.l #NewlineText, d1
	jsr dos.putStr
	move.l #IncludeEnterText, d1
	jsr dos.putStr
	move.l #NativeCliCurrentPath, d1
	jsr dos.putStr
	move.l #SpaceText, d1
	jsr dos.putStr
	move.l #NativeCliIncludePath, d1
	jsr dos.putStr
	move.l #NewlineText, d1
	jsr dos.putStr

	move.w #NATIVE_INCLUDE_DEPTH_LIMIT, NativeCliIncludeDepth
	lea NativeCliIncludePath, a0
	lea NativeCliCurrentPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s fail
	moveq #1, d1
	moveq #0, d0
	rts

none
	moveq #0, d1
	moveq #0, d0
	rts

fail
	move.l #IncludeFailureText, d1
	jsr dos.putStr
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliPreparePendingInclude

opforgeNativeCliFinishPendingInclude	.block
	movem.l d1/a0-a1, -(sp)
	tst.l d0
	bne.s restoreFail
	move.l #IncludeLeaveText, d1
	jsr dos.putStr
	move.l #IncludeOkText, d1
	jsr dos.putStr
	moveq #0, d1
	bra.s restore

restoreFail
	move.l #IncludeFailureText, d1
	jsr dos.putStr
	moveq #1, d1

restore
	move.w NativeCliSavedLineLen, d0
	move.w d0, NativeCliSourceLineLen
	move.w NativeCliSavedSawCr, d0
	move.w d0, NativeCliSawCr
	move.l NativeCliSavedLineNum, d0
	move.l d0, NativeCliSourceLineNum
	lea NativeCliSavedPath, a0
	lea NativeCliCurrentPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s fail
	clr.w NativeCliIncludeDepth
	move.l d1, d0
	bra.s return

fail
	clr.w NativeCliIncludeDepth
	moveq #1, d0

return
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; opforgeNativeCliFinishPendingInclude

opforgeNativeCliResolveIncludePath	.block
	lea NativeCliCurrentPath, a0
	lea NativeCliIncludeRootPath, a1
	jsr opforgeNativeCliCopyPathRoot
	tst.l d0
	bne.w fail
	lea NativeCliIncludeTarget, a0
	jsr opforgeNativeCliPathIsAbsolute
	tst.l d0
	beq.s relative
	lea NativeCliIncludeTarget, a0
	lea NativeCliIncludePath, a1
	jsr opforgeNativeCliCopyPathBuffer
	rts

relative
	lea NativeCliIncludeRootPath, a0
	lea NativeCliIncludePath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s fail
	lea NativeCliIncludeTarget, a0
	lea NativeCliIncludePath, a1
	jsr opforgeNativeCliAppendPathBuffer
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
	move.l #PATH_BUFFER_CAPACITY - 1, d6
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
	move.l #PATH_BUFFER_CAPACITY - 1, d6
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
