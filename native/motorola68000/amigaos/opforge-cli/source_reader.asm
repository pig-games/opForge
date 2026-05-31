; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.source_reader
	.cpu 68020

	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.strings
	.use opforge.cli.dos
	.use opforge.cli.path
	.use tkpkg.amigaos.buffers

	.use tkpkg.amigaos.abi

	.use opforge.cli.package_pipeline
	.use opforge.cli.line_processor
	.use opforge.cli.include_use
	.use opforge.cli.tkpkg_control_block

	.section code, kind=code
	.pub

; Initialize package state, tokenize every source line, and run parser routing.
opforgeNativeCliTokenizeFrontend	.block
	movem.l d2-d7/a2-a6, -(sp)
	bsr.w package_pipeline.opforgeNativeCliInitPackagePipeline
	tst.l d0
	bne.b return
	move.l #strings.TokenizerOkText, d1
	jsr dos.putStr
	move.w #-1, state.NativeCliResolvedModuleId
	bsr.w opforgeNativeCliTokenizeFile
	tst.l d0
	bne.s return

success
	moveq #0, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opforgeNativeCliTokenizeFrontend

	.priv

; Tokenize the primary input file path recorded by argument parsing.
opforgeNativeCliTokenizeFile	.block
	lea state.NativeCliInputPath, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	lea state.NativeCliInputPath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeFile

; Read and tokenize one AmigaDOS text file at A0, preserving logical line state.
opforgeNativeCliTokenizeFileAtPath	.block
	jsr dos.openInput
	tst.l d0
	bne.s openOk
	moveq #1, d0
	rts

openOk
	move.l d0, d5
	move.l #1, state.NativeCliSourceLineNum
	clr.w state.NativeCliSourceLineLen
	clr.w state.NativeCliSawCr

loop
	lea state.NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr dos.readInput
	cmp.l #-1, d0
	beq.w close
	tst.l d0
	beq.w fileEof

	move.b state.NativeCliInputChar, d0
	tst.w state.NativeCliSawCr
	beq.s checkBreak
	clr.w state.NativeCliSawCr
	cmpi.b #10, d0
	beq.w loop

checkBreak
	cmpi.b #10, d0
	beq.s lineDone
	cmpi.b #13, d0
	beq.s crDone

	move.w state.NativeCliSourceLineLen, d1
	cmpi.w #constants.SOURCE_LINE_BUFFER_CAPACITY, d1
	bhs.w close
	lea state.NativeCliSourceLine, a1
	move.b d0, 0(a1, d1.W)
	addq.w #1, d1
	move.w d1, state.NativeCliSourceLineLen
	bra.w loop

crDone
	move.w #1, state.NativeCliSawCr

lineDone
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingInclude
	tst.l d0
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingUseModule
	tst.l d0
	bne.s close
	move.l state.NativeCliSourceLineNum, d0
	addq.l #1, d0
	move.l d0, state.NativeCliSourceLineNum
	clr.w state.NativeCliSourceLineLen
	bra.w loop

fileEof
	tst.w state.NativeCliSourceLineLen
	beq.s checkModuleDepth
	jsr line_processor.opforgeNativeCliTokenizeCurrentLine
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingInclude
	tst.l d0
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingUseModule
	tst.l d0
	bne.s close

checkModuleDepth
	tst.w state.NativeCliIncludeDepth
	bne.s successClose
	tst.w state.NativeCliModuleResolveDepth
	bne.s successClose
	tst.w state.NativeCliModuleDepth
	beq.s successClose
	move.l #strings.ModuleDepthFailureText, d1
	jsr dos.putStr
	bra.s close

successClose
	move.l d5, d1
	jsr dos.close
	moveq #0, d0
	rts

close
	move.l d5, d1
	jsr dos.close
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeFileAtPath

opforgeNativeCliTokenizePendingInclude	.block
	bsr.w include_use.opforgeNativeCliPreparePendingInclude
	tst.l d0
	bne.s return
	tst.l d1
	beq.s return
	lea state.NativeCliIncludePath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	bsr.w include_use.opforgeNativeCliFinishPendingInclude

return
	rts
	.bend  ; opforgeNativeCliTokenizePendingInclude

opforgeNativeCliTokenizePendingUseModule	.block
	cmpi.w #-1, state.NativeCliResolvedModuleId
	beq.s ok
	move.w #-1, state.NativeCliResolvedModuleId
	bsr.w opforgeNativeCliTokenizeResolvedUseModule
	rts

ok
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliTokenizePendingUseModule

opforgeNativeCliTokenizeResolvedUseModule	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.w state.NativeCliSourceLineLen, d0
	move.w d0, state.NativeCliModuleSavedLineLen
	move.w state.NativeCliSawCr, d0
	move.w d0, state.NativeCliModuleSavedSawCr
	move.l state.NativeCliSourceLineNum, d0
	move.l d0, state.NativeCliModuleSavedLineNum
	lea state.NativeCliCurrentPath, a0
	lea state.NativeCliModuleSavedPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w fail
	lea state.NativeCliIncludePath, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.w fail
	lea state.NativeCliIncludePath, a0
	move.w #1, state.NativeCliModuleResolveDepth
	bsr.w opforgeNativeCliTokenizeFileAtPath
	clr.w state.NativeCliModuleResolveDepth
	tst.l d0
	bne.s restoreFail
	moveq #0, d1
	bra.s restore

restoreFail
	moveq #1, d1

restore
	move.w state.NativeCliModuleSavedLineLen, d2
	move.w d2, state.NativeCliSourceLineLen
	move.w state.NativeCliModuleSavedSawCr, d2
	move.w d2, state.NativeCliSawCr
	move.l state.NativeCliModuleSavedLineNum, d2
	move.l d2, state.NativeCliSourceLineNum
	lea state.NativeCliModuleSavedPath, a0
	lea state.NativeCliCurrentPath, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	tst.l d1
	bne.s fail
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; opforgeNativeCliTokenizeResolvedUseModule

	.endsection
	.endmodule
