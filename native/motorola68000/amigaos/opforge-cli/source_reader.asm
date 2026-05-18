; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.source_reader
	.cpu 68020

	.use opforge.cli.state (NativeCliInputChar, NativeCliDosBase, NativeCliInputPath, NativeCliCurrentPath)
	.use opforge.cli.state (NativeCliPackageLenActive, NativeCliSourceLine, NativeCliSourceLineNum, NativeCliSourceLineLen)
	.use opforge.cli.state (NativeCliSawCr, NativeCliIncludeDepth, NativeCliModuleResolveDepth)
	.use opforge.cli.state (NativeCliModuleDepth, NativeCliResolvedModuleId)
	.use opforge.cli.state (NativeCliModuleSavedLineLen, NativeCliModuleSavedSawCr, NativeCliModuleSavedLineNum, NativeCliModuleSavedPath, NativeCliIncludePath)
	.use opforge.cli.constants (PACKAGE_INPUT_PTR_V1, SOURCE_LINE_BUFFER_CAPACITY)
	.use opforge.cli.strings (TokenizerOkText, ModuleDepthFailureText)
	.use opforge.cli.dos (opforgeNativeCliPutStr, opforgeNativeCliOpenInput)
	.use opforge.cli.dos (opforgeNativeCliReadInput, opforgeNativeCliClose)
	.use opforge.cli.path (opforgeNativeCliCopyPathBuffer)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)
	.use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_PTR_V1)

	.use tkpkg.amigaos.abi (ENTRY_ORD_INIT, ENTRY_ORD_LOAD_PACKAGE)

	.use opforge.cli.package_pipeline (opforgeNativeCliInitPackagePipeline)
	.use opforge.cli.line_processor (opforgeNativeCliTokenizeCurrentLine)
	.use opforge.cli.include_use (opforgeNativeCliPreparePendingInclude, opforgeNativeCliFinishPendingInclude)
	.use opforge.cli.tkpkg_control_block (opforgeNativeCliReadStatus, opforgeNativeCliReadOutputLen, opforgeNativeCliWriteInputWindow)

	.section code, kind=code
	.pub

; Initialize package state, tokenize every source line, and run parser routing.
opforgeNativeCliTokenizeFrontend	.block
	movem.l d2-d7/a2-a6, -(sp)
	bsr.w opforgeNativeCliInitPackagePipeline
	tst.l d0
	bne.b return
	move.l #TokenizerOkText, d1
	jsr opforgeNativeCliPutStr
	move.w #-1, NativeCliResolvedModuleId
	bsr.w opforgeNativeCliTokenizeFile
	tst.l d0
	bne.s return

success
	moveq #0, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; opforgeNativeCliTokenizeFrontend

; Tokenize the primary input file path recorded by argument parsing.
opforgeNativeCliTokenizeFile	.block
	lea NativeCliInputPath, a0
	lea NativeCliCurrentPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s fail
	lea NativeCliInputPath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeFile

; Read and tokenize one AmigaDOS text file at A0, preserving logical line state.
opforgeNativeCliTokenizeFileAtPath	.block
	jsr opforgeNativeCliOpenInput
	tst.l d0
	bne.s openOk
	moveq #1, d0
	rts

openOk
	move.l d0, d5
	move.l #1, NativeCliSourceLineNum
	clr.w NativeCliSourceLineLen
	clr.w NativeCliSawCr

loop
	lea NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	jsr opforgeNativeCliReadInput
	cmp.l #-1, d0
	beq.w close
	tst.l d0
	beq.w fileEof

	move.b NativeCliInputChar, d0
	tst.w NativeCliSawCr
	beq.s checkBreak
	clr.w NativeCliSawCr
	cmpi.b #10, d0
	beq.w loop

checkBreak
	cmpi.b #10, d0
	beq.s lineDone
	cmpi.b #13, d0
	beq.s crDone

	move.w NativeCliSourceLineLen, d1
	cmpi.w #SOURCE_LINE_BUFFER_CAPACITY, d1
	bhs.w close
	lea NativeCliSourceLine, a1
	move.b d0, 0(a1, d1.W)
	addq.w #1, d1
	move.w d1, NativeCliSourceLineLen
	bra.w loop

crDone
	move.w #1, NativeCliSawCr

lineDone
	jsr opforgeNativeCliTokenizeCurrentLine
	tst.l d0
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingInclude
	tst.l d0
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingUseModule
	tst.l d0
	bne.s close
	move.l NativeCliSourceLineNum, d0
	addq.l #1, d0
	move.l d0, NativeCliSourceLineNum
	clr.w NativeCliSourceLineLen
	bra.w loop

fileEof
	tst.w NativeCliSourceLineLen
	beq.s checkModuleDepth
	jsr opforgeNativeCliTokenizeCurrentLine
	tst.l d0
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingInclude
	tst.l d0
	bne.s close
	bsr.w opforgeNativeCliTokenizePendingUseModule
	tst.l d0
	bne.s close

checkModuleDepth
	tst.w NativeCliIncludeDepth
	bne.s successClose
	tst.w NativeCliModuleResolveDepth
	bne.s successClose
	tst.w NativeCliModuleDepth
	beq.s successClose
	move.l #ModuleDepthFailureText, d1
	jsr opforgeNativeCliPutStr
	bra.s close

successClose
	move.l d5, d1
	jsr opforgeNativeCliClose
	moveq #0, d0
	rts

close
	move.l d5, d1
	jsr opforgeNativeCliClose
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeFileAtPath

opforgeNativeCliTokenizePendingInclude	.block
	bsr.w opforgeNativeCliPreparePendingInclude
	tst.l d0
	bne.s return
	tst.l d1
	beq.s return
	lea NativeCliIncludePath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	bsr.w opforgeNativeCliFinishPendingInclude

return
	rts
	.bend  ; opforgeNativeCliTokenizePendingInclude

opforgeNativeCliTokenizePendingUseModule	.block
	cmpi.w #-1, NativeCliResolvedModuleId
	beq.s ok
	move.w #-1, NativeCliResolvedModuleId
	bsr.w opforgeNativeCliTokenizeResolvedUseModule
	rts

ok
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliTokenizePendingUseModule

opforgeNativeCliTokenizeResolvedUseModule	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.w NativeCliSourceLineLen, d0
	move.w d0, NativeCliModuleSavedLineLen
	move.w NativeCliSawCr, d0
	move.w d0, NativeCliModuleSavedSawCr
	move.l NativeCliSourceLineNum, d0
	move.l d0, NativeCliModuleSavedLineNum
	lea NativeCliCurrentPath, a0
	lea NativeCliModuleSavedPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w fail
	lea NativeCliIncludePath, a0
	lea NativeCliCurrentPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w fail
	lea NativeCliIncludePath, a0
	move.w #1, NativeCliModuleResolveDepth
	bsr.w opforgeNativeCliTokenizeFileAtPath
	clr.w NativeCliModuleResolveDepth
	tst.l d0
	bne.s restoreFail
	moveq #0, d1
	bra.s restore

restoreFail
	moveq #1, d1

restore
	move.w NativeCliModuleSavedLineLen, d2
	move.w d2, NativeCliSourceLineLen
	move.w NativeCliModuleSavedSawCr, d2
	move.w d2, NativeCliSawCr
	move.l NativeCliModuleSavedLineNum, d2
	move.l d2, NativeCliSourceLineNum
	lea NativeCliModuleSavedPath, a0
	lea NativeCliCurrentPath, a1
	jsr opforgeNativeCliCopyPathBuffer
	tst.l d0
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
