; Native AmigaOS opForge CLI flat output writer.

	.module opforge.cli.output
	.cpu 68020

	.use opasm.amigaos.engine (opasmEngineImageBuffer, opasmEngineImageByteCount)
	.use opforge.cli.state (NativeCliBinPath)
	.use opforge.cli.dos (opforgeNativeCliOpenOutput, opforgeNativeCliWriteOutput, opforgeNativeCliClose)

	.section code, kind=code
	.pub

opforgeNativeCliWriteFlatOutput	.block
	movem.l d1-d4/a0-a1, -(sp)
	lea NativeCliBinPath, a0
	jsr opforgeNativeCliOpenOutput
	tst.l d0
	beq.s fail
	move.l d0, d4
	lea opasmEngineImageBuffer.l, a0
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	move.l d0, d3
	move.l d4, d1
	jsr opforgeNativeCliWriteOutput
	cmp.l d3, d0
	bne.s closeFail
	move.l d4, d1
	jsr opforgeNativeCliClose
	moveq #0, d0
	bra.s return

closeFail
	move.l d4, d1
	jsr opforgeNativeCliClose

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliWriteFlatOutput

	.endsection
	.endmodule
