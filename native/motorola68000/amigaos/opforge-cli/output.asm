; Native AmigaOS opForge CLI flat output writer.

	.module opforge.cli.output
	.cpu 68020

	.use opasm.amigaos.engine
	.use opforge.cli.state
	.use opforge.cli.dos

	.section code, kind=code
	.pub

; Inputs:
;   state.NativeCliBinPath = output file path
;   opasm engine image buffer/count contain the assembled flat output
; Outputs:
;   D0.L = 0 on success, 1 when the output file cannot be opened or written fully
; Clobbers:
;   D0-D4/A0-A1/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliWriteFlatOutput	.block
	movem.l d1-d4/a0-a1, -(sp)
	lea state.NativeCliBinPath, a0
	jsr dos.openOutput
	tst.l d0
	beq.s fail
	move.l d0, d4
	jsr engine.opasmEngineGetImageBufferPtrV1
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d3
	move.l d4, d1
	jsr dos.writeOutput
	cmp.l d3, d0
	bne.s closeFail
	move.l d4, d1
	jsr dos.close
	moveq #0, d0
	bra.s return

closeFail
	move.l d4, d1
	jsr dos.close

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a0-a1
	rts
	.bend  ; opforgeNativeCliWriteFlatOutput

	.endsection
	.endmodule
