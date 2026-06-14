; Native AmigaOS opForge CLI flat output writer.

	.module opforge.cli.output
	.cpu 68020

	.use opasm.amigaos.output_artifacts as artifacts
	.use opforge.cli.state
	.use opforge.cli.constants
	.use opforge.cli.dos

	.section code, kind=code
	.pub

; Inputs:
;   state.NativeCliOutputFormat selects the artifact/path pair.
;   opasm output artifact layer can render the selected flat output
; Outputs:
;   D0.L = 0 on success, 1 when the output file cannot be opened or written fully
; Clobbers:
;   D0-D4/A0-A1/CCR
; CCR:
;   Reflects D0.L on return
opforgeNativeCliWriteFlatOutput	.block
	movem.l d1-d4/a0-a1, -(sp)
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, state.NativeCliOutputFormat
	beq.s openPrg
	lea state.NativeCliBinPath, a0
	bra.s openSelected

openPrg
	lea state.NativeCliPrgPath, a0

openSelected
	jsr dos.openOutput
	tst.l d0
	beq.s fail
	move.l d0, d4
	cmpi.w #constants.NATIVE_OUTPUT_FORMAT_PRG, state.NativeCliOutputFormat
	bne.s buildBin
	moveq #-1, d2
	tst.w state.NativeCliPrgLoadAddrSet
	beq.s buildPrg
	move.l state.NativeCliPrgLoadAddr, d2

buildPrg
	jsr artifacts.opasmOutputBuildPrgArtifactV1
	bra.s artifactBuilt

buildBin
	jsr artifacts.opasmOutputBuildBinArtifactV1

artifactBuilt
	bne.s closeFail
	move.l d1, d3
	move.l d3, d0
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
