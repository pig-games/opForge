; Native opasm output artifact builders.

	.module opasm.amigaos.output_artifacts
	.cpu 68020

	.use opasm.amigaos.engine

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Build the first-run flat `.bin` artifact payload from the opasm image.
;
; Inputs:
; - opasm engine image buffer/count contain the assembled flat output.
;
; Outputs:
; - D0.L: status, 0 on success.
; - D1.L: artifact byte count.
; - A0: artifact payload pointer.
;
; Clobbers:
; - D0-D1/A0-A1/CCR
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
opasmOutputBuildBinArtifactV1	.block
	jsr engine.opasmEngineGetImageBufferPtrV1
	movea.l a0, a1
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d1
	movea.l a1, a0
	moveq #0, d0
	rts
	.bend  ; opasmOutputBuildBinArtifactV1

	.endsection
	.endmodule
