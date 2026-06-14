; Native opasm output artifact builders.

	.module opasm.amigaos.output_artifacts
	.cpu 68020

	.use opasm.amigaos.engine

OPASM_OUTPUT_PRG_BUFFER_CAPACITY = 4098

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

; Build a Commodore PRG artifact from the current engine image.
; Inputs:
; - D2.L = load address, or -1 to use the session origin.
; Outputs:
; - D0.L = 0 on success, 1 on invalid load address.
; - A0 = opasm-owned PRG artifact buffer pointer.
; - D1.L = byte count including the two-byte load address prefix.
opasmOutputBuildPrgArtifactV1	.block
	cmpi.l #$FFFFFFFF, d2
	bne.s haveLoadAddr
	jsr engine.opasmEngineGetSessionOriginV1
	move.l d0, d2

haveLoadAddr
	cmpi.l #$0000FFFF, d2
	bhi.s fail
	lea OpasmPrgArtifactBuffer.l, a2
	move.b d2, (a2)+
	move.l d2, d0
	lsr.w #8, d0
	move.b d0, (a2)+
	jsr opasmOutputBuildBinArtifactV1
	bne.s fail
	move.l d1, d3
	beq.s doneCopy
	subq.l #1, d3

copyLoop
	move.b (a0)+, (a2)+
	dbra d3, copyLoop

doneCopy
	addi.l #2, d1
	lea OpasmPrgArtifactBuffer.l, a0
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; opasmOutputBuildPrgArtifactV1

	.endsection

	.section bss, kind=bss
	.align 4

OpasmPrgArtifactBuffer
	.res byte, OPASM_OUTPUT_PRG_BUFFER_CAPACITY

	.endsection
	.endmodule
