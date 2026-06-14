; Native opasm output artifact builders.

	.module opasm.amigaos.output_artifacts
	.cpu 68020

	.use opasm.amigaos.engine

OPASM_OUTPUT_PRG_BUFFER_CAPACITY = 4098
OPASM_OUTPUT_HEX_BUFFER_CAPACITY = 12000

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

; Build an Intel HEX artifact from the current contiguous engine image.
; Outputs:
; - D0.L = 0 on success, 1 when output exceeds 16-bit HEX address range.
; - A0 = opasm-owned HEX artifact buffer pointer.
; - D1.L = text byte count.
opasmOutputBuildHexArtifactV1	.block
	movem.l d2-d7/a2-a3, -(sp)
	jsr engine.opasmEngineGetSessionOriginV1
	cmpi.l #$0000FFFF, d0
	bhi.w fail
	move.l d0, d6
	jsr opasmOutputBuildBinArtifactV1
	bne.w fail
	movea.l a0, a3
	move.l d1, d5
	lea OpasmHexArtifactBuffer.l, a2
	tst.l d5
	beq.w eofRecord
	move.l d6, d0
	add.l d5, d0
	subq.l #1, d0
	cmpi.l #$0000FFFF, d0
	bhi.w fail

recordLoop
	move.l #255, d7
	cmp.l d7, d5
	bhs.s haveRecordLen
	move.l d5, d7

haveRecordLen
	move.b #':', (a2)+
	move.l d7, d3
	move.l d7, d0
	bsr.w opasmOutputEmitHexByte
	move.l d6, d0
	lsr.w #8, d0
	andi.l #$000000FF, d0
	add.l d0, d3
	bsr.w opasmOutputEmitHexByte
	move.l d6, d0
	andi.l #$000000FF, d0
	add.l d0, d3
	bsr.w opasmOutputEmitHexByte
	moveq #0, d0
	bsr.w opasmOutputEmitHexByte
	move.l d7, d4
	subq.l #1, d4

dataLoop
	moveq #0, d0
	move.b (a3)+, d0
	add.l d0, d3
	bsr.w opasmOutputEmitHexByte
	dbra d4, dataLoop
	move.l d3, d0
	neg.l d0
	andi.l #$000000FF, d0
	bsr.w opasmOutputEmitHexByte
	move.b #10, (a2)+
	sub.l d7, d5
	add.l d7, d6
	tst.l d5
	bne.w recordLoop

eofRecord
	lea OpasmHexEofRecord.l, a0
	moveq #11, d0

copyEof
	move.b (a0)+, (a2)+
	dbra d0, copyEof
	lea OpasmHexArtifactBuffer.l, a0
	move.l a2, d1
	move.l a0, d0
	sub.l d0, d1
	moveq #0, d0
	movem.l (sp)+, d2-d7/a2-a3
	rts

fail
	moveq #1, d0
	movem.l (sp)+, d2-d7/a2-a3
	rts
	.bend  ; opasmOutputBuildHexArtifactV1

; Append one byte as two uppercase hexadecimal characters.
; Inputs: D0.B = byte; A2 = destination cursor.
; Outputs: A2 advanced by two bytes.
opasmOutputEmitHexByte	.block
	movem.l d0-d2/a1, -(sp)
	andi.l #$000000FF, d0
	lea OpasmHexDigits.l, a1
	move.l d0, d1
	lsr.b #4, d1
	move.b 0(a1, d1.l), (a2)+
	andi.b #$0F, d0
	move.b 0(a1, d0.l), (a2)+
	movem.l (sp)+, d0-d2/a1
	rts
	.bend  ; opasmOutputEmitHexByte

	.endsection

	.section data, kind=data

OpasmHexDigits
	.byte "0123456789ABCDEF"
OpasmHexEofRecord
	.byte ":00000001FF", 10

	.endsection

	.section bss, kind=bss
	.align 4

OpasmPrgArtifactBuffer
	.res byte, OPASM_OUTPUT_PRG_BUFFER_CAPACITY
OpasmHexArtifactBuffer
	.res byte, OPASM_OUTPUT_HEX_BUFFER_CAPACITY

	.endsection
	.endmodule
