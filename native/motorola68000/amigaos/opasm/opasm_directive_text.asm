; Text directive sizing and emission owner.

	.module opasm.amigaos.directive_text
	.cpu 68020

	.use opasm.amigaos.engine as eng

	.section code, kind=code
	.pub

; Size parsed `.text`, `.null`, or `.ptext` bytes.
; Inputs: D5.W mode; A0 parse callback (returns D3 length); A1 zero-check callback.
; Outputs: D0.L status; D3.L total byte size.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0 on return.
sizeTextDirectiveV1	.block
	movea.l a0, a2
	jsr (a2)
	bne.s fail
	cmpi.w #0, d5
	beq.s ok
	cmpi.w #1, d5
	bne.s ptext
	movea.l a1, a2
	jsr (a2)
	bne.s fail
	addq.l #1, d3
	bra.s ok
ptext
	cmpi.l #255, d3
	bhi.s fail
	addq.l #1, d3
ok
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; sizeTextDirectiveV1

; Emit parsed `.text`, `.null`, or `.ptext` bytes.
; Inputs: D5.W mode; A0 parse callback (returns D3 length); A1 zero-check callback;
;         A2 text scratch pointer.
; Outputs: D0.L status.
; Clobbers: D0-D5/A0-A3/CCR.
; CCR: reflects D0 on return.
emitTextDirectiveV1	.block
	movem.l d1-d5/a0-a3, -(sp)
	movea.l a2, a3
	movea.l a0, a2
	jsr (a2)
	bne.s fail
	move.l d3, d4
	cmpi.w #0, d5
	beq.s bytes
	cmpi.w #1, d5
	beq.s null
	cmpi.l #255, d4
	bhi.s fail
	lea TextPrefix, a0
	move.b d4, (a0)
	moveq #1, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.s fail
	bra.s bytes
null
	movea.l a1, a2
	jsr (a2)
	bne.s fail
bytes
	move.l d4, d0
	beq.s suffix
	movea.l a3, a0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.s fail
suffix
	cmpi.w #1, d5
	bne.s ok
	lea TextPrefix, a0
	clr.b (a0)
	moveq #1, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.s fail
ok
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d5/a0-a3
	rts
	.bend  ; emitTextDirectiveV1

	.endsection

	.section bss, kind=bss
TextPrefix
	.res byte, 1

	.endsection
	.endmodule
