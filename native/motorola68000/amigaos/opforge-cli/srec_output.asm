; Native AmigaOS Motorola S-record payload writer.

	.module opforge.cli.srec_output
	.cpu 68020

	.use opasm.amigaos.engine as engine
	.use opforge.cli.constants

NATIVE_SREC_LINE_DATA_BYTES = 32
NATIVE_SREC_BUFFER_CAPACITY = (constants.NATIVE_IMAGE_BUFFER_CAPACITY * 3) + 128

	.section code, kind=code
	.pub

; Build Rust ImageStore::write_srec_file for the current contiguous image.
; Rust currently emits no S0 or count record: only S1/S2/S3 data records in
; 32-byte contiguous chunks and one matching S9/S8/S7 termination record.
; @opforge-owner: opforge.cli.srec_output
; @opforge-slice: documentation/plans/slices/native-porting-slice-srec-output-v1.toml
; @opforge-role: implementation
; Inputs: D0.W=1 when a start address is supplied; D1=start address.
; Outputs: D0=0 success/1 overflow; A0=payload; D1=payload byte count.
buildFlatV1	.block
	movem.l d2-d7/a2-a4, -(sp)
	lea -8(sp), sp
	move.w d0, (sp)
	move.l d1, 4(sp)
	jsr engine.opasmEngineGetSessionOriginV1
	move.l d0, d6
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d5
	move.l d5, d0
	move.l d0, d1
	add.l d0, d0
	bcs.w fail
	add.l d1, d0
	bcs.w fail
	addi.l #128, d0
	cmpi.l #NATIVE_SREC_BUFFER_CAPACITY, d0
	bhi.w fail

	move.l d6, d7
	tst.l d5
	beq.s maxWithStart
	add.l d5, d7
	bcs.w fail
	subq.l #1, d7
maxWithStart
	tst.w (sp)
	beq.s addressWidth
	cmp.l 4(sp), d7
	bhs.s addressWidth
	move.l 4(sp), d7
addressWidth
	moveq #2, d4
	cmpi.l #$0000ffff, d7
	bls.s widthReady
	moveq #3, d4
	cmpi.l #$00ffffff, d7
	bls.s widthReady
	moveq #4, d4
widthReady
	lea NativeSrecOutputBuffer.l, a2
	jsr engine.opasmEngineGetImageBufferPtrV1
	movea.l a0, a3
	jsr engine.opasmEngineGetImagePresentBufferPtrV1
	movea.l a0, a4

recordLoop
	tst.l d5
	beq.s termination
	tst.b (a4)
	bne.s recordStart
	addq.l #1, a3
	addq.l #1, a4
	addq.l #1, d6
	subq.l #1, d5
	bra.s recordLoop

recordStart
	moveq #0, d1
recordLengthLoop
	cmpi.l #NATIVE_SREC_LINE_DATA_BYTES, d1
	bhs.s recordLengthReady
	cmp.l d5, d1
	bhs.s recordLengthReady
	tst.b 0(a4, d1.l)
	beq.s recordLengthReady
	addq.l #1, d1
	bra.s recordLengthLoop
recordLengthReady
	move.l d6, d0
	move.l d4, d2
	move.l d4, d3
	addi.b #'/', d3
	movea.l a3, a0
	bsr.w emitRecordV1
	adda.l d1, a3
	adda.l d1, a4
	add.l d1, d6
	sub.l d1, d5
	bra.s recordLoop

termination
	moveq #0, d0
	tst.w (sp)
	beq.s terminationAddressReady
	move.l 4(sp), d0
terminationAddressReady
	moveq #0, d1
	move.l d4, d2
	moveq #11, d3
	sub.w d4, d3
	addi.b #'0', d3
	suba.l a0, a0
	bsr.w emitRecordV1
	lea NativeSrecOutputBuffer.l, a0
	move.l a2, d1
	sub.l a0, d1
	moveq #0, d0
	bra.s return
fail
	suba.l a0, a0
	moveq #0, d1
	moveq #1, d0
return
	lea 8(sp), sp
	movem.l (sp)+, d2-d7/a2-a4
	tst.l d0
	rts
	.bend  ; buildFlatV1

	.priv

; Emit one Rust-format S-record and advance A2.
; Inputs: A0=data; D0=address; D1=data bytes; D2=address bytes; D3=type ASCII.
emitRecordV1	.block
	movem.l d0-d7/a0-a1/a3-a4, -(sp)
	move.l d0, d6
	move.l d1, d5
	move.l d2, d4
	move.b #'S', (a2)+
	move.b d3, (a2)+
	move.l d4, d0
	add.l d5, d0
	addq.l #1, d0
	move.l d0, d7
	bsr.w emitHexByteV1

	cmpi.w #4, d4
	bne.s maybeAddress24
	move.l d6, d0
	lsr.l #8, d0
	lsr.l #8, d0
	lsr.l #8, d0
	add.b d0, d7
	bsr.w emitHexByteV1
maybeAddress24
	cmpi.w #3, d4
	blo.s address16
	move.l d6, d0
	lsr.l #8, d0
	lsr.l #8, d0
	add.b d0, d7
	bsr.w emitHexByteV1
address16
	move.l d6, d0
	lsr.l #8, d0
	add.b d0, d7
	bsr.w emitHexByteV1
	move.l d6, d0
	add.b d0, d7
	bsr.w emitHexByteV1

dataLoop
	tst.l d5
	beq.s checksum
	moveq #0, d0
	move.b (a0)+, d0
	add.b d0, d7
	bsr.w emitHexByteV1
	subq.l #1, d5
	bra.s dataLoop
checksum
	moveq #-1, d0
	sub.b d7, d0
	bsr.w emitHexByteV1
	move.b #10, (a2)+
	movem.l (sp)+, d0-d7/a0-a1/a3-a4
	rts
	.bend  ; emitRecordV1

emitHexByteV1	.block
	movem.l d0-d2/a1, -(sp)
	andi.l #$ff, d0
	lea NativeSrecHexDigits.l, a1
	move.l d0, d1
	lsr.b #4, d1
	move.b 0(a1, d1.l), (a2)+
	andi.b #$0f, d0
	move.b 0(a1, d0.l), (a2)+
	movem.l (sp)+, d0-d2/a1
	rts
	.bend  ; emitHexByteV1

	.endsection

	.section data, kind=data

NativeSrecHexDigits
	.byte "0123456789ABCDEF"

	.endsection

	.section bss, kind=bss
	.align 4

NativeSrecOutputBuffer
	.res byte, NATIVE_SREC_BUFFER_CAPACITY

	.endsection
	.endmodule
