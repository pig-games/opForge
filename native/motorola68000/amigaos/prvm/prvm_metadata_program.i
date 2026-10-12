; Validate the one current counted metadata program before record execution.
; A4=request,A5=program. D0/CCR=status,D7=target count; clobbers D1/D2/D5/A2.
validateEnvelope	.block
	cmpi.l #abi.METADATA_PROGRAM_ROWS+abi.METADATA_PROGRAM_TRAILER_BYTES, abi.PRVM_FRAME_PROGRAM_LEN(a4)
	blo.w bad
	cmpi.b #abi.OP_METADATA_SELECT, (a5)
	bne.w bad
	tst.b abi.MetadataProgram.Reserved(a5)
	bne.w bad
	moveq #0, d5
	move.w abi.MetadataProgram.Count(a5), d5
	beq.w bad
	moveq #0, d7
	move.w abi.MetadataProgram.TargetCount(a5), d7
	moveq #0, d0
	move.w abi.MetadataProgram.SelectedTarget(a5), d0
	cmp.l d7, d0
	bhi.w bad
	move.l d5, d0
	mulu.w #abi.METADATA_ROW_BYTES, d0
	addi.l #abi.METADATA_PROGRAM_ROWS, d0
	lea 0(a5, d0.l), a2
	addq.l #abi.METADATA_PROGRAM_TRAILER_BYTES, d0
	cmp.l abi.PRVM_FRAME_PROGRAM_LEN(a4), d0
	bne.w bad
	cmpi.b #abi.OP_PUBLISH, (a2)
	bne.w bad
	tst.b 1(a2)
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; validateEnvelope

; Selected-row policy validation; A2=row,D7=target count.
validateRow	.block
	move.b abi.MetadataRow.Contexts(a2), d0
	beq.w bad
	andi.b #$f0, d0
	bne.w bad
	moveq #0, d0
	move.w abi.MetadataRow.Target(a2), d0
	cmp.l d7, d0
	bhi.w bad
	cmpi.b #abi.PRVM_METADATA_OUTPUT, abi.MetadataRow.Role(a2)
	blo.w bad
	cmpi.b #abi.PRVM_METADATA_TARGET_CLOSE, abi.MetadataRow.Role(a2)
	bhi.w bad
	cmpi.b #abi.PRVM_METADATA_DESCRIPTIVE, abi.MetadataRow.Role(a2)
	bls.w value
	tst.b abi.MetadataRow.Key(a2)
	bne.w bad
	cmpi.b #abi.PRVM_METADATA_OPERAND_NONE, abi.MetadataRow.Operand(a2)
	bne.w bad
	cmpi.b #abi.PRVM_METADATA_TARGET_OPEN, abi.MetadataRow.Role(a2)
	blo.w good
	tst.w abi.MetadataRow.Target(a2)
	beq.w bad
	bra.w good
value
	cmpi.b #abi.PRVM_METADATA_NAME, abi.MetadataRow.Key(a2)
	blo.w bad
	cmpi.b #abi.PRVM_METADATA_VERSION, abi.MetadataRow.Key(a2)
	bhi.w bad
	cmpi.b #abi.PRVM_METADATA_OPERAND_FILL, abi.MetadataRow.Operand(a2)
	bhi.w bad
	cmpi.b #abi.PRVM_METADATA_OPERAND_RESERVED, abi.MetadataRow.Operand(a2)
	beq.w bad
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; validateRow

	.pub
; A0=program,D0=bytes. Validate full sorted table once at package acquisition.
; D0/CCR=status; all other registers preserved.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #abi.PRVM_REQUEST_FRAME_SIZE, sp
	movea.l sp, a4
	movea.l a0, a5
	move.l d0, abi.PRVM_FRAME_PROGRAM_LEN(a4)
	bsr.w validateEnvelope
	bne.w done
	lea abi.METADATA_PROGRAM_ROWS(a5), a2
	moveq #0, d5
	move.w abi.MetadataProgram.Count(a5), d5
	subq.w #1, d5
	moveq #0, d4
	moveq #-1, d6  ; first head has no predecessor
row
	moveq #0, d2
	move.b abi.MetadataRow.HeadHigh(a2), d2
	lsl.w #8, d2
	move.b abi.MetadataRow.HeadLow(a2), d2
	tst.l d6
	bmi.w first
	cmp.l d6, d2
	blo.w bad
	beq.w sameHead
first
	clr.l d4
sameHead
	move.l d2, d6
	moveq #0, d0
	move.b abi.MetadataRow.Contexts(a2), d0
	move.l d0, d1
	and.l d4, d1
	bne.w bad
	or.l d0, d4
	bsr.w validateRow
	bne.w done
	addq.l #abi.METADATA_ROW_BYTES, a2
	dbra d5, row
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #abi.PRVM_REQUEST_FRAME_SIZE, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate
	.priv
