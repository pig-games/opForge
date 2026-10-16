; Private scratch-file transport. Only successfully CreateDir-owned paths are deleted.
; @opforge-owner: experimental.amigaos.binary_capture_io
	.module experimental.amigaos.binary_capture_io
	.cpu 68020
	.pub
Context	.struct
Dos	.long ?
File	.long ?
Owned	.long ?
End	.long ?
Path	.res 512
.endstruct
CONTEXT_BYTES = Context.Path+512
OPEN = -30
CLOSE = -36
DOS_READ = -42
DOS_WRITE = -48
DELETE = -72
SEEK = -66
UNLOCK = -90
CREATE_DIR = -120
MODE_NEWFILE = 1006
	.section code, kind=code
; A0=zero Context,A1=DOS base,A2=existing parent path. D0/CCR=status.
; Other registers preserved. Atomically owns a fresh directory before opening file.
configure	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l a1, Context.Dos(a5)
	movea.l a1, a6
	lea Context.Path(a5), a3
	movea.l a3, a4
	move.w #450, d4
copy
	move.b (a2)+, (a4)+
	beq.w copied
	dbra d4, copy
	bra.w bad
copied
	subq.l #1, a4
	cmpa.l a3, a4
	beq.w bad
	cmpi.b #':', -1(a4)
	beq.w suffix
	cmpi.b #'/', -1(a4)
	beq.w suffix
	move.b #'/', (a4)+
suffix
	move.l #$6f706667, (a4)+
	move.l a5, d5
	bsr.w hex
	movea.l a4, a2
	moveq #0, d7
attempt
	movea.l a2, a4
	move.l d7, d5
	bsr.w hex
	clr.b (a4)
	move.l a4, Context.End(a5)
	move.l a3, d1
	jsr CREATE_DIR(a6)
	tst.l d0
	bne.w owned
	addq.l #1, d7
	cmpi.l #256, d7
	blo.w attempt
	bra.w bad
owned
	move.l #1, Context.Owned(a5)
	move.l d0, d1
	jsr UNLOCK(a6)
	move.l #$2f636170, (a4)+
	move.l #$74757265, (a4)+
	clr.b (a4)
	move.l a3, d1
	move.l #MODE_NEWFILE, d2
	jsr OPEN(a6)
	move.l d0, Context.File(a5)
	beq.w bad
	moveq #0, d0
	bra.w done
bad
	movea.l a5, a0
	bsr.w cleanup
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; configure

; A0=Context,D1=absolute offset,A1=buffer,D2=positive count. D0/CCR=status.
; Exact transfers reject negative offsets, seek errors and invalid/short results.
; Other registers preserved. Partial failed writes are never committed by owner.
write	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #DOS_WRITE, d7
	bsr.w transfer
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; write
read	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #DOS_READ, d7
	bsr.w transfer
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; read
	.priv
transfer	.block
	movea.l a0, a5
	movea.l Context.Dos(a5), a6
	move.l d1, d5
	bmi.w bad
	move.l d2, d6
	ble.w bad
	add.l d2, d5
	bvs.w bad
	move.l a1, d4
	move.l d1, d2
	move.l Context.File(a5), d1
	beq.w bad
	moveq #-1, d3
	jsr SEEK(a6)
	tst.l d0
	bmi.w bad
	; Verify the resulting position too: Seek returns the previous position.
	move.l Context.File(a5), d1
	moveq #0, d2
	moveq #0, d3
	jsr SEEK(a6)
	sub.l d6, d5
	cmp.l d5, d0
	bne.w bad
loop
	move.l Context.File(a5), d1
	move.l d4, d2
	move.l d6, d3
	movea.l a6, a2
	adda.l d7, a2
	jsr (a2)
	tst.l d0
	ble.w bad
	cmp.l d6, d0
	bhi.w bad
	add.l d0, d4
	sub.l d0, d6
	bne.w loop
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; transfer
	.pub
; A0=Context. Idempotently close and delete only this context's owned paths.
; Preserves registers; CCR unspecified. Clear ownership even if OS removal fails.
cleanup	.block
	movem.l d0-d3/a0-a2/a6, -(sp)
	movea.l a0, a2
	movea.l Context.Dos(a2), a6
	move.l Context.File(a2), d1
	beq.w directory
	clr.l Context.File(a2)
	jsr CLOSE(a6)
directory
	tst.l Context.Owned(a2)
	beq.w done
	clr.l Context.Owned(a2)
	lea Context.Path(a2), a0
	move.l a0, d1
	jsr DELETE(a6)
	movea.l Context.End(a2), a0
	clr.b (a0)
	lea Context.Path(a2), a0
	move.l a0, d1
	jsr DELETE(a6)
done
	movem.l (sp)+, d0-d3/a0-a2/a6
	rts
	.bend  ; cleanup
	.priv
; D5=number,A4=destination. Writes eight hexadecimal digits; clobbers D0/D4/D5.
hex	.block
	moveq #7, d4
loop
	rol.l #4, d5
	move.l d5, d0
	andi.l #15, d0
	addi.b #'0', d0
	cmpi.b #'9', d0
	bls.w digit
	addi.b #7, d0
digit
	move.b d0, (a4)+
	dbra d4, loop
	rts
	.bend  ; hex
	.endsection
	.endmodule
