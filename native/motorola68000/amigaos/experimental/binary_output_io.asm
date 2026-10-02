; File transport only; containers and source declarations belong to the caller.
; @opforge-owner: experimental.amigaos.binary_output_io
	.module experimental.amigaos.binary_output_io
	.cpu 68020
	.pub
Frame	.struct
Path	.long ?
Data	.long ?
Bytes	.long ?
Prefix	.long ?
PrefixBytes	.long ?
Generator	.long ?
Context	.long ?
	.endstruct
FRAME_BYTES = Frame.Context+4
OPEN = -30
CLOSE = -36
DOS_WRITE = -48
LOCK = -84
UNLOCK = -90
CREATE_DIR = -120
READ_LOCK = -2
PATH_BYTES = 256
NEW_FILE = 1006
	.section code, kind=code
	.pub
; A0=Frame,A6=dos.library. Create/truncate a file, write optional container
; prefix then payload, and close on every opened path. Handles short writes;
; a zero/negative/oversized write or failed close returns D0/CCR=1, else 0.
; Path is NUL-terminated within PATH_BYTES. Creates missing parent directories.
; Optional Generator(A0=Context) returns D0/CCR ITEM=0,A0=data,D1=bytes;
; END=1 or error>=2, preserving other registers. Fixed Data/Bytes are used only
; without a generator. Other registers preserved; no ownership is transferred.
write	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l Frame.Path(a5), d1
	beq.w bad
	bsr.w parents
	bne.w bad
	move.l Frame.Path(a5), d1
	move.l #NEW_FILE, d2
	jsr OPEN(a6)
	tst.l d0
	beq.w bad
	move.l d0, d4
	move.l Frame.Prefix(a5), d5
	move.l Frame.PrefixBytes(a5), d6
	bsr.w bytes
	bne.w closeBad
	move.l Frame.Generator(a5), d7
	beq.w fixed
chunk
	movea.l d7, a2
	movea.l Frame.Context(a5), a0
	jsr (a2)
	cmpi.l #1, d0
	beq.w finishFile
	tst.l d0
	bne.w closeBad
	tst.l d1
	ble.w closeBad
	move.l a0, d5
	move.l d1, d6
	bsr.w bytes
	bne.w closeBad
	bra.w chunk
fixed
	move.l Frame.Data(a5), d5
	move.l Frame.Bytes(a5), d6
	bsr.w bytes
	bne.w closeBad
finishFile
	move.l d4, d1
	jsr CLOSE(a6)
	tst.l d0
	beq.w bad
	moveq #0, d0
	bra.w done
closeBad
	move.l d4, d1
	jsr CLOSE(a6)
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; write
	.priv
; D1=path,A6=dos.library; D0/CCR=status, other registers preserved.
; Copy before temporarily terminating each parent prefix; never mutate caller data.
parents	.block
	movem.l d1-d3/a0-a2, -(sp)
	lea -PATH_BYTES(sp), sp
	movea.l d1, a0
	movea.l sp, a1
	moveq #0, d3
copy
	cmpi.w #PATH_BYTES, d3
	bhs.w bad
	move.b (a0)+, (a1)+
	addq.w #1, d3
	tst.b -1(a1)
	bne.w copy
	movea.l sp, a2
scan
	move.b (a2)+, d0
	beq.w good
	cmpi.b #'/', d0
	bne.w scan
	; A leading slash denotes an AmigaDOS parent, not an empty directory name.
	lea 1(sp), a0
	cmpa.l a0, a2
	beq.w scan
	clr.b -1(a2)
	move.l sp, d1
	moveq #READ_LOCK, d2
	jsr LOCK(a6)
	tst.l d0
	bne.w release
	move.l sp, d1
	jsr CREATE_DIR(a6)
	tst.l d0
	beq.w bad
release
	move.l d0, d1
	jsr UNLOCK(a6)
	move.b #'/', -1(a2)
	bra.w scan
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	lea PATH_BYTES(sp), sp
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; parents
; D4=file,D5=pointer,D6=bytes; D0/CCR=status, preserves D4-D6/A5-A6.
bytes	.block
	tst.l d6
	beq.w empty
	tst.l d5
	beq.w bad
loop
	move.l d4, d1
	move.l d5, d2
	move.l d6, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w bad
	cmp.l d6, d0
	bhi.w bad
	add.l d0, d5
	sub.l d0, d6
	bne.w loop
empty
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; bytes
	.endsection
	.endmodule
