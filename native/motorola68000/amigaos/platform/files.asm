; Generic persistent byte transport. Callers own offset-only image contracts.
; @opforge-owner: opforge.platform.files
	.module opforge.platform.files
	.cpu 68020
	.use opforge.platform.memory as memory
	.pub
FileInfo	.struct
Bytes	.long ?
Days	.long ?
Minutes	.long ?
Ticks	.long ?
	.endstruct
INFO_BYTES = FileInfo.Ticks+4
	.priv
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_READ = -42
DOS_WRITE = -48
DOS_DELETE = -72
DOS_RENAME = -78
DOS_LOCK = -84
DOS_UNLOCK = -90
DOS_EXAMINE = -102
DOS_CREATE_DIR = -120
DOS_IOERR = -132
READ_LOCK = -2
OLD_FILE = 1005
NEW_FILE = 1006
OBJECT_NOT_FOUND = 205
PATH_BYTES = 512
; dos/dos.h classic FileInfoBlock, including its reserved tail.
Fib	.struct
DiskKey	.long ?
EntryType	.long ?
Name	.res 108
Protection	.long ?
Type	.long ?
Bytes	.long ?
Blocks	.long ?
Days	.long ?
Minutes	.long ?
Ticks	.long ?
Comment	.res 80
OwnerUid	.word ?
OwnerGid	.word ?
Reserved	.res 32
	.endstruct
FIB_BYTES = Fib.Reserved+32
	.section code, kind=code
	.pub
; A0=NUL path,A1=FileInfo,A2=DOS. D0/CCR=0 success,1 missing/error.
; Other registers preserved. Rejects directories; output changes only on success.
stamp	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -FIB_BYTES(sp), sp
	movea.l a1, a5
	movea.l a2, a6
	move.l a0, d1
	moveq #READ_LOCK, d2
	jsr DOS_LOCK(a6)
	move.l d0, d4
	beq.w bad
	move.l d4, d1
	move.l sp, d2
	jsr DOS_EXAMINE(a6)
	move.l d0, d5
	move.l d4, d1
	jsr DOS_UNLOCK(a6)
	tst.l d5
	beq.w bad
	tst.l Fib.EntryType(sp)
	bpl.w bad
	move.l Fib.Bytes(sp), FileInfo.Bytes(a5)
	move.l Fib.Days(sp), FileInfo.Days(a5)
	move.l Fib.Minutes(sp), FileInfo.Minutes(a5)
	move.l Fib.Ticks(sp), FileInfo.Ticks(a5)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	lea FIB_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; stamp

; A0=NUL path,A1=memory.Block,A2=DOS. D0/CCR=status; others preserved.
; Reserve <=memory.LIMIT, require exact full read and EOF, always close.
; Used changes only on success; failed reads may modify allocated byte storage.
load	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -20(sp), sp
	movea.l a0, a4
	movea.l a1, a5
	movea.l a2, a6
	movea.l sp, a1
	bsr.w stamp
	bne.w bad
	move.l FileInfo.Bytes(sp), d6
	cmpi.l #memory.LIMIT, d6
	bhi.w bad
	movea.l a5, a0
	move.l d6, d0
	jsr memory.reserveExact
	bne.w bad
	move.l a4, d1
	move.l #OLD_FILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, d7
	beq.w bad
	move.l memory.Block.Pointer(a5), d4
	move.l d6, d5
	moveq #DOS_READ, d0
	bsr.w transfer
	bne.w closeBad
	move.l d7, d1
	lea 16(sp), a0
	move.l a0, d2
	moveq #1, d3
	jsr DOS_READ(a6)
	tst.l d0
	bne.w closeBad
	move.l d7, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	beq.w bad
	move.l d6, memory.Block.Used(a5)
	moveq #0, d0
	bra.w done
closeBad
	move.l d7, d1
	jsr DOS_CLOSE(a6)
bad
	moveq #1, d0
done
	lea 20(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; load

; A0=NUL path,A1=memory.Block,A2=DOS. D0/CCR=status; others preserved.
; Single writer required. Path has a three-byte extension, replaced by .tmp.
; Write/close .tmp, remove old final, then Rename.
; Classic DOS has no atomic replacement: publication failure can lose the old
; disposable cache, but never publishes partial bytes. Under the single-writer
; contract a leftover regular .tmp file is a disposable interrupted write.
save	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -PATH_BYTES(sp), sp
	movea.l a0, a4
	movea.l a1, a5
	movea.l a2, a6
	move.l memory.Block.Used(a5), d5
	cmpi.l #memory.LIMIT, d5
	bhi.w bad
	cmp.l memory.Block.Capacity(a5), d5
	bhi.w bad
	move.l memory.Block.Pointer(a5), d4
	tst.l d5
	beq.w rangeReady
	tst.l d4
	beq.w bad
	move.l d4, d0
	add.l d5, d0
	bcs.w bad
rangeReady
	movea.l sp, a1
	movea.l #0, a3
	move.w #PATH_BYTES-1, d3
copy
	move.b (a0)+, (a1)+
	beq.w copied
	cmpi.b #'.', -1(a1)
	bne.w separator
	movea.l a1, a3
	bra.w nextCharacter
separator
	cmpi.b #'/', -1(a1)
	beq.w resetExtension
	cmpi.b #':', -1(a1)
	bne.w nextCharacter
resetExtension
	movea.l #0, a3
nextCharacter
	dbra d3, copy
	bra.w bad
copied
	; A three-byte final extension keeps classic 30-byte filenames intact.
	move.l a3, d0
	beq.w bad
	lea 4(a3), a0
	cmpa.l a0, a1
	bne.w bad
	; DOS filename matching folds case; final and temporary must differ.
	move.b (a3), d0
	ori.b #$20, d0
	cmpi.b #'t', d0
	bne.w temporary
	move.b 1(a3), d0
	ori.b #$20, d0
	cmpi.b #'m', d0
	bne.w temporary
	move.b 2(a3), d0
	ori.b #$20, d0
	cmpi.b #'p', d0
	beq.w bad
temporary
	move.b #'t', (a3)+
	move.b #'m', (a3)+
	move.b #'p', (a3)
	movea.l sp, a0
	bsr.w discardTemporary
	bne.w bad
	move.l sp, d1
	move.l #NEW_FILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, d7
	beq.w bad
	moveq #DOS_WRITE, d0
	bsr.w transfer
	move.l d0, d6
	move.l d7, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	beq.w discard
	tst.l d6
	bne.w discard
	; Never remove an empty directory supplied as a cache filename.
	lea -16(sp), sp
	movea.l a4, a0
	movea.l sp, a1
	bsr.w stamp
	lea 16(sp), sp
	tst.l d0
	beq.w replace
	movea.l a4, a0
	bsr.w absent
	bne.w discard
	bra.w publish
replace
	move.l a4, d1
	jsr DOS_DELETE(a6)
	tst.l d0
	bne.w publish
	jsr DOS_IOERR(a6)
	cmpi.l #OBJECT_NOT_FOUND, d0
	bne.w discard
publish
	move.l sp, d1
	move.l a4, d2
	jsr DOS_RENAME(a6)
	tst.l d0
	beq.w discard
	moveq #0, d0
	bra.w done
discard
	move.l sp, d1
	jsr DOS_DELETE(a6)
bad
	moveq #1, d0
done
	lea PATH_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; save

; A0=NUL directory path,A2=DOS. D0/CCR=status; others preserved.
; Create missing components, including final; existing components must be dirs.
; Amiga ':' roots and '/' parent components are passed through to DOS.
ensureDirectory	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -PATH_BYTES-FIB_BYTES(sp), sp
	movea.l a2, a6
	movea.l sp, a3
	movea.l sp, a1
	move.w #PATH_BYTES-1, d3
copy
	move.b (a0)+, (a1)+
	beq.w copied
	dbra d3, copy
	bra.w bad
copied
	tst.b (a3)
	beq.w bad
	movea.l a3, a4
scan
	move.b (a4), d5
	beq.w component
	addq.l #1, a4
	cmpi.b #':', d5
	beq.w scan
	cmpi.b #'/', d5
	bne.w scan
	; Empty and parent components need no creation.
	lea 1(a3), a0
	cmpa.l a0, a4
	beq.w scan
	cmpi.b #'/', -2(a4)
	beq.w scan
	cmpi.b #':', -2(a4)
	beq.w scan
	subq.l #1, a4
component
	clr.b (a4)
	move.l a3, d1
	moveq #READ_LOCK, d2
	jsr DOS_LOCK(a6)
	tst.l d0
	bne.w examine
	jsr DOS_IOERR(a6)
	cmpi.l #OBJECT_NOT_FOUND, d0
	bne.w bad
	move.l a3, d1
	jsr DOS_CREATE_DIR(a6)
	tst.l d0
	beq.w bad
examine
	move.l d0, d7
	move.l d7, d1
	lea PATH_BYTES(sp), a0
	move.l a0, d2
	jsr DOS_EXAMINE(a6)
	move.l d0, d6
	move.l d7, d1
	jsr DOS_UNLOCK(a6)
	tst.l d6
	beq.w bad
	tst.l PATH_BYTES+Fib.EntryType(sp)
	ble.w bad
	tst.b d5
	beq.w good
	move.b d5, (a4)+
	bra.w scan
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	lea PATH_BYTES+FIB_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; ensureDirectory
	.priv
; Remove only a regular interrupted cache temporary. A directory or IO error
; is not a disposable partial file. A0=path,A6=DOS; D0/CCR=status, others kept.
discardTemporary	.block
	movem.l d1-d3/a0-a2, -(sp)
	bsr.w absent
	beq.w good
	lea -INFO_BYTES(sp), sp
	movea.l sp, a1
	movea.l a6, a2
	bsr.w stamp
	lea INFO_BYTES(sp), sp
	tst.l d0
	bne.w bad
	move.l a0, d1
	jsr DOS_DELETE(a6)
	tst.l d0
	beq.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; discardTemporary

; A0=path,A6=DOS. D0/CCR=0 only when DOS reports OBJECT_NOT_FOUND.
; Clobbers D1/D2; preserves A0.
absent	.block
	move.l a0, -(sp)
	move.l a0, d1
	moveq #READ_LOCK, d2
	jsr DOS_LOCK(a6)
	tst.l d0
	beq.w missing
	move.l d0, d1
	jsr DOS_UNLOCK(a6)
	bra.w bad
missing
	jsr DOS_IOERR(a6)
	cmpi.l #OBJECT_NOT_FOUND, d0
	bne.w bad
	moveq #0, d0
	movea.l (sp)+, a0
	rts
bad
	moveq #1, d0
	movea.l (sp)+, a0
	rts
	.bend  ; absent
; D0=DOS vector,D7=file,D4=pointer,D5=remaining,A6=DOS.
; D0/CCR=status; clobbers D1-D5/A3; permits empty payloads.
transfer	.block
	movea.l a6, a3
	adda.l d0, a3
	tst.l d5
	beq.w good
loop
	move.l d7, d1
	move.l d4, d2
	move.l d5, d3
	jsr (a3)
	tst.l d0
	ble.w bad
	cmp.l d5, d0
	bhi.w bad
	add.l d0, d4
	sub.l d0, d5
	bne.w loop
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; transfer
	.endsection
	.endmodule
