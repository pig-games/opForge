; Optional disk persistence for leaf objects; all cache failures are nonfatal.
; @opforge-owner: experimental.amigaos.binary_leaf_io
	.module experimental.amigaos.binary_leaf_io
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.pub
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_READ = -42
DOS_WRITE = -48
MODE_OLDFILE = 1005
MODE_NEWFILE = 1006
PATH_BYTES = 256
IO_BYTES = 256
MAGIC = $4f464c31; OFL1, private experimental format
FNV_OFFSET = $811c9dc5
Header	.struct
Magic	.long ?
KeyHigh	.long ?
KeyLow	.long ?
PackageBytes	.long ?
ObjectBytes	.long ?
PathBytes	.long ?
Checksum	.long ?
	.endstruct
HEADER_BYTES = Header.Checksum+4
State	.struct
Dos	.long ?
Directory	.long ?
Path	.res PATH_BYTES
Header	.res HEADER_BYTES
Buffer	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Buffer+memory.Block.Used+4
Local	.struct
KeyHigh	.long ?
KeyLow	.long ?
PackageBytes	.long ?
PathBytes	.long ?
Handle	.long ?
Remaining	.long ?
Chunk	.long ?
SourcePath	.long ?
ObjectPointer	.long ?
ObjectBytes	.long ?
Scratch	.res IO_BYTES
	.endstruct
LOCAL_BYTES = Local.Scratch+IO_BYTES
	.section code, kind=code
	.pub
; Read a matching entry. A0=zeroed State,A1=source path,D1/D2=key halves,
; A2=package bytes,D3=package size. D0=0 hit, 1 miss; Buffer owns a hit.
; Every corrupt, incomplete, oversized or inaccessible entry is a miss.
; Other registers preserved; CCR reflects D0. State.Dos/Directory are borrowed;
; Buffer is owned and may be reused/released by subsequent loads.
load	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.l #LOCAL_BYTES, sp
	movea.l sp, a4
	movea.l a0, a5
	movea.l a1, a3
	move.l d1, Local.KeyHigh(a4)
	move.l d2, Local.KeyLow(a4)
	move.l d3, Local.PackageBytes(a4)
	clr.l Local.Handle(a4)
	clr.l State.Buffer+memory.Block.Used(a5)
	move.l State.Dos(a5), d0
	beq.w miss
	move.l a3, d0
	beq.w miss
	tst.l Local.PackageBytes(a4)
	beq.w loadPointersReady
	move.l a2, d0
	beq.w miss
loadPointersReady
	movea.l a3, a0
	bsr.w pathBytes
	bmi.w miss
	move.l d0, Local.PathBytes(a4)
	bsr.w buildPath
	bne.w miss
	movea.l State.Dos(a5), a6
	move.l #MODE_OLDFILE, d2
	lea State.Path(a5), a0
	move.l a0, d1
	jsr DOS_OPEN(a6)
	tst.l d0
	beq.w miss
	move.l d0, Local.Handle(a4)
	move.l d0, d5
	lea State.Header(a5), a0
	move.l #HEADER_BYTES, d0
	bsr.w readExact
	bne.w missOpen
	cmpi.l #MAGIC, State.Header+Header.Magic(a5)
	bne.w missOpen
	move.l State.Header+Header.KeyHigh(a5), d0
	cmp.l Local.KeyHigh(a4), d0
	bne.w missOpen
	move.l State.Header+Header.KeyLow(a5), d0
	cmp.l Local.KeyLow(a4), d0
	bne.w missOpen
	move.l State.Header+Header.PackageBytes(a5), d0
	cmpi.l #memory.LIMIT, d0
	bhi.w missOpen
	cmp.l Local.PackageBytes(a4), d0
	bne.w missOpen
	move.l State.Header+Header.PathBytes(a5), d0
	cmp.l Local.PathBytes(a4), d0
	bne.w missOpen
	move.l State.Header+Header.ObjectBytes(a5), d0
	cmpi.l #memory.LIMIT, d0
	bhi.w missOpen
	lea Local.Scratch(a4), a0
	move.l State.Header+Header.PathBytes(a5), d0
	bsr.w readExact
	bne.w missOpen
	lea Local.Scratch(a4), a0
	movea.l a3, a1
	move.l Local.PathBytes(a4), d0
	bsr.w comparePath
	bne.w missOpen
	move.l State.Header+Header.PackageBytes(a5), Local.Remaining(a4)
packageLoop
	move.l Local.Remaining(a4), d0
	beq.w packageDone
	cmpi.l #IO_BYTES, d0
	bls.w packageChunk
	move.l #IO_BYTES, d0
packageChunk
	move.l d0, Local.Chunk(a4)
	lea Local.Scratch(a4), a0
	bsr.w readExact
	bne.w missOpen
	lea Local.Scratch(a4), a0
	move.l Local.Chunk(a4), d0
	bsr.w comparePackage
	bne.w missOpen
	move.l Local.Chunk(a4), d0
	sub.l d0, Local.Remaining(a4)
	bra.w packageLoop
packageDone
	move.l State.Header+Header.ObjectBytes(a5), d0
	lea State.Buffer(a5), a0
	jsr memory.reserveExact
	bne.w missOpen
	movea.l State.Buffer+memory.Block.Pointer(a5), a0
	move.l State.Header+Header.ObjectBytes(a5), d0
	bsr.w readExact
	bne.w missOpen
	move.l State.Header+Header.ObjectBytes(a5), memory.Block.Used+State.Buffer(a5)
	movea.l State.Buffer+memory.Block.Pointer(a5), a0
	move.l State.Header+Header.ObjectBytes(a5), d0
	bsr.w checksum
	cmp.l State.Header+Header.Checksum(a5), d0
	bne.w missOpen
	; Require physical EOF so appended or truncated records are rejected.
	movea.l State.Dos(a5), a6
	move.l Local.Handle(a4), d1
	lea Local.Scratch(a4), a0
	move.l a0, d2
	moveq #1, d3
	jsr DOS_READ(a6)
	tst.l d0
	bne.w missOpen
	bsr.w closeFile
	beq.w missOpen
	moveq #0, d0
	bra.w finish
missOpen
	bsr.w closeFile
	lea State.Buffer(a5), a0
	jsr memory.release
	clr.l State.Buffer+memory.Block.Used(a5)
miss
	moveq #1, d0
finish
	adda.l #LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; load
; Save an entry. Inputs match load and A3/D4 identify the object. D0=0 saved,
; 1 skipped/failed. Save failures never affect assembly success.
; Other registers preserved; CCR reflects D0. Existing matching files are
; replaced; incomplete writes remain invalid and load subsequently rejects them.
save	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.l #LOCAL_BYTES, sp
	movea.l sp, a4
	movea.l a0, a5
	move.l a1, Local.SourcePath(a4)
	move.l a3, Local.ObjectPointer(a4)
	movea.l a1, a3
	move.l d1, Local.KeyHigh(a4)
	move.l d2, Local.KeyLow(a4)
	move.l d3, Local.PackageBytes(a4)
	move.l d4, Local.ObjectBytes(a4)
	clr.l Local.Handle(a4)
	move.l State.Dos(a5), d0
	beq.w saveBad
	move.l a3, d0
	beq.w saveBad
	tst.l Local.PackageBytes(a4)
	beq.w savePackageReady
	move.l a2, d0
	beq.w saveBad
savePackageReady
	tst.l Local.ObjectBytes(a4)
	beq.w savePointersReady
	move.l Local.ObjectPointer(a4), d0
	beq.w saveBad
savePointersReady
	movea.l a3, a0
	bsr.w pathBytes
	bmi.w saveBad
	move.l d0, Local.PathBytes(a4)
	cmpi.l #memory.LIMIT, Local.PackageBytes(a4)
	bhi.w saveBad
	cmpi.l #memory.LIMIT, Local.ObjectBytes(a4)
	bhi.w saveBad
	bsr.w buildPath
	bne.w saveBad
	move.l #MAGIC, State.Header+Header.Magic(a5)
	move.l Local.KeyHigh(a4), State.Header+Header.KeyHigh(a5)
	move.l Local.KeyLow(a4), State.Header+Header.KeyLow(a5)
	move.l Local.PackageBytes(a4), State.Header+Header.PackageBytes(a5)
	move.l Local.ObjectBytes(a4), State.Header+Header.ObjectBytes(a5)
	move.l Local.PathBytes(a4), State.Header+Header.PathBytes(a5)
	movea.l Local.ObjectPointer(a4), a0
	move.l Local.ObjectBytes(a4), d0
	bsr.w checksum
	move.l d0, State.Header+Header.Checksum(a5)
	movea.l State.Dos(a5), a6
	lea State.Path(a5), a0
	move.l a0, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	tst.l d0
	beq.w saveBad
	move.l d0, Local.Handle(a4)
	move.l d0, d5
	lea State.Header(a5), a0
	move.l #HEADER_BYTES, d0
	bsr.w writeExact
	bne.w saveOpenBad
	movea.l Local.SourcePath(a4), a0
	move.l Local.PathBytes(a4), d0
	bsr.w writeExact
	bne.w saveOpenBad
	movea.l a2, a0
	move.l Local.PackageBytes(a4), d0
	bsr.w writeExact
	bne.w saveOpenBad
	movea.l Local.ObjectPointer(a4), a0
	move.l Local.ObjectBytes(a4), d0
	bsr.w writeExact
	bne.w saveOpenBad
	bsr.w closeFile
	beq.w saveBad
	moveq #0, d0
	bra.w saveFinish
saveOpenBad
	bsr.w closeFile
saveBad
	moveq #1, d0
saveFinish
	adda.l #LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; save
	.priv
; A0=NUL source path. D0=bytes including NUL, negative if over 256.
pathBytes	.block
	moveq #0, d0
loop
	cmpi.l #PATH_BYTES, d0
	bhs.w bad
	move.b (a0)+, d1
	addq.l #1, d0
	tst.b d1
	bne.w loop
	tst.l d0
	rts
bad
	moveq #-1, d0
	rts
	.bend  ; pathBytes
; A5=State,A3=source path. Build directory/hash filename in State.Path.
; D0/CCR=status; clobbers D0-D2/D6-D7/A0-A1.
buildPath	.block
	move.l #FNV_OFFSET, d7
	movea.l a3, a0
hashLoop
	moveq #0, d0
	move.b (a0)+, d0
	beq.w hashDone
	eor.b d0, d7
	move.l d7, d0
	add.l d7, d0  ; x*2
	add.l d7, d0  ; x*3
	move.l d7, d1
	lsl.l #4, d1
	add.l d1, d0
	move.l d7, d1
	lsl.l #7, d1
	add.l d1, d0
	move.l d7, d1
	lsl.l #8, d1
	add.l d1, d0
	move.l d7, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	add.l d1, d0
	move.l d0, d7
	bra.w hashLoop
hashDone
	move.l State.Directory(a5), d0
	beq.w bad
	movea.l d0, a0
	lea State.Path(a5), a1
	moveq #0, d6
directoryLoop
	move.b (a0)+, d0
	beq.w separator
	cmpi.w #PATH_BYTES-14, d6
	bhs.w bad
	move.b d0, (a1)+
	addq.w #1, d6
	bra.w directoryLoop
separator
	tst.w d6
	beq.w slashReady
	cmpi.b #':', -1(a1)
	beq.w slashReady
	cmpi.b #'/', -1(a1)
	beq.w slashReady
	move.b #'/', (a1)+
	addq.w #1, d6
slashReady
	cmpi.w #242, d6
	bhi.w bad
	lea HexDigits, a0
	moveq #7, d2
hexLoop
	move.l d7, d0
	lsr.l #8, d0
	lsr.l #8, d0
	lsr.l #8, d0
	lsr.l #4, d0
	andi.w #15, d0
	move.b 0(a0, d0.w), (a1)+
	lsl.l #4, d7
	dbra d2, hexLoop
	move.l #$2e6f666c, (a1)+  ; .ofl
	clr.b (a1)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; buildPath
; A5=State, D5=file handle, A0=buffer, D0=bytes. Exact transfer, D0 status.
readExact	.block
	movem.l d1-d4/a0-a2/a6, -(sp)
	movea.l a0, a2
	move.l d0, d4
loop
	tst.l d4
	beq.w good
	movea.l State.Dos(a5), a6
	move.l d5, d1
	move.l a2, d2
	move.l d4, d3
	jsr DOS_READ(a6)
	tst.l d0
	ble.w bad
	cmp.l d4, d0
	bhi.w bad
	adda.l d0, a2
	sub.l d0, d4
	bra.w loop
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a2/a6
	tst.l d0
	rts
	.bend  ; readExact
; A5=State, D5=file handle, A0=buffer, D0=bytes. Exact transfer, D0 status.
writeExact	.block
	movem.l d1-d4/a0-a2/a6, -(sp)
	movea.l a0, a2
	move.l d0, d4
loop
	tst.l d4
	beq.w good
	movea.l State.Dos(a5), a6
	move.l d5, d1
	move.l a2, d2
	move.l d4, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w bad
	cmp.l d4, d0
	bhi.w bad
	adda.l d0, a2
	sub.l d0, d4
	bra.w loop
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a2/a6
	tst.l d0
	rts
	.bend  ; writeExact
; A4=Local,A5=State. Close and clear the owned handle. D0/CCR=AmigaDOS
; close result (nonzero success); D1/A0-A1/A6 scratch. No double close.
closeFile	.block
	move.l Local.Handle(a4), d1
	beq.w closed
	clr.l Local.Handle(a4)
	movea.l State.Dos(a5), a6
	jsr DOS_CLOSE(a6)
	tst.l d0
	rts
closed
	moveq #1, d0
	rts
	.bend  ; closeFile
; A0=data,D0=bytes. Return FNV-1a checksum in D0; other registers
; preserved. No stack-frame or mutable caller-context dependency.
checksum	.block
	movem.l d1-d3/a0, -(sp)
	move.l #FNV_OFFSET, d2
	move.l d0, d3
loop
	tst.l d3
	beq.w done
	moveq #0, d0
	move.b (a0)+, d0
	eor.b d0, d2
	move.l d2, d0
	add.l d2, d0
	add.l d2, d0
	move.l d2, d1
	lsl.l #4, d1
	add.l d1, d0
	move.l d2, d1
	lsl.l #7, d1
	add.l d1, d0
	move.l d2, d1
	lsl.l #8, d1
	add.l d1, d0
	move.l d2, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	add.l d1, d0
	move.l d0, d2
	subq.l #1, d3
	bra.w loop
done
	move.l d2, d0
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; checksum
; A0=read path,A1=expected path,D0=nonzero exact bytes. D0/CCR=status;
; clobbers D1-D2/A0-A1.
comparePath	.block
	moveq #0, d1
loop
	move.b (a0)+, d2
	cmp.b (a1)+, d2
	bne.w bad
	addq.l #1, d1
	cmp.l d0, d1
	blo.w loop
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; comparePath

; A0=read chunk,D0=nonzero bytes,A2=package pointer. D0/CCR=status;
; advances A2 on success. Clobbers D6/A0.
comparePackage	.block
	move.l d0, d6
loop
	move.b (a0)+, d0
	cmp.b (a2)+, d0
	bne.w bad
	subq.l #1, d6
	bne.w loop
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; comparePackage
	.endsection
	.section data, kind=data
HexDigits	.byte "0123456789ABCDEF"
	.endsection
	.endmodule
