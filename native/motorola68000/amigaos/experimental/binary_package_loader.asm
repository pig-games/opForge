; Package acquisition owns external storage and borrows embedded image bytes.
; @opforge-owner: experimental.amigaos.binary_package_loader
	.module experimental.amigaos.binary_package_loader
	.cpu 68020
	.use experimental.amigaos.binary_catalog as catalog
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_package_validation as validation
	.use experimental.amigaos.binary_memory as memory
	.include "memory_telemetry.i"
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_READ = -42
MODE_OLDFILE = 1005
PATH_BYTES = 256
	.pub
OK = 0
INVALID_PACKAGE = 1
NO_PACKAGE = 2
UNKNOWN_CPU = 3
IO_ERROR = 4
Frame	.struct
Data	.long ?
Bytes	.long ?
Handle	.long ?
DosBase	.long ?
Path	.long ?
Catalog	.long ?
CatalogBytes	.long ?
Cpu	.long ?
Dialect	.long ?
Root	.long ?
Expected	.long ?
AllowTrailing	.word ?
Reserved	.word ?
Storage	.long ?  ; zero external, one borrowed embedded image
OwnedPointer	.long ?
OwnedCapacity	.long ?
OwnedUsed	.long ?
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
ReadCalls	.long ?
.endif
.endif
End	.byte ?
	.endstruct
FRAME_BYTES = Frame.End
	.section code, kind=code
; A0=zero-initialized Frame. Cpu requests catalog lookup; otherwise Path is an
; explicit package file. DosBase must be open. D0/CCR=status; other registers
; preserved. On success Data/Bytes are readable until release. A manifest caller
; may set AllowTrailing and take Handle, positioned just beyond the package.
acquire	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	.MEMORY_COUNTER_CLEAR Frame.ReadCalls(a5)
	clr.l Frame.Expected(a5)
	clr.l Frame.Storage(a5)
	move.l Frame.DosBase(a5), d0
	beq.w invalid
	move.l Frame.Cpu(a5), d0
	beq.w file
	movea.l Frame.Catalog(a5), a0
	move.l Frame.CatalogBytes(a5), d0
	movea.l Frame.Cpu(a5), a1
	movea.l Frame.Dialect(a5), a2
	jsr catalog.find
	bne.w unknown
	movea.l Frame.Catalog(a5), a4
	move.l catalog.Entry.Key(a1), d0
	lea 0(a4, d0.l), a0
	move.l a0, Frame.Expected(a5)
	move.l catalog.Entry.PayloadBytes(a1), d0
	beq.w external
	move.l d0, Frame.Bytes(a5)
	move.l catalog.Entry.Payload(a1), d1
	lea 0(a4, d1.l), a0
	move.l a0, Frame.Data(a5)
	move.l #1, Frame.Storage(a5)
	bra.w validate
external
	movea.l Frame.Root(a5), a0
	move.l a0, d0
	beq.w invalid
	lea Path, a1
	move.w #PATH_BYTES-1, d2
rootLoop
	move.b (a0)+, d0
	beq.w separator
	tst.w d2
	beq.w invalid
	move.b d0, (a1)+
	subq.w #1, d2
	bra.w rootLoop
separator
	cmpi.w #PATH_BYTES-1, d2  ; an empty root consumed no bytes
	beq.w invalid
	cmpi.b #':', -1(a1)
	beq.w key
	cmpi.b #'/', -1(a1)
	beq.w key
	tst.w d2
	beq.w invalid
	move.b #'/', (a1)+
	subq.w #1, d2
key
	movea.l Frame.Expected(a5), a0
keyLoop
	move.b (a0)+, d0
	beq.w extension
	tst.w d2
	beq.w invalid
	move.b d0, (a1)+
	subq.w #1, d2
	bra.w keyLoop
extension
	cmpi.w #4, d2
	blo.w invalid
	move.l #$2e62696e, (a1)+  ; .bin
	clr.b (a1)
	move.l #Path, Frame.Path(a5)
file
	move.l Frame.Path(a5), d1
	beq.w invalid
	movea.l Frame.DosBase(a5), a6
	move.l #MODE_OLDFILE, d2
	jsr DOS_OPEN(a6)
	tst.l d0
	beq.w missing
	move.l d0, Frame.Handle(a5)
	move.l #Header, d2
	move.l #package.HEADER_BYTES, d3
	bsr.w readExact
	bne.w ioFailure
	lea Header, a4
	cmpi.l #package.MAGIC, package.Header.Magic(a4)
	bne.w invalid
	move.l package.Header.Bytes(a4), d0
	cmpi.l #package.HEADER_BYTES, d0
	blo.w invalid
	cmpi.l #memory.LIMIT, d0
	bhi.w invalid
	move.l d0, Frame.Bytes(a5)
	lea Frame.OwnedPointer(a5), a0
	jsr memory.reserveExact
	bne.w ioFailure
	move.l Frame.OwnedPointer(a5), Frame.Data(a5)
	movea.l Frame.Data(a5), a1
	lea Header, a0
	move.l #package.HEADER_BYTES, d0
copyHeader
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyHeader
	move.l a1, d2
	move.l Frame.Bytes(a5), d3
	subi.l #package.HEADER_BYTES, d3
	bsr.w readExact
	bne.w ioFailure
	move.l Frame.Bytes(a5), Frame.OwnedUsed(a5)
	tst.w Frame.AllowTrailing(a5)
	bne.w validate
	move.l Frame.Handle(a5), d1
	move.l #EndByte, d2
	moveq #1, d3
	.MEMORY_COUNTER_INC Frame.ReadCalls(a5)
	jsr DOS_READ(a6)
	tst.l d0
	bne.w invalid
	bsr.w close
	tst.l d0
	beq.w ioFailure
validate
	movea.l Frame.Data(a5), a0
	move.l Frame.Bytes(a5), d0
	movea.l Frame.Expected(a5), a1
	jsr validation.validate
	bne.w invalid
	moveq #OK, d0
	bra.w done
invalid
	moveq #INVALID_PACKAGE, d0
	bra.w done
missing
	moveq #NO_PACKAGE, d0
	bra.w done
unknown
	moveq #UNKNOWN_CPU, d0
	bra.w done
ioFailure
	moveq #IO_ERROR, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; acquire

; A0=Frame. Close retained handle, release only owned allocation, clear result.
; Preserves all registers; CCR unspecified. Safe after failed acquire.
release	.block
	movem.l d0-d2/a0-a1/a5-a6, -(sp)
	movea.l a0, a5
	bsr.w close
	lea Frame.OwnedPointer(a5), a0
	jsr memory.release
	clr.l Frame.OwnedUsed(a5)
	clr.l Frame.Data(a5)
	clr.l Frame.Bytes(a5)
	clr.l Frame.Storage(a5)
	movem.l (sp)+, d0-d2/a0-a1/a5-a6
	rts
	.bend  ; release
	.priv
; A5=Frame,D2=destination,D3=bytes. D0/CCR=status; preserves other registers.
readExact	.block
	movem.l d1-d4/a6, -(sp)
	movea.l Frame.DosBase(a5), a6
loop
	tst.l d3
	beq.w good
	move.l d3, d4
	move.l Frame.Handle(a5), d1
	.MEMORY_COUNTER_INC Frame.ReadCalls(a5)
	jsr DOS_READ(a6)
	tst.l d0
	ble.w bad
	cmp.l d4, d0
	bhi.w bad
	add.l d0, d2
	sub.l d0, d3
	bra.w loop
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a6
	tst.l d0
	rts
	.bend  ; readExact
; A5=Frame. D0/D1/A6 clobbered, CCR unspecified.
close	.block
	move.l Frame.Handle(a5), d1
	beq.w done
	clr.l Frame.Handle(a5)
	movea.l Frame.DosBase(a5), a6
	jsr DOS_CLOSE(a6)
done
	rts
	.bend  ; close
	.endsection
	.section bss, kind=bss
	.align 4
Header	.res byte, package.HEADER_BYTES
Path	.res byte, PATH_BYTES
EndByte	.res byte, 1
	.endsection
	.endmodule
