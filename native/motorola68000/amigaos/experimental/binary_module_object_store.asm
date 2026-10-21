; Write-once module objects. Completed files and their directory are deliverables.
; @opforge-owner: experimental.amigaos.binary_module_object_store
	.module experimental.amigaos.binary_module_object_store
	.cpu 68020
	.pub
Context	.struct
Dos	.long ?
OwnedDir	.long ?
File	.long ?
Unfinished	.long ?
Directory	.res 512
Final	.res 512
Temporary	.res 512
	.endstruct
CONTEXT_BYTES = Context.Temporary+512
NAME_BYTES = 32
FILE_NAME_LIMIT = 30
PARENT_LIMIT = 467; leaves room for /objects-255/<30-byte filename> and NUL
DIRECTORY_LIMIT = 479
STEM_BYTES = 21
LIMIT = 1048576
OK = 0
INVALID = 1
	.priv
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_READ = -42
DOS_WRITE = -48
DOS_SEEK = -66
DOS_DELETE = -72
DOS_RENAME = -78
DOS_LOCK = -84
DOS_UNLOCK = -90
DOS_CREATE_DIR = -120
MODE_OLDFILE = 1005
MODE_NEWFILE = 1006
ACCESS_READ = -2
	.section code, kind=code
	.pub
; A0=zero Context,A1=DOS base,A2=existing parent path. D0/CCR=status;
; others preserved. Atomically creates objects, or objects-1 through objects-255.
; Never adopts/deletes an existing directory. No dynamic allocation.
configure	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	tst.l Context.Dos(a5)
	bne.w bad
	move.l a1, d0
	beq.w bad
	move.l a1, Context.Dos(a5)
	movea.l a1, a6
	lea Context.Directory(a5), a4
	move.w #PARENT_LIMIT, d4
copyParent
	move.b (a2)+, (a4)+
	beq.w parentReady
	dbra d4, copyParent
	bra.w reset
parentReady
	subq.l #1, a4
	lea Context.Directory(a5), a0
	cmpa.l a0, a4
	beq.w reset
	cmpi.b #':', -1(a4)
	beq.w suffix
	cmpi.b #'/', -1(a4)
	beq.w suffix
	move.b #'/', (a4)+
suffix
	lea DirectoryName, a0
	bsr.w appendString
	movea.l a4, a3
	moveq #0, d7
attempt
	movea.l a3, a4
	tst.l d7
	beq.w terminate
	move.b #'-', (a4)+
	move.l d7, d5
	cmpi.l #100, d5
	blo.w tens
	move.l d5, d0
	divu.w #100, d0
	move.w d0, d4
	addi.b #'0', d4
	move.b d4, (a4)+
	swap d0
	moveq #0, d5
	move.w d0, d5
tens
	cmpi.l #10, d5
	bhs.w writeTens
	cmpi.l #100, d7
	blo.w units
writeTens
	move.l d5, d0
	divu.w #10, d0
	move.w d0, d4
	addi.b #'0', d4
	move.b d4, (a4)+
	swap d0
	moveq #0, d5
	move.w d0, d5
units
	addi.b #'0', d5
	move.b d5, (a4)+
terminate
	clr.b (a4)
	lea Context.Directory(a5), a0
	move.l a0, d1
	jsr DOS_CREATE_DIR(a6)
	tst.l d0
	bne.w owned
	addq.l #1, d7
	cmpi.l #255, d7
	bls.w attempt
reset
	clr.l Context.Dos(a5)
	clr.b Context.Directory(a5)
	bra.w bad
owned
	move.l #1, Context.OwnedDir(a5)
	move.l d0, d1
	jsr DOS_UNLOCK(a6)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; configure

; A1=qualified module spelling,D2=positive bytes<=255,D1=unique u16 module ID,
; A2=aligned writable NAME_BYTES. D0/CCR=status; other registers preserved. No IO.
; <leaf>[.<parent qualification>]-<4hex ID>.ofm fits classic30-byte filenames.
; The21-byte stem keeps a complete leaf first, then as much parent qualification
; prefix as fits. A leaf longer than21 uses its distinctive final21 bytes and
; omits qualification. Example foo.bar -> bar.foo-0001.ofm; long qualification
; experimental.amigaos.binary_frontend -> binary_frontend.exper-0001.ofm.
; Unique IDs disambiguate case folding, truncation and sanitized characters.
makeName	.block
	movem.l d1-d7/a0-a4, -(sp)
	cmpi.l #65535, d1
	bhi.w bad
	move.l a1, d0
	beq.w bad
	tst.l d2
	beq.w bad
	cmpi.l #255, d2
	bhi.w bad
	add.l d2, d0
	bcs.w bad
	movea.l a1, a0
	movea.l a1, a3
	move.l d2, d6
scan
	tst.b (a0)
	beq.w bad
	cmpi.b #'.', (a0)+
	bne.w next
	movea.l a0, a3
next
	subq.l #1, d6
	bne.w scan
	move.l a3, d7
	sub.l a1, d7
	beq.w parentReady
	subq.l #1, d7
parentReady
	move.l a0, d6
	sub.l a3, d6
	beq.w bad
	cmpi.l #STEM_BYTES, d6
	bls.w leafReady
	movea.l a0, a3
	suba.w #STEM_BYTES, a3
	moveq #STEM_BYTES, d6
	moveq #0, d7
leafReady
	movea.l a2, a4
	moveq #NAME_BYTES/4-1, d0
clear
	clr.l (a4)+
	dbra d0, clear
	movea.l a2, a4
	moveq #STEM_BYTES, d4
leaf
	moveq #0, d0
	move.b (a3)+, d0
	bsr.w safeCharacter
	move.b d0, (a4)+
	subq.l #1, d4
	subq.l #1, d6
	bne.w leaf
	tst.l d7
	beq.w suffix
	cmpi.l #1, d4
	bls.w suffix
	move.b #'.', (a4)+
	subq.l #1, d4
qualification
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w safeCharacter
	move.b d0, (a4)+
	subq.l #1, d7
	beq.w suffix
	subq.l #1, d4
	bne.w qualification
suffix
	move.b #'-', (a4)+
	move.l d1, d5
	bsr.w hex
	lea Extension, a0
	bsr.w appendString
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; makeName

; A0=Context,A2=generated filename,A1=image,D2=positive exact bytes <= LIMIT.
; D0/CCR=status; others preserved. Writes/Closes .tmp then Renames to .ofm.
; Existing final or temporary names reject; single writer owns this directory.
store	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l a1, d4
	move.l d2, d6
	bsr.w paths
	bne.w bad
	bsr.w bufferRange
	bne.w bad
	lea Context.Final(a5), a0
	bsr.w absent
	bne.w bad
	lea Context.Temporary(a5), a0
	bsr.w absent
	bne.w bad
	move.l a0, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, Context.File(a5)
	beq.w bad
	move.l #1, Context.Unfinished(a5)
	moveq #DOS_WRITE, d7
	bsr.w transfer
	bne.w failed
	bsr.w closeFile
	bne.w failed
	lea Context.Temporary(a5), a0
	move.l a0, d1
	lea Context.Final(a5), a0
	move.l a0, d2
	jsr DOS_RENAME(a6)
	tst.l d0
	beq.w failed
	clr.l Context.Unfinished(a5)
	moveq #OK, d0
	bra.w done
failed
	bsr.w discard
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; store

; A0=Context,A2=generated filename,A1=buffer,D2=positive exact bytes <= LIMIT.
; D0/CCR=status; others preserved. Rejects short data and any trailing byte.
; No persistent file cursor; every operation opens and closes its object.
read	.block
	movem.l d1-d7/a0-a6, -(sp)
	subq.l #4, sp
	movea.l a0, a5
	move.l a1, d4
	move.l d2, d6
	bsr.w paths
	bne.w bad
	bsr.w bufferRange
	bne.w bad
	lea Context.Final(a5), a0
	move.l a0, d1
	move.l #MODE_OLDFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, Context.File(a5)
	beq.w bad
	moveq #DOS_READ, d7
	bsr.w transfer
	bne.w failed
	move.l Context.File(a5), d1
	move.l sp, d2
	moveq #1, d3
	jsr DOS_READ(a6)
	tst.l d0
	bne.w failed
	bsr.w closeFile
	bne.w bad
	moveq #OK, d0
	bra.w done
failed
	bsr.w closeFile
bad
	moveq #INVALID, d0
done
	addq.l #4, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; read

; A0=Context,A2=filename,A1=buffer,D2=positive range bytes,D3=offset,
; D4=exact object bytes. D0/CCR=status; others preserved. Opens/closes one
; file; checks exact file extent before reading only the requested bounded range.
readRange	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d3, -(sp)
	move.l d4, -(sp)
	movea.l a0, a5
	move.l a1, d4
	move.l d2, d6
	bsr.w paths
	bne.w bad
	bsr.w bufferRange
	bne.w bad
	move.l (sp), d0
	beq.w bad
	cmpi.l #LIMIT, d0
	bhi.w bad
	move.l 4(sp), d1
	add.l d6, d1
	bcs.w bad
	cmp.l d0, d1
	bhi.w bad
	lea Context.Final(a5), a0
	move.l a0, d1
	move.l #MODE_OLDFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, Context.File(a5)
	beq.w bad
	move.l d0, d1
	moveq #0, d2
	moveq #1, d3
	jsr DOS_SEEK(a6)
	cmpi.l #-1, d0
	beq.w failed
	move.l Context.File(a5), d1
	moveq #0, d2
	moveq #-1, d3
	jsr DOS_SEEK(a6)
	cmp.l (sp), d0
	bne.w failed
	move.l Context.File(a5), d1
	move.l 4(sp), d2
	moveq #-1, d3
	jsr DOS_SEEK(a6)
	cmpi.l #-1, d0
	beq.w failed
	moveq #DOS_READ, d7
	bsr.w transfer
	bne.w failed
	bsr.w closeFile
	bne.w bad
	moveq #OK, d0
	bra.w done
failed
	bsr.w closeFile
bad
	moveq #INVALID, d0
done
	addq.l #8, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; readRange

; A0=Context,A2=caller-owned generated filename. D0/CCR=status; others preserved.
; Rollback deletes that completed object only; caller tracks published names.
remove	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	bsr.w paths
	bne.w bad
	lea Context.Final(a5), a0
	move.l a0, d1
	jsr DOS_DELETE(a6)
	tst.l d0
	beq.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; remove

; A0=Context. D0/CCR=status; others preserved. Close/delete unfinished work
; only; completed objects and Directory remain available as build deliverables.
; Clears live ownership state even on IO failure. Idempotent.
cleanup	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l Context.Dos(a5), a6
	bsr.w discard
	clr.l Context.Dos(a5)
	clr.l Context.OwnedDir(a5)
	clr.l Context.Unfinished(a5)
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; cleanup
	.priv
; A5=Context,A2=generated filename. D0=status,A6=DOS. Construct bounded paths.
; Validate the component length and safe alphabet before appending it.
paths	.block
	tst.l Context.OwnedDir(a5)
	beq.w bad
	movea.l Context.Dos(a5), a6
	move.l a6, d0
	beq.w bad
	tst.l Context.File(a5)
	bne.w bad
	tst.l Context.Unfinished(a5)
	bne.w bad
	move.l a2, d0
	beq.w bad
	movea.l a2, a3
	moveq #0, d5
checkName
	moveq #0, d0
	move.b (a3)+, d0
	beq.w nameReady
	addq.l #1, d5
	cmpi.l #FILE_NAME_LIMIT, d5
	bhi.w bad
	cmpi.b #'.', d0
	beq.w checkName
	cmpi.b #'-', d0
	beq.w checkName
	cmpi.b #'_', d0
	beq.w checkName
	cmpi.b #'0', d0
	blo.w bad
	cmpi.b #'9', d0
	bls.w checkName
	cmpi.b #'A', d0
	blo.w bad
	cmpi.b #'Z', d0
	bls.w checkName
	cmpi.b #'a', d0
	blo.w bad
	cmpi.b #'z', d0
	bhi.w bad
	bra.w checkName
nameReady
	cmpi.l #4, d5
	bls.w bad
	lea Context.Directory(a5), a0
	lea Context.Final(a5), a4
	move.w #DIRECTORY_LIMIT, d0
copy
	move.b (a0)+, (a4)+
	beq.w copied
	dbra d0, copy
	bra.w bad
copied
	subq.l #1, a4
	move.b #'/', (a4)+
	movea.l a2, a0
	bsr.w appendString
	lea Context.Final(a5), a0
	lea Context.Temporary(a5), a1
copyTemporary
	move.b (a0)+, (a1)+
	bne.w copyTemporary
	move.b #'t', -4(a1)
	move.b #'m', -3(a1)
	move.b #'p', -2(a1)
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; paths
; D4=buffer,D6=size. Checked positive bounded range, no wrap.
bufferRange	.block
	tst.l d4
	beq.w bad
	tst.l d6
	ble.w bad
	cmpi.l #LIMIT, d6
	bhi.w bad
	move.l d4, d0
	add.l d6, d0
	bcs.w bad
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; bufferRange
; A0=path,A6=DOS. D0=status; A0 preserved. Lock success means reject.
absent	.block
	move.l a0, -(sp)
	move.l a0, d1
	moveq #ACCESS_READ, d2
	jsr DOS_LOCK(a6)
	tst.l d0
	beq.w missing
	move.l d0, d1
	jsr DOS_UNLOCK(a6)
	moveq #INVALID, d0
	bra.w done
missing
	moveq #OK, d0
done
	movea.l (sp)+, a0
	rts
	.bend  ; absent
; D4=buffer,D6=remaining,D7=Read/Write vector,A5=Context,A6=DOS.
transfer	.block
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
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; transfer
; A5=Context,A6=DOS. D0=status. Clear handle before OS close.
closeFile	.block
	move.l Context.File(a5), d1
	beq.w good
	clr.l Context.File(a5)
	jsr DOS_CLOSE(a6)
	tst.l d0
	beq.w bad
good
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; closeFile
; A5=Context,A6=DOS. D0=status; no completed object is removed.
; Failed deletion retains Unfinished so cleanup can retry the owned path.
discard	.block
	bsr.w closeFile
	move.l d0, d7
	tst.l Context.Unfinished(a5)
	beq.w done
	lea Context.Temporary(a5), a0
	move.l a0, d1
	jsr DOS_DELETE(a6)
	tst.l d0
	beq.w deleteFailed
	clr.l Context.Unfinished(a5)
	bra.w done
deleteFailed
	moveq #INVALID, d7
done
	move.l d7, d0
	rts
	.bend  ; discard
; A0=NUL-terminated text,A4=bounded destination. Leaves A4 at its NUL.
appendString	.block
copy
	move.b (a0)+, (a4)+
	bne.w copy
	subq.l #1, a4
	rts
	.bend  ; appendString
; D0=byte. Preserve safe module spelling; map every unsafe byte to underscore.
; No slash/colon/control byte can escape into the generated filename component.
safeCharacter	.block
	cmpi.b #'.', d0
	beq.w done
	cmpi.b #'_', d0
	beq.w done
	cmpi.b #'-', d0
	beq.w done
	cmpi.b #'0', d0
	blo.w sanitized
	cmpi.b #'9', d0
	bls.w done
	cmpi.b #'A', d0
	blo.w sanitized
	cmpi.b #'Z', d0
	bls.w done
	cmpi.b #'a', d0
	blo.w sanitized
	cmpi.b #'z', d0
	bls.w done
sanitized
	moveq #'_', d0
done
	rts
	.bend  ; safeCharacter
; D5=u16 ID,A4=destination. Writes four hex digits; clobbers D0/D3/D5.
hex	.block
	swap d5
	moveq #3, d3
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
	dbra d3, loop
	rts
	.bend  ; hex
	.endsection
	.section data, kind=data
DirectoryName	.byte "objects", 0
Extension	.byte ".ofm", 0
	.endsection
	.endmodule
