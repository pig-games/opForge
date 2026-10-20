; Request-owned images, mutable placements, and public address bindings.
; Disk ownership retains offset-only linker metadata and one final emission image.
; Semantic interface installation belongs to its separate canonical owner.
	.module experimental.amigaos.binary_module_registry
	.cpu 68020
	.include "memory_telemetry.i"
	.use experimental.amigaos.binary_module_object_store as io
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_module_image as image
	.pub
State	.struct
Modules	.res memory.Block.Used+4
Placements	.res memory.Block.Used+4
Addresses	.res memory.Block.Used+4
Configured	.long ?
Cached	.long ?  ; module ID plus one, zero means no valid image
Cache	.res memory.Block.Used+4
Io	.res io.CONTEXT_BYTES
	.endstruct
STATE_BYTES = State.Io+io.CONTEXT_BYTES
Module	.struct
Image	.res memory.Block.Used+4  ; resident or not-yet-stored full image owner
Metadata	.res memory.Block.Used+4  ; disk: validated [0,Header.Payload) copy
ImageBytes	.long ?  ; full serialized extent, independent of Metadata.Used
Stored	.long ?
PlacementStart	.long ?  ; byte offset into State.Placements
Count	.long ?
	.endstruct
MODULE_BYTES = Module.Count+4
Placement	.struct
Base	.long ?
Section	.long ?  ; global zero-based section, $ffffffff until mapped
Known	.long ?
	.endstruct
PLACEMENT_BYTES = Placement.Known+4
Address	.struct
Module	.long ?
Fragment	.long ?
Offset	.long ?
Symbol	.long ?
	.endstruct
ADDRESS_BYTES = Address.Symbol+4
Snapshot	.struct
Modules	.long ?
Placements	.long ?
Addresses	.long ?
	.endstruct
CHECKPOINT_BYTES = Snapshot.Addresses+4
OK = 0
INVALID = 1
UNMAPPED = $ffffffff
; Existing progress schema: reads/writes/cache hits, then byte counts.
PROGRESS_STORE_IO = 451
PROGRESS_STORE_BYTES = 452
	.section code, kind=code
; A0=zero Registry,A1=DOS base,A2=existing temporary directory.
; D0/CCR=status; all other registers preserved. Configure before any add.
; Owns an object output directory; completed .ofm files survive release.
; Unconfigured registries retain resident image ownership.
configure	.block
	movem.l a0/a3, -(sp)
	movea.l a0, a3
	tst.l State.Modules+memory.Block.Used(a3)
	bne.w bad
	tst.l State.Configured(a3)
	bne.w bad
	lea State.Io(a3), a0
	jsr io.configure
	bne.w done
	move.l #1, State.Configured(a3)
	.MEMORY_COUNTER_CLEAR DiskReads
	.MEMORY_COUNTER_CLEAR DiskReadBytes
	.MEMORY_COUNTER_CLEAR DiskWrites
	.MEMORY_COUNTER_CLEAR DiskWriteBytes
	.MEMORY_COUNTER_CLEAR CacheHits
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, a0/a3
	tst.l d0
	rts
	.bend  ; configure

; A0=zero/live Registry,A1=owned immutable image Block. D0/CCR=status;
; success D1=module ID (zero based), consumes A1's complete owner descriptor.
; Disk mode copies linker metadata and retains the full owner until store.
; No IO occurs here; pending borrows serve carrier/public-closure construction.
; Other registers preserved. Validate and reserve every table before committing
; Used lengths; failure leaves caller image ownership and registry rows intact.
; Buffer capacity may grow on failure. New placements are unknown/unmapped.
add	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l memory.Block.Used(a5), d0
	cmp.l memory.Block.Capacity(a5), d0
	bhi.w bad
	movea.l memory.Block.Pointer(a5), a0
	jsr image.validate
	bne.w bad
	movea.l memory.Block.Pointer(a5), a4
	move.l image.Header.BlockCount(a4), d5
	cmpi.l #memory.LIMIT/PLACEMENT_BYTES, d5
	bhi.w bad
	move.l State.Modules+memory.Block.Used(a6), d7
	move.l d7, d0
	moveq #MODULE_BYTES, d1
	divu.l d1, d0
	cmpi.l #65535, d0
	bhi.w bad
	move.l d0, d4
	move.l d7, d0
	addi.l #MODULE_BYTES, d0
	bcs.w bad
	lea State.Modules(a6), a0
	jsr memory.reserve
	bne.w bad
	move.l State.Placements+memory.Block.Used(a6), d6
	move.l d5, d0
	mulu.l #PLACEMENT_BYTES, d0
	add.l d6, d0
	bcs.w bad
	lea State.Placements(a6), a0
	jsr memory.reserve
	bne.w bad
	; Prepare an unpublished row. No fallible work follows metadata copying.
	movea.l State.Modules+memory.Block.Pointer(a6), a3
	adda.l d7, a3
	lea Module.Metadata(a3), a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	clr.l Module.Stored(a3)
	tst.l State.Configured(a6)
	beq.w storageReady
	move.l memory.Block.Used(a5), d0
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	move.l image.Header.Payload(a4), d2
	move.l d2, d0
	jsr memory.reserveExact
	bne.w bad
	move.l d2, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	movea.l a4, a2
copyMetadata
	move.b (a2)+, (a1)+
	subq.l #1, d2
	bne.w copyMetadata
storageReady
	movea.l State.Placements+memory.Block.Pointer(a6), a0
	adda.l d6, a0
	move.l d5, d0
clear
	tst.l d0
	beq.w publish
	clr.l Placement.Base(a0)
	move.l #UNMAPPED, Placement.Section(a0)
	clr.l Placement.Known(a0)
	adda.w #PLACEMENT_BYTES, a0
	subq.l #1, d0
	bra.w clear
publish
	movea.l State.Modules+memory.Block.Pointer(a6), a0
	adda.l d7, a0
	move.l memory.Block.Pointer(a5), Module.Image+memory.Block.Pointer(a0)
	move.l memory.Block.Capacity(a5), Module.Image+memory.Block.Capacity(a0)
	move.l memory.Block.Used(a5), Module.Image+memory.Block.Used(a0)
	move.l memory.Block.Used(a5), Module.ImageBytes(a0)
	move.l d6, Module.PlacementStart(a0)
	move.l d5, Module.Count(a0)
	mulu.l #PLACEMENT_BYTES, d5
	add.l d5, State.Placements+memory.Block.Used(a6)
	addi.l #MODULE_BYTES, State.Modules+memory.Block.Used(a6)
	clr.l memory.Block.Pointer(a5)
	clr.l memory.Block.Capacity(a5)
	clr.l memory.Block.Used(a5)
	move.l d4, d1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; add

; A0=Registry,D1=module ID,D2=fragment ID. Success A1=metadata header,
; A2=descriptor,A3=Placement. D0/CCR=status; other registers preserved.
; No IO. Disk A1 is only Metadata.Used readable bytes, not a full image;
; header/descriptor offsets remain relative to the original full image.
; Metadata/resident image borrows expire on removal/release only. Placement
; borrows expire on any registry table mutation. Failure outputs unspecified.
locate	.block
	movem.l d1-d7/a0/a4-a6, -(sp)
	movea.l a0, a6
	move.l d2, d5
	bsr.w moduleRow
	bne.w bad
	movea.l Module.Image+memory.Block.Pointer(a4), a5
	tst.l State.Configured(a6)
	beq.w metadataReady
	movea.l Module.Metadata+memory.Block.Pointer(a4), a5
metadataReady
	movea.l a5, a0
	move.l d5, d1
	jsr image.descriptorValidated
	bne.w bad
	movea.l a1, a2
	move.l a2, d0
	sub.l a5, d0
	sub.l image.Header.Blocks(a5), d0
	moveq #image.BLOCK_BYTES, d1
	divu.l d1, d0
	cmp.l Module.Count(a4), d0
	bhs.w bad
	mulu.l #PLACEMENT_BYTES, d0
	add.l Module.PlacementStart(a4), d0
	bcs.w bad
	move.l d0, d1
	addi.l #PLACEMENT_BYTES, d1
	bcs.w bad
	cmp.l State.Placements+memory.Block.Used(a6), d1
	bhi.w bad
	movea.l State.Placements+memory.Block.Pointer(a6), a3
	adda.l d0, a3
	movea.l a5, a1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0/a4-a6
	tst.l d0
	rts
	.bend  ; locate

; A0=Registry,D1=module ID. D0/CCR=status,A1=borrowed full image.
; Other registers preserved. No IO; disk borrow expires on successful store,
; removal or release. Resident borrow survives store. Failure A1 unspecified.
pending	.block
	movem.l a4/a6, -(sp)
	movea.l a0, a6
	bsr.w moduleRow
	bne.w done
	movea.l Module.Image+memory.Block.Pointer(a4), a1
	move.l a1, d0
	beq.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, a4/a6
	tst.l d0
	rts
	.bend  ; pending

; A0=Registry,D1=module ID. D0/CCR=status; other registers preserved.
; Disk mode publishes one exact immutable object then frees the pending image.
; Failure retains the pending owner. Repeated store and resident store are no-ops.
store	.block
	movem.l d1-d2/a0-a1/a4/a6, -(sp)
	movea.l a0, a6
	bsr.w moduleRow
	bne.w done
	tst.l State.Configured(a6)
	beq.w good
	tst.l Module.Stored(a4)
	bne.w good
	movea.l Module.Image+memory.Block.Pointer(a4), a1
	move.l a1, d0
	beq.w bad
	move.l Module.ImageBytes(a4), d2
	lea State.Io(a6), a0
	jsr io.store
	bne.w done
	.MEMORY_COUNTER_INC DiskWrites
	.MEMORY_COUNTER_ADD DiskWriteBytes, d2
	move.l #1, Module.Stored(a4)
	lea Module.Image(a4), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d2/a0-a1/a4/a6
	tst.l d0
	rts
	.bend  ; store

; A0=Registry,D1=module ID. D0/CCR=status,A1=full validated image.
; Other registers preserved. Disk requires a stored object. Only this routine
; reads payload. Same-module acquisition preserves the borrow; switching modules
; or rollback/release expires it. Metadata-only locate never expires this borrow.
; Fresh reads validate the full image and match its exact retained metadata.
acquireEmit	.block
	movem.l d1-d4/a0/a2/a4/a6, -(sp)
	movea.l a0, a6
	bsr.w moduleRow
	bne.w bad
	tst.l State.Configured(a6)
	beq.w resident
	tst.l Module.Stored(a4)
	beq.w bad
	move.l d1, d3
	addq.l #1, d3
	cmp.l State.Cached(a6), d3
	bne.w load
	.MEMORY_COUNTER_INC CacheHits
	bra.w cached
load
	clr.l State.Cached(a6)
	lea State.Cache(a6), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	move.l Module.ImageBytes(a4), d2
	beq.w bad
	cmpi.l #memory.LIMIT, d2
	bhi.w bad
	move.l d2, d0
	jsr memory.reserveExact
	bne.w bad
	lea State.Io(a6), a0
	movea.l State.Cache+memory.Block.Pointer(a6), a1
	jsr io.read
	bne.w failedRead
	.MEMORY_COUNTER_INC DiskReads
	.MEMORY_COUNTER_ADD DiskReadBytes, d2
	movea.l a1, a0
	move.l d2, d0
	jsr image.validate
	bne.w failedRead
	movea.l Module.Metadata+memory.Block.Pointer(a4), a2
	move.l Module.Metadata+memory.Block.Used(a4), d4
compareMetadata
	cmpm.b (a1)+, (a2)+
	bne.w failedRead
	subq.l #1, d4
	bne.w compareMetadata
	move.l d2, State.Cache+memory.Block.Used(a6)
	move.l d3, State.Cached(a6)
cached
	movea.l State.Cache+memory.Block.Pointer(a6), a1
	bra.w good
failedRead
	lea State.Cache(a6), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	bra.w bad
resident
	movea.l Module.Image+memory.Block.Pointer(a4), a1
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d4/a0/a2/a4/a6
	tst.l d0
	rts
	.bend  ; acquireEmit

; A0=Registry,D1=module,D2=fragment,D3=offset,D4=global source symbol u16.
; D0/CCR=status; other registers preserved. Bounded one-past labels are legal
; for every section kind. Identical repeated bindings are idempotent; a symbol
; already bound elsewhere rejects. Failure publishes no address row.
bindAddress	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	cmpi.l #65535, d4
	bhi.w bad
	bsr.w locate
	bne.w bad
	cmp.l image.Block.Extent(a2), d3
	bhi.w bad
	move.l State.Addresses+memory.Block.Used(a6), d7
	moveq #0, d6
	movea.l State.Addresses+memory.Block.Pointer(a6), a4
next
	cmp.l d7, d6
	bhs.w append
	cmp.l Address.Symbol(a4), d4
	beq.w existing
	adda.w #ADDRESS_BYTES, a4
	addi.l #ADDRESS_BYTES, d6
	bra.w next
existing
	cmp.l Address.Module(a4), d1
	bne.w bad
	cmp.l Address.Fragment(a4), d2
	bne.w bad
	cmp.l Address.Offset(a4), d3
	bne.w bad
	bra.w good
append
	move.l d7, d0
	addi.l #ADDRESS_BYTES, d0
	bcs.w bad
	lea State.Addresses(a6), a0
	jsr memory.reserve
	bne.w bad
	movea.l State.Addresses+memory.Block.Pointer(a6), a4
	adda.l d7, a4
	move.l d1, Address.Module(a4)
	move.l d2, Address.Fragment(a4)
	move.l d3, Address.Offset(a4)
	move.l d4, Address.Symbol(a4)
	addi.l #ADDRESS_BYTES, State.Addresses+memory.Block.Used(a6)
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; bindAddress

; A0=Registry,A1=writable Snapshot. D0/CCR=OK; other registers preserved.
; Checkpoints cover append-only publication, not edits to existing placements.
checkpoint	.block
	move.l State.Modules+memory.Block.Used(a0), Snapshot.Modules(a1)
	move.l State.Placements+memory.Block.Used(a0), Snapshot.Placements(a1)
	move.l State.Addresses+memory.Block.Used(a0), Snapshot.Addresses(a1)
	moveq #OK, d0
	rts
	.bend  ; checkpoint

; A0=Registry,A1=previous checkpoint from this Registry. D0/CCR=status;
; other registers preserved. Releases images added after checkpoint, then
; truncates all three tables. Existing placement values are not restored.
; Disk expires emission borrows and removes only objects after the checkpoint.
; OS deletion is not transactional: failure retains all row owners and marks
; successfully removed objects unstored. Retry rollback or release after failure.
rollback	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l Snapshot.Modules(a5), d7
	cmp.l State.Modules+memory.Block.Used(a6), d7
	bhi.w bad
	move.l d7, d0
	moveq #MODULE_BYTES, d1
	divu.l d1, d0
	mulu.l #MODULE_BYTES, d0
	cmp.l d7, d0
	bne.w bad
	move.l Snapshot.Placements(a5), d6
	cmp.l State.Placements+memory.Block.Used(a6), d6
	bhi.w bad
	move.l d6, d0
	moveq #PLACEMENT_BYTES, d1
	divu.l d1, d0
	mulu.l #PLACEMENT_BYTES, d0
	cmp.l d6, d0
	bne.w bad
	move.l Snapshot.Addresses(a5), d5
	cmp.l State.Addresses+memory.Block.Used(a6), d5
	bhi.w bad
	move.l d5, d0
	andi.l #ADDRESS_BYTES-1, d0
	bne.w bad
	; The placement prefix must be exactly the retained module prefix.
	moveq #0, d0
	tst.l d7
	beq.w prefixReady
	movea.l State.Modules+memory.Block.Pointer(a6), a4
	adda.l d7, a4
	suba.w #MODULE_BYTES, a4
	move.l Module.Count(a4), d0
	mulu.l #PLACEMENT_BYTES, d0
	add.l Module.PlacementStart(a4), d0
	bcs.w bad
prefixReady
	cmp.l d6, d0
	bne.w bad
	clr.l State.Cached(a6)
	lea State.Cache(a6), a0
	jsr memory.release
	clr.l State.Cache+memory.Block.Used(a6)
	move.l d7, d4
removeObjects
	cmp.l State.Modules+memory.Block.Used(a6), d4
	bhs.w releaseRows
	movea.l State.Modules+memory.Block.Pointer(a6), a4
	adda.l d4, a4
	tst.l Module.Stored(a4)
	beq.w nextObject
	move.l d4, d1
	moveq #MODULE_BYTES, d0
	divu.l d0, d1
	lea State.Io(a6), a0
	jsr io.remove
	bne.w bad
	clr.l Module.Stored(a4)
nextObject
	addi.l #MODULE_BYTES, d4
	bra.w removeObjects
releaseRows
	move.l d7, d4
free
	cmp.l State.Modules+memory.Block.Used(a6), d4
	bhs.w truncate
	movea.l State.Modules+memory.Block.Pointer(a6), a4
	adda.l d4, a4
	lea Module.Image(a4), a0
	jsr memory.release
	lea Module.Metadata(a4), a0
	jsr memory.release
	addi.l #MODULE_BYTES, d4
	bra.w free
truncate
	move.l d7, State.Modules+memory.Block.Used(a6)
	move.l d6, State.Placements+memory.Block.Used(a6)
	move.l d5, State.Addresses+memory.Block.Used(a6)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; rollback

; A0=Registry. D0/CCR=status; other registers preserved. Idempotently releases
; every image/metadata owner and mutable table, leaving a zero State.
; Reports object-store unfinished-file cleanup failures after freeing memory.
; Completed object files and their inspectable directory remain on disk.
release	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	moveq #0, d2
next
	cmp.l State.Modules+memory.Block.Used(a2), d2
	bhs.w tables
	movea.l State.Modules+memory.Block.Pointer(a2), a0
	adda.l d2, a0
	movea.l a0, a1
	lea Module.Image(a1), a0
	jsr memory.release
	lea Module.Metadata(a1), a0
	jsr memory.release
	addi.l #MODULE_BYTES, d2
	bra.w next
tables
	lea State.Cache(a2), a0
	jsr memory.release
	lea State.Io(a2), a0
	jsr io.cleanup
	lea State.Modules(a2), a0
	jsr memory.release
	lea State.Placements(a2), a0
	jsr memory.release
	lea State.Addresses(a2), a0
	jsr memory.release
	movea.l a2, a0
	move.w #STATE_BYTES/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; release
	.priv
; A6=Registry,D1=module ID. D0/CCR=status,A4=Module row; D1 preserved.
moduleRow	.block
	cmpi.l #65535, d1
	bhi.w bad
	move.l d1, d0
	mulu.l #MODULE_BYTES, d0
	move.l d0, -(sp)
	addi.l #MODULE_BYTES, d0
	bcs.w stackedBad
	cmp.l State.Modules+memory.Block.Used(a6), d0
	bhi.w stackedBad
	movea.l State.Modules+memory.Block.Pointer(a6), a4
	move.l (sp)+, d0
	adda.l d0, a4
	moveq #OK, d0
	rts
stackedBad
	addq.l #4, sp
bad
	moveq #INVALID, d0
	rts
	.bend  ; moduleRow
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
	.section bss, kind=bss
DiskReads	.res byte, 4
DiskReadBytes	.res byte, 4
DiskWrites	.res byte, 4
DiskWriteBytes	.res byte, 4
CacheHits	.res byte, 4
	.endsection
.endif
.endif
	.endmodule
