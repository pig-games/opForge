; Cache ownership handoff, short-page bounds and failed transport cleanup.
; @opforge-owner: tests.amigaos.prepared_store_reuse_probe
	.module tests.amigaos.prepared_store_reuse_probe
	.cpu 68020
	.use experimental.amigaos.binary_prepared_store as store
	.use experimental.amigaos.binary_capture_io as io
	.use experimental.amigaos.binary_memory as memory
RECORD_BYTES = 256
RECORDS_PER_PAGE = store.PAGE_BYTES/RECORD_BYTES
RESIDENT_PAGES = store.PAYLOAD_BUDGET/store.PAGE_BYTES
SHORT_BYTES = 7
TOTAL_BYTES = RESIDENT_PAGES*store.PAGE_BYTES+SHORT_BYTES
DOS_OPEN = -30
DOS_CLOSE = -36
MODE_NEWFILE = 1006
	.pub
	.section code, kind=code
; A0=DOS base. D0/CCR=status; others preserved. Owns and cleans its fixture.
; Pointer membership proves ownership, not allocator call identity; existing
; memory telemetry supplies the separate allocation-traffic comparison.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a0, DosBase
	moveq #1, d7
	lea State, a0
	jsr store.cleanup
	movea.l DosBase, a1
	lea Scratch, a2
	jsr store.configure
	bne.w finish
	bsr.w verifyEarlyReader
	bne.w finish
	lea State, a0
	jsr store.cleanup
	movea.l DosBase, a1
	lea Scratch, a2
	jsr store.configure
	bne.w finish
	bsr.w build
	bne.w finish
	bsr.w verifyShort
	bne.w finish
	bsr.w verifyDirtyFailure
	bne.w finish
	bsr.w verifyPinned
	bne.w finish
	bsr.w verifyReadFailure
	bne.w finish
	moveq #0, d7
finish
	lea State, a0
	jsr store.cleanup
	movea.l a0, a1
	move.w #store.STATE_BYTES/4-1, d0
cleared
	tst.l (a1)+
	bne.w badCleanup
	dbra d0, cleared
	bra.w done
badCleanup
	moveq #1, d7
done
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.priv

; Reading an unsealed short writer must persist the dense file prefix before
; another writer starts. Force its eviction, then fault back every exact byte.
verifyEarlyReader	.block
	movem.l d1-d7/a0-a2, -(sp)
	moveq #17, d0
	bsr.w fillSource
	lea State, a0
	lea Source, a1
	moveq #SHORT_BYTES, d0
	jsr store.append
	bne.w done
	moveq #0, d1
	jsr store.acquire
	bne.w done
	cmpi.l #SHORT_BYTES, d2
	bne.w bad
	move.l d3, d1
	jsr store.releaseView
	bne.w done
	tst.l store.State.Active(a0)
	bne.w bad
	movea.l store.State.Directory+memory.Block.Pointer(a0), a1
	tst.l store.Row.Backed(a1)
	beq.w bad
	tst.l store.Row.Dirty(a1)
	bne.w bad
	moveq #34, d0
	bsr.w fillSource
	move.w #RESIDENT_PAGES*RECORDS_PER_PAGE-1, d7
append
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	bne.w done
	dbra d7, append
	jsr store.seal
	bne.w done
	movea.l store.State.Directory+memory.Block.Pointer(a0), a1
	tst.l store.Row.Storage+memory.Block.Pointer(a1)
	bne.w bad
	moveq #0, d1
	jsr store.acquire
	bne.w done
	cmpi.l #SHORT_BYTES, d2
	bne.w bad
	moveq #SHORT_BYTES-1, d6
bytes
	cmpi.b #17, (a1)+
	bne.w bad
	dbra d6, bytes
	move.l d3, d1
	jsr store.releaseView
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; verifyEarlyReader

; Four full pages establish the allocation set; a short fifth forces handoff.
build	.block
	movem.l d1-d7/a0-a2, -(sp)
	moveq #0, d6
page
	move.l d6, d0
	addq.l #1, d0
	bsr.w fillSource
	move.w #RECORDS_PER_PAGE-1, d5
record
	lea State, a0
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	bne.w done
	dbra d5, record
	addq.l #1, d6
	cmpi.l #RESIDENT_PAGES, d6
	blo.w page
	movea.l store.State.Directory+memory.Block.Pointer(a0), a1
	lea Pointers, a2
	moveq #RESIDENT_PAGES-1, d0
snapshot
	move.l store.Row.Storage+memory.Block.Pointer(a1), (a2)+
	adda.w #store.ROW_BYTES, a1
	dbra d0, snapshot
	bsr.w verifyOwners
	bne.w done
	moveq #85, d0
	bsr.w fillSource
	lea State, a0
	lea Source, a1
	moveq #SHORT_BYTES, d0
	jsr store.append
	bne.w done
	cmpi.l #RESIDENT_PAGES*store.PAGE_BYTES, d1
	bne.w bad
	jsr store.seal
	bne.w done
	bsr.w verifyOwners
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; build

; D0=byte. Populate every source byte; caller state preserved.
fillSource	.block
	movem.l d1/a0, -(sp)
	lea Source, a0
	move.w #RECORD_BYTES-1, d1
next
	move.b d0, (a0)+
	dbra d1, next
	movem.l (sp)+, d1/a0
	rts
	.bend  ; fillSource

; Every live payload owns one unique original allocation; holes retain no capacity.
; This invariant also checks the cache count after a failed read frees one buffer.
verifyOwners	.block
	movem.l d1-d7/a0-a2, -(sp)
	lea State, a0
	movea.l store.State.Directory+memory.Block.Pointer(a0), a1
	move.l store.State.Directory+memory.Block.Used(a0), d6
	moveq #0, d5
	moveq #0, d7
row
	tst.l d6
	beq.w counted
	move.l store.Row.Storage+memory.Block.Pointer(a1), d1
	beq.w hole
	cmpi.l #store.PAGE_BYTES, store.Row.Storage+memory.Block.Capacity(a1)
	bne.w bad
	moveq #0, d2
	lea Pointers, a2
find
	cmp.l (a2)+, d1
	beq.w found
	addq.l #1, d2
	cmpi.l #RESIDENT_PAGES, d2
	blo.w find
	bra.w bad
found
	bset d2, d5
	bne.w bad
	addi.l #store.PAGE_BYTES, d7
	bra.w next
hole
	tst.l store.Row.Storage+memory.Block.Capacity(a1)
	bne.w bad
next
	adda.w #store.ROW_BYTES, a1
	subi.l #store.ROW_BYTES, d6
	bra.w row
counted
	cmp.l store.State.Reserved(a0), d7
	bne.w bad
	cmpi.l #store.PAYLOAD_BUDGET, d7
	bhi.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; verifyOwners

; A reused short page exposes only initialized bytes, despite its nonzero tail.
verifyShort	.block
	movem.l d1-d3/a0-a1, -(sp)
	lea State, a0
	move.l #RESIDENT_PAGES*store.PAGE_BYTES, d1
	jsr store.acquire
	bne.w done
	cmpi.l #SHORT_BYTES, d2
	bne.w bad
	cmpi.b #85, SHORT_BYTES-1(a1)
	bne.w bad
	move.l d3, d1
	jsr store.releaseView
	bne.w done
	move.l #TOTAL_BYTES, d1
	jsr store.acquire
	beq.w bad
	move.l a1, d0
	or.l d2, d0
	or.l d3, d0
	bne.w bad
	bsr.w verifyOwners
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; verifyShort

; A failed dirty flush must not transfer/free its victim. Restore the real file
; handle immediately after the call, before checking results or taking branches.
verifyDirtyFailure	.block
	movem.l d1-d7/a0-a2, -(sp)
	lea State, a0
	moveq #0, d1
	jsr store.acquire
	bne.w done
	move.b #99, (a1)
	move.l a1, d6
	move.l d3, d1
	jsr store.commitView
	bne.w done
	jsr store.releaseView
	bne.w done
	clr.l store.State.Victim(a0)
	bsr.w nonresident
	bne.w done
	move.l d1, FaultOffset
	move.l store.State.Io+io.Context.File(a0), d7
	clr.l store.State.Io+io.Context.File(a0)
	jsr store.acquire
	move.l d0, d5
	move.l d7, store.State.Io+io.Context.File(a0)
	tst.l d5
	beq.w bad
	move.l a1, d0
	or.l d2, d0
	or.l d3, d0
	bne.w bad
	cmpi.l #store.FAILURE_IO, store.State.Failure(a0)
	bne.w bad
	cmpi.l #store.PAYLOAD_BUDGET, store.State.Reserved(a0)
	bne.w bad
	movea.l store.State.Directory+memory.Block.Pointer(a0), a2
	cmp.l store.Row.Storage+memory.Block.Pointer(a2), d6
	bne.w bad
	tst.l store.Row.Dirty(a2)
	beq.w bad
	bsr.w verifyOwners
	bne.w done
	; Retry writes the complete dirty victim before handing its buffer away.
	move.l FaultOffset, d1
	jsr store.acquire
	bne.w done
	move.l d3, d1
	jsr store.releaseView
	bne.w done
	moveq #0, d1
	jsr store.acquire
	bne.w done
	cmpi.b #99, (a1)
	bne.w bad
	move.l d3, d1
	jsr store.releaseView
	bne.w done
	bsr.w verifyOwners
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; verifyDirtyFailure

; A pinned payload cannot be selected while other pages repeatedly exchange buffers.
verifyPinned	.block
	movem.l d1-d7/a0-a2, -(sp)
	lea State, a0
	moveq #0, d1
	jsr store.acquire
	bne.w done
	movea.l a1, a2
	move.l d3, d7
	moveq #11, d6
next
	bsr.w nonresident
	bne.w done
	jsr store.acquire
	bne.w done
	move.l d3, d1
	jsr store.releaseView
	bne.w done
	cmpi.b #99, (a2)
	bne.w bad
	bsr.w verifyOwners
	bne.w done
	dbra d6, next
	move.l d7, d1
	jsr store.releaseView
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; verifyPinned

; Clean pages survive a transport short read without exposing a partial payload.
; Truncate only this fixture's owned file using the existing DOS open operation.
verifyReadFailure	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea State, a0
	; Evict row zero so the eventual empty-file read starts at valid offset zero.
	clr.l store.State.Victim(a0)
	bsr.w nonresident
	bne.w done
	jsr store.acquire
	bne.w done
	move.l d3, d1
	jsr store.releaseView
	bne.w done
	jsr store.seal
	bne.w done
	movea.l store.State.Directory+memory.Block.Pointer(a0), a1
	tst.l store.Row.Storage+memory.Block.Pointer(a1)
	bne.w bad
	clr.l FaultOffset
	move.l store.Row.Storage+memory.Block.Used(a1), d7
	movea.l a1, a2
	movea.l DosBase, a6
	move.l store.State.Io+io.Context.File(a0), d1
	clr.l store.State.Io+io.Context.File(a0)
	jsr DOS_CLOSE(a6)
	lea State+store.State.Io+io.Context.Path, a1
	move.l a1, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	lea State, a0
	move.l d0, store.State.Io+io.Context.File(a0)
	beq.w bad
	move.l FaultOffset, d1
	jsr store.acquire
	beq.w bad
	move.l a1, d0
	or.l d2, d0
	or.l d3, d0
	bne.w bad
	cmpi.l #store.FAILURE_IO, store.State.Failure(a0)
	bne.w bad
	cmpi.l #store.PAYLOAD_BUDGET-store.PAGE_BYTES, store.State.Reserved(a0)
	bne.w bad
	tst.l store.Row.Storage+memory.Block.Pointer(a2)
	bne.w bad
	tst.l store.Row.Storage+memory.Block.Capacity(a2)
	bne.w bad
	cmp.l store.Row.Storage+memory.Block.Used(a2), d7
	bne.w bad
	cmpi.l #TOTAL_BYTES, store.State.LogicalUsed(a0)
	bne.w bad
	bsr.w verifyOwners
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; verifyReadFailure

; A0=State -> D0/CCR=status,D1=first nonresident logical offset,A1=row.
; Other registers preserved; the fixture always has five rows and four buffers.
nonresident	.block
	movem.l d2, -(sp)
	movea.l store.State.Directory+memory.Block.Pointer(a0), a1
	move.l store.State.Directory+memory.Block.Used(a0), d2
next
	tst.l d2
	beq.w bad
	tst.l store.Row.Storage+memory.Block.Pointer(a1)
	beq.w found
	adda.w #store.ROW_BYTES, a1
	subi.l #store.ROW_BYTES, d2
	bra.w next
found
	move.l store.Row.Base(a1), d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; nonresident
	.endsection
	.section data, kind=data
Scratch	.byte "Work:", 0
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
State	.res byte, store.STATE_BYTES
Pointers	.res long, RESIDENT_PAGES
FaultOffset	.res long, 1
Source	.res byte, RECORD_BYTES
	.endsection
	.endmodule
