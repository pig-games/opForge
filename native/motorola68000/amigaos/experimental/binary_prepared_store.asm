; Owned packed records with stable dense offsets and bounded mutable disk pages.
; Directory growth never invalidates numeric leases. Payload pointers survive only
; while their page is pinned; no descriptor pointer crosses this module boundary.
; @opforge-owner: experimental.amigaos.binary_prepared_store
	.module experimental.amigaos.binary_prepared_store
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_capture_io as io
	.use experimental.amigaos.binary_source as source
	.pub
State	.struct
Directory	.res memory.Block.Used+4
Active	.long ?
Cached	.long ?
LogicalUsed	.long ?
Reserved	.long ?
Configured	.long ?
Failure	.long ?
Victim	.long ?
Limit	.long ?  ; owner-selected checked logical extent; payload cache stays bounded
Io	.res io.CONTEXT_BYTES
.endstruct
Row	.struct
Base	.long ?
Storage	.res memory.Block.Used+4
Dirty	.long ?
Pins	.long ?
Backed	.long ?
Padding	.long ?
.endstruct
STATE_BYTES = State.Io+io.CONTEXT_BYTES
ROW_BYTES = Row.Padding+4
ROW_SHIFT = 5
PAGE_BYTES = 32768
PAYLOAD_BUDGET = 4*PAGE_BYTES
MAX_RECORD_BYTES = 256
FAILURE_IO = 1
FAILURE_BUDGET = 2
FAILURE_ALLOCATION = 3
FAILURE_LEASE = 4
FAILURE_RANGE = 5
	.section code, kind=code
; A0=zero State,A1=DOS base,A2=existing parent path. D0/CCR=status;
; other registers preserved. Failure owns no file or payload; cleanup is safe.
configure	.block
	move.l #source.RECORD_LIMIT, d0
	bra.w configureBounded
	.bend  ; configure

; Same transport/cache contract as configure, D0=positive owner logical limit.
; Limits remain below signed offsets; no allocation grows with this value alone.
; D0/CCR=status; other registers preserved. configure retains source's old limit.
configureBounded	.block
	movem.l d1/a0, -(sp)
	tst.l d0
	ble.w range
	move.l d0, d1
	lea State.Io(a0), a0
	jsr io.configure
	movea.l 4(sp), a0
	tst.l d0
	bne.w bad
	move.l d1, State.Limit(a0)
	move.l #1, State.Configured(a0)
	moveq #0, d0
	bra.w done
range
	move.l #FAILURE_RANGE, State.Failure(a0)
	moveq #1, d0
	bra.w done
bad
	move.l #FAILURE_IO, State.Failure(a0)
	moveq #1, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; configureBounded

; A0=State,A1=complete packed record,D0=positive bytes <=MAX_RECORD_BYTES.
; D0/CCR=status,D1=dense offset on success (zero on failure); others preserved.
; Records never straddle pages. Total logical extent is bounded by the owner's
; configured contract, independently of memory.Block's per-allocation limit.
append	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a4
	movea.l a1, a5
	clr.l State.Failure(a4)
	tst.l State.Configured(a4)
	beq.w bad
	move.l d0, d5
	beq.w bad
	cmpi.l #MAX_RECORD_BYTES, d5
	bhi.w bad
	move.l State.LogicalUsed(a4), d6
	add.l d5, d6
	bcs.w bad
	cmp.l State.Limit(a4), d6
	bhi.w bad
	move.l State.Active(a4), d4
	beq.w newRow
	subq.l #1, d4
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	adda.l d4, a3
	move.l #PAGE_BYTES, d0
	sub.l Row.Storage+memory.Block.Used(a3), d0
	cmp.l d5, d0
	bhs.w copyRecord
newRow
	bsr.w sealActive
	bne.w failed
	move.l State.Directory+memory.Block.Used(a4), d4
	move.l d4, d0
	addi.l #ROW_BYTES, d0
	lea State.Directory(a4), a0
	jsr memory.reserve
	bne.w allocationFailed
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	adda.l d4, a3
	movea.l a3, a0
	moveq #ROW_BYTES/4-1, d0
clearRow
	clr.l (a0)+
	dbra d0, clearRow
	lea Row.Storage(a3), a0
	bsr.w providePage
	bne.w failed
	move.l State.LogicalUsed(a4), Row.Base(a3)
	addi.l #ROW_BYTES, State.Directory+memory.Block.Used(a4)
	move.l d4, d0
	addq.l #1, d0
	move.l d0, State.Active(a4)
copyRecord
	movea.l Row.Storage+memory.Block.Pointer(a3), a1
	adda.l Row.Storage+memory.Block.Used(a3), a1
	move.l d5, d2
copy
	move.b (a5)+, (a1)+
	subq.l #1, d2
	bne.w copy
	add.l d5, Row.Storage+memory.Block.Used(a3)
	move.l #1, Row.Dirty(a3)
	move.l State.LogicalUsed(a4), d1
	move.l d6, State.LogicalUsed(a4)
	moveq #0, d0
	bra.w done
allocationFailed
	move.l #FAILURE_ALLOCATION, State.Failure(a4)
	bra.w failed
bad
	move.l #FAILURE_RANGE, State.Failure(a4)
failed
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; append

; A0=State. Persist dirty pages and close the writer boundary.
; D0/CCR=status; all other registers preserved. Reject pinned dirty pages;
; release every modified view before sealing. IO failure retains dirty ownership.
seal	.block
	movem.l d1/a2/a4, -(sp)
	movea.l a0, a4
	clr.l State.Failure(a4)
	movea.l State.Directory+memory.Block.Pointer(a4), a2
	move.l State.Directory+memory.Block.Used(a4), d1
next
	tst.l d1
	beq.w good
	tst.l Row.Dirty(a2)
	beq.w advance
	tst.l Row.Pins(a2)
	bne.w leased
	bsr.w flushRow
	bne.w done
advance
	adda.l #ROW_BYTES, a2
	subi.l #ROW_BYTES, d1
	bra.w next
leased
	move.l #FAILURE_LEASE, State.Failure(a4)
	moveq #1, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1/a2/a4
	tst.l d0
	rts
	.bend  ; seal

; A0=State,D1=logical offset. D0/CCR=status; success A1=payload pointer at
; offset,D2=contiguous remaining used bytes,D3=lease (row index+1).
; D1 and all other registers preserved. Failure clears A1/D2/D3. Caller must
; commitView before releasing modified bytes; retain no pointer after release.
acquire	.block
	movem.l d1/d4-d7/a0/a2-a6, -(sp)
	movea.l a0, a4
	clr.l State.Failure(a4)
	tst.l State.Configured(a4)
	beq.w bad
	cmp.l State.LogicalUsed(a4), d1
	bhs.w bad
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	move.l State.Cached(a4), d4
	beq.w search
	subq.l #1, d4
	lea 0(a3, d4.l), a2
	move.l d1, d5
	sub.l Row.Base(a2), d5
	bcs.w search
	cmp.l Row.Storage+memory.Block.Used(a2), d5
	blo.w found
search
	moveq #0, d6
	move.l State.Directory+memory.Block.Used(a4), d7
	lsr.l #ROW_SHIFT, d7
loop
	cmp.l d7, d6
	bhs.w bad
	move.l d6, d4
	add.l d7, d4
	lsr.l #1, d4
	move.l d4, d3
	lsl.l #ROW_SHIFT, d4
	lea 0(a3, d4.l), a2
	move.l d1, d5
	sub.l Row.Base(a2), d5
	bcs.w lower
	cmp.l Row.Storage+memory.Block.Used(a2), d5
	blo.w found
	move.l d3, d6
	addq.l #1, d6
	bra.w loop
lower
	move.l d3, d7
	bra.w loop
found
	cmpi.l #-1, Row.Pins(a2)
	beq.w leased
	bsr.w resident
	bne.w failed
	; A borrowed writer page must never receive subsequent appends.
	move.l d4, d0
	addq.l #1, d0
	cmp.l State.Active(a4), d0
	bne.w pin
	clr.l State.Active(a4)
pin
	addq.l #1, Row.Pins(a2)
	move.l d4, d0
	addq.l #1, d0
	move.l d0, State.Cached(a4)
	move.l d4, d3
	lsr.l #ROW_SHIFT, d3
	addq.l #1, d3
	movea.l Row.Storage+memory.Block.Pointer(a2), a1
	adda.l d5, a1
	move.l Row.Storage+memory.Block.Used(a2), d2
	sub.l d5, d2
	moveq #0, d0
	bra.w done
leased
	move.l #FAILURE_LEASE, State.Failure(a4)
	bra.w failed
bad
	move.l #FAILURE_RANGE, State.Failure(a4)
failed
	suba.l a1, a1
	moveq #0, d2
	moveq #0, d3
	moveq #1, d0
done
	movem.l (sp)+, d1/d4-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; acquire

; A0=State,D1=lease. Mark a pinned resident page dirty without writing it.
; D0/CCR=status; other registers preserved. Caller owns writes within Used only.
commitView	.block
	movem.l a1, -(sp)
	bsr.w leasedRow
	bne.w done
	move.l #1, Row.Dirty(a1)
	moveq #0, d0
done
	movem.l (sp)+, a1
	tst.l d0
	rts
	.bend  ; commitView

; A0=State,D1=lease. Drop one pin without IO. D0/CCR=status; others preserved.
; Invalid/unbalanced lease leaves ownership unchanged and latches FAILURE_LEASE.
releaseView	.block
	movem.l a1, -(sp)
	bsr.w leasedRow
	bne.w done
	subq.l #1, Row.Pins(a1)
	moveq #0, d0
done
	movem.l (sp)+, a1
	tst.l d0
	rts
	.bend  ; releaseView

; A0=State. Discard every page and private scratch file, then zero State.
; Preserves all registers; CCR unspecified. Idempotent; no view survives cleanup.
cleanup	.block
	movem.l d0-d2/a0-a2, -(sp)
	movea.l a0, a2
	movea.l State.Directory+memory.Block.Pointer(a2), a1
	move.l State.Directory+memory.Block.Used(a2), d2
next
	tst.l d2
	beq.w directory
	lea Row.Storage(a1), a0
	jsr memory.release
	adda.l #ROW_BYTES, a1
	subi.l #ROW_BYTES, d2
	bra.w next
directory
	lea State.Directory(a2), a0
	jsr memory.release
	lea State.Io(a2), a0
	jsr io.cleanup
	move.w #STATE_BYTES/4-1, d2
clear
	clr.l (a2)+
	dbra d2, clear
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend  ; cleanup
	.priv
; A0=State,D1=lease. D0/CCR=status,A1=row; D2 preserved.
leasedRow	.block
	move.l d2, -(sp)
	move.l d1, d2
	beq.w bad
	subq.l #1, d2
	move.l State.Directory+memory.Block.Used(a0), d0
	lsr.l #ROW_SHIFT, d0
	cmp.l d0, d2
	bhs.w bad
	lsl.l #ROW_SHIFT, d2
	movea.l State.Directory+memory.Block.Pointer(a0), a1
	adda.l d2, a1
	tst.l Row.Pins(a1)
	beq.w bad
	tst.l Row.Storage+memory.Block.Pointer(a1)
	beq.w bad
	moveq #0, d0
	bra.w done
bad
	move.l #FAILURE_LEASE, State.Failure(a0)
	moveq #1, d0
done
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; leasedRow

; A4=State. D0/CCR=status; all other registers preserved.
sealActive	.block
	movem.l a2, -(sp)
	move.l State.Active(a4), d0
	beq.w good
	movea.l State.Directory+memory.Block.Pointer(a4), a2
	adda.l d0, a2
	subq.l #1, a2
	tst.l Row.Pins(a2)
	bne.w leased
	bsr.w flushRow
	bra.w done
leased
	move.l #FAILURE_LEASE, State.Failure(a4)
	moveq #1, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, a2
	tst.l d0
	rts
	.bend  ; sealActive

; A4=State,A2=row. Write dirty original offsets once, then close its writer.
; D0/CCR=status; all other registers preserved. Never called for pinned pages.
flushRow	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.l Row.Dirty(a2)
	beq.w clean
	lea State.Io(a4), a0
	move.l Row.Base(a2), d1
	movea.l Row.Storage+memory.Block.Pointer(a2), a1
	move.l Row.Storage+memory.Block.Used(a2), d2
	jsr io.write
	bne.w bad
	clr.l Row.Dirty(a2)
	move.l #1, Row.Backed(a2)
clean
	move.l a2, d0
	sub.l State.Directory+memory.Block.Pointer(a4), d0
	addq.l #1, d0
	cmp.l State.Active(a4), d0
	bne.w good
	clr.l State.Active(a4)
good
	moveq #0, d0
	bra.w done
bad
	move.l #FAILURE_IO, State.Failure(a4)
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; flushRow

; A4=State,A0=destination Storage with zero Pointer/Capacity; Used may retain
; its logical extent. Allocate below the payload budget, otherwise transfer one
; flushed, unpinned victim buffer. Logical row extents never move with buffers.
; D0/CCR=status; all other registers preserved. A failed flush changes no owners.
providePage	.block
	movem.l d1-d3/a0-a3, -(sp)
	movea.l a0, a3
	cmpi.l #PAYLOAD_BUDGET-PAGE_BYTES, State.Reserved(a4)
	bls.w allocate
	move.l State.Directory+memory.Block.Used(a4), d3
	move.l State.Victim(a4), d2
	cmp.l d3, d2
	blo.w scan
	moveq #0, d2
scan
	tst.l d3
	beq.w bad
	movea.l State.Directory+memory.Block.Pointer(a4), a2
	adda.l d2, a2
	tst.l Row.Pins(a2)
	bne.w next
	tst.l Row.Storage+memory.Block.Pointer(a2)
	beq.w next
	bsr.w flushRow
	bne.w failed
	; Transfer allocation ownership only. Both logical Used fields stay intact.
	move.l Row.Storage+memory.Block.Pointer(a2), memory.Block.Pointer(a3)
	move.l Row.Storage+memory.Block.Capacity(a2), memory.Block.Capacity(a3)
	clr.l Row.Storage+memory.Block.Pointer(a2)
	clr.l Row.Storage+memory.Block.Capacity(a2)
	addi.l #ROW_BYTES, d2
	move.l d2, State.Victim(a4)
	bra.w good
next
	addi.l #ROW_BYTES, d2
	cmp.l State.Directory+memory.Block.Used(a4), d2
	blo.w cursorReady
	moveq #0, d2
cursorReady
	subi.l #ROW_BYTES, d3
	bra.w scan
allocate
	; A nonresident row has Used but no source allocation to copy during growth.
	move.l memory.Block.Used(a3), d3
	clr.l memory.Block.Used(a3)
	move.l #PAGE_BYTES, d0
	jsr memory.reserveExact
	move.l d3, memory.Block.Used(a3)
	tst.l d0
	bne.w allocationFailed
	addi.l #PAGE_BYTES, State.Reserved(a4)
	bra.w good
allocationFailed
	move.l #FAILURE_ALLOCATION, State.Failure(a4)
	bra.w failed
bad
	move.l #FAILURE_BUDGET, State.Failure(a4)
failed
	moveq #1, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; providePage

; A4=State,A2=row. Restore its stable logical extent, including committed
; edits. Used never changes across faults. D0/CCR=status; others preserved.
resident	.block
	movem.l d1-d3/a0-a1, -(sp)
	tst.l Row.Storage+memory.Block.Pointer(a2)
	bne.w good
	tst.l Row.Backed(a2)
	beq.w bad
	lea Row.Storage(a2), a0
	bsr.w providePage
	bne.w failed
	move.l Row.Storage+memory.Block.Used(a2), d3
	lea State.Io(a4), a0
	move.l Row.Base(a2), d1
	movea.l Row.Storage+memory.Block.Pointer(a2), a1
	move.l d3, d2
	jsr io.read
	beq.w good
	lea Row.Storage(a2), a0
	jsr memory.release
	subi.l #PAGE_BYTES, State.Reserved(a4)
	move.l #FAILURE_IO, State.Failure(a4)
	bra.w failed
bad
	move.l #FAILURE_ALLOCATION, State.Failure(a4)
failed
	moveq #1, d0
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; resident
	.endsection
	.endmodule
