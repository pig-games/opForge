; Append-only dense logical storage with optional bounded scratch-file backing.
; Resident chunks never move while pinned; directory rows
; can move on append, so callers retain numeric handles, never descriptor pointers.
; Retirement records module spans, while chunks remain the allocation/IO unit.
; Logical Used includes tombstones; resident capacity excludes fully retired rows.
; @opforge-owner: experimental.amigaos.binary_capture_store
	.module experimental.amigaos.binary_capture_store
	.cpu 68020
	.include "capture_telemetry.i"
	.include "hotspot_telemetry.i"
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_capture_io as io
	.pub
State	.struct
Directory	.res memory.Block.Used+4
Retired	.res memory.Block.Used+4
Active	.long ?
Cached	.long ?
LogicalUsed	.long ?
Reserved	.long ?
Budget	.long ?
Configured	.long ?
Failure	.long ?
Victim	.long ?
Io	.res io.CONTEXT_BYTES
.endstruct
Row	.struct
Base	.long ?
Storage	.res memory.Block.Used+4
Backed	.long ?
Pins	.long ?
Live	.long ?
Padding	.res 4
.endstruct
STATE_BYTES = State.Io+io.CONTEXT_BYTES
ROW_BYTES = Row.Padding+4
Extent	.struct
Start	.long ?
End	.long ?
.endstruct
EXTENT_BYTES = Extent.End+4
ROW_SHIFT = 5
CHUNK_BYTES = 32768
DEDICATED_THRESHOLD = 4096
FAILURE_IO = 1
FAILURE_BUDGET = 2
FAILURE_ALLOCATION = 3
FAILURE_LEASE = 4
FAILURE_RANGE = 5
STORE_COUNT	.macro counter, value
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .value, d0
	add.l d0, .counter
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
STORE_PEAK	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	jsr observePeak
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
STORE_CLEAR	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	clr.l DiskReads
	clr.l DiskReadBytes
	clr.l DiskWrites
	clr.l DiskWriteBytes
	clr.l Faults
	clr.l Evictions
	clr.l PeakPayload
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
	.section code, kind=code
; A0=zero State,A1=DOS base,A2=existing temporary directory,D0=payload budget.
; D0/CCR=status; others preserved. Budget includes every resident payload capacity.
; Directory metadata is separate. Failure leaves no owned allocations or paths.
; Configured operations latch the latest failure reason in State.Failure.
configure	.block
	movem.l d1/a0, -(sp)
	.STORE_CLEAR
	andi.l #$fffffff8, d0
	tst.l d0
	beq.w bad
	move.l d0, State.Budget(a0)
	lea State.Io(a0), a0
	jsr io.configure
	beq.w configured
	movea.l 4(sp), a0
	move.l #FAILURE_IO, State.Failure(a0)
	bra.w done
configured
	movea.l 4(sp), a0
	move.l #1, State.Configured(a0)
	bra.w done
bad
	move.l #FAILURE_BUDGET, State.Failure(a0)
	moveq #1, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; configure

; A0=zero-initialized State,A1=record,D0=positive bytes. D0/CCR=status,
; D1=dense logical handle on success (zero on failure). Others preserved.
; Complete records are contiguous. Failed allocation preserves committed records;
; directory capacity may grow. memory telemetry observes all allocations/copies.
append	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a4
	tst.l State.Configured(a4)
	beq.w failureReady
	clr.l State.Failure(a4)
failureReady
	movea.l a1, a5
	move.l d0, d5
	beq.w bad
	cmpi.l #memory.LIMIT, d5
	bhi.w bad
	move.l State.LogicalUsed(a4), d6
	add.l d5, d6
	bcs.w bad
	tst.l State.Configured(a4)
	beq.w sizeReady
	tst.l d6
	bmi.w bad
sizeReady
	cmpi.l #DEDICATED_THRESHOLD, d5
	bhi.w newRow
	move.l State.Active(a4), d4
	beq.w newRow
	subq.l #1, d4
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	adda.l d4, a3
	move.l Row.Storage+memory.Block.Capacity(a3), d0
	sub.l Row.Storage+memory.Block.Used(a3), d0
	cmp.l d5, d0
	bhs.w copyRecord
newRow
	tst.l State.Configured(a4)
	beq.w directoryReady
	bsr.w sealActive
	bne.w bad
directoryReady
	move.l State.Directory+memory.Block.Used(a4), d4
	move.l d4, d0
	addi.l #ROW_BYTES, d0
	lea State.Directory(a4), a0
	jsr memory.reserve
	bne.w allocationFailed
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	adda.l d4, a3
	; An uncommitted row owns nothing, including after a failed allocation.
	clr.l Row.Storage+memory.Block.Pointer(a3)
	clr.l Row.Storage+memory.Block.Capacity(a3)
	clr.l Row.Storage+memory.Block.Used(a3)
	clr.l Row.Backed(a3)
	clr.l Row.Pins(a3)
	clr.l Row.Live(a3)
	move.l #CHUNK_BYTES, d0
	cmpi.l #DEDICATED_THRESHOLD, d5
	bls.w allocate
	move.l d5, d0
allocate
	tst.l State.Configured(a4)
	beq.w allocateReady
	cmp.l State.Budget(a4), d0
	bls.w capacityReady
	move.l State.Budget(a4), d0
capacityReady
	cmp.l d5, d0
	bhs.w capacityFits
	move.l #FAILURE_BUDGET, State.Failure(a4)
	bra.w bad
capacityFits
	bsr.w makeRoom
	bne.w bad
allocateReady
	lea Row.Storage(a3), a0
	.CAPTURE_GROW_BEGIN
	jsr memory.reserveExact
	.CAPTURE_GROW_END
	bne.w allocationFailed
	move.l Row.Storage+memory.Block.Capacity(a3), d0
	add.l d0, State.Reserved(a4)
	.STORE_PEAK
	move.l State.LogicalUsed(a4), Row.Base(a3)
	addi.l #ROW_BYTES, State.Directory+memory.Block.Used(a4)
	clr.l State.Active(a4)
	cmpi.l #DEDICATED_THRESHOLD, d5
	bhi.w copyRecord
	move.l d4, d0
	addq.l #1, d0
	move.l d0, State.Active(a4)
copyRecord
	.HOTSPOT_ENTER #HOTSPOT_CAPTURE_APPEND
	movea.l Row.Storage+memory.Block.Pointer(a3), a1
	adda.l Row.Storage+memory.Block.Used(a3), a1
	move.l d5, d2
	lsr.l #2, d2
	beq.w tail
copyLong
	move.l (a5)+, (a1)+
	subq.l #1, d2
	bne.w copyLong
tail
	move.l d5, d2
	andi.l #3, d2
	beq.w copied
copyByte
	move.b (a5)+, (a1)+
	subq.l #1, d2
	bne.w copyByte
copied
	.HOTSPOT_LEAVE #HOTSPOT_CAPTURE_APPEND
	add.l d5, Row.Live(a3)
	add.l d5, Row.Storage+memory.Block.Used(a3)
	move.l State.LogicalUsed(a4), d1
	move.l d6, State.LogicalUsed(a4)
	moveq #0, d0
	bra.w done
allocationFailed
	tst.l State.Configured(a4)
	beq.w bad
	move.l #FAILURE_ALLOCATION, State.Failure(a4)
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; append

; A0=State,D1=logical byte offset. D0/CCR=status; on success A1=Block,
; D1=local byte offset,D2=remaining used bytes. Failure clears all outputs.
; Preserves D3-D7/A0/A2-A6. Cached interval precedes binary search. Descriptor
; is valid until append/resolve/acquire/cleanup; remaining excludes capacity slack.
; Configured callers retaining payload pointers must use acquire/releaseView.
resolve	.block
	movem.l d3-d7/a0/a2-a6, -(sp)
	movea.l a0, a4
	tst.l State.Configured(a4)
	beq.w failureReady
	clr.l State.Failure(a4)
failureReady
	cmp.l State.LogicalUsed(a4), d1
	bhs.w bad
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	move.l State.Cached(a4), d4
	beq.w search
	subq.l #1, d4
	lea 0(a3, d4.l), a2
	move.l d1, d3
	sub.l Row.Base(a2), d3
	bcs.w search
	cmp.l Row.Storage+memory.Block.Used(a2), d3
	blo.w found
search
	moveq #0, d5
	move.l State.Directory+memory.Block.Used(a4), d6
	lsr.l #ROW_SHIFT, d6
searchLoop
	cmp.l d6, d5
	bhs.w bad
	move.l d5, d4
	add.l d6, d4
	lsr.l #1, d4
	move.l d4, d7
	lsl.l #ROW_SHIFT, d4
	lea 0(a3, d4.l), a2
	move.l d1, d3
	sub.l Row.Base(a2), d3
	bcs.w lower
	cmp.l Row.Storage+memory.Block.Used(a2), d3
	blo.w found
	move.l d7, d5
	addq.l #1, d5
	bra.w searchLoop
lower
	move.l d7, d6
	bra.w searchLoop
found
	move.l Row.Live(a2), d0
	beq.w bad
	cmp.l Row.Storage+memory.Block.Used(a2), d0
	bne.w sharedRow
	; File/module boundaries seal writers, so most rows have one live owner.
	; Only partially retired shared rows need the interval-directory lookup.
	move.l d0, d2
	sub.l d3, d2
	bra.w liveReady
sharedRow
	bsr.w liveSpan
	bne.w bad
liveReady
	move.l d2, d7
	bsr.w resident
	bne.w bad
	addq.l #1, d4
	move.l d4, State.Cached(a4)
	lea Row.Storage(a2), a1
	move.l d3, d1
	move.l memory.Block.Used(a1), d2
	sub.l d1, d2
	cmp.l d7, d2
	bls.w spanReady
	move.l d7, d2
spanReady
	moveq #0, d0
	bra.w done
bad
	suba.l a1, a1
	moveq #0, d1
	moveq #0, d2
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; resolve

; A0=State,D1=global offset. Same outputs as resolve; pins the row on success.
; Preserves D3-D7/A0/A2-A6. Acquiring the writer seals it before exposing a view.
; Keep the original global offset for releaseView; append may move descriptors.
acquire	.block
	movem.l d3/a0/a2, -(sp)
	move.l d1, d3
	bsr.w resolve
	bne.w done
	movea.l State.Directory+memory.Block.Pointer(a0), a2
	adda.l State.Cached(a0), a2
	subq.l #1, a2
	cmpi.l #-1, Row.Pins(a2)
	beq.w bad
	movem.l d1-d2/a0-a1/a3-a4, -(sp)
	movea.l a0, a4
	movea.l a2, a3
	bsr.w sealRow
	movem.l (sp)+, d1-d2/a0-a1/a3-a4
	bne.w bad
	addq.l #1, Row.Pins(a2)
	moveq #0, d0
	bra.w done
bad
	suba.l a1, a1
	moveq #0, d1
	moveq #0, d2
	moveq #1, d0
done
	movem.l (sp)+, d3/a0/a2
	tst.l d0
	rts
	.bend  ; acquire

; A0=State,D1=original global offset. Unpin its row without file IO.
; Preserves all registers; CCR unspecified. Invalid/unbalanced configured releases
; latch FAILURE_LEASE. Cached interval precedes binary search; never performs IO.
releaseView	.block
	movem.l d0-d7/a1-a2, -(sp)
	cmp.l State.LogicalUsed(a0), d1
	bhs.w bad
	movea.l State.Directory+memory.Block.Pointer(a0), a1
	move.l State.Cached(a0), d4
	beq.w search
	subq.l #1, d4
	lea 0(a1, d4.l), a2
	move.l d1, d3
	sub.l Row.Base(a2), d3
	bcs.w search
	cmp.l Row.Storage+memory.Block.Used(a2), d3
	blo.w found
search
	moveq #0, d5
	move.l State.Directory+memory.Block.Used(a0), d6
	lsr.l #ROW_SHIFT, d6
searchLoop
	cmp.l d6, d5
	bhs.w bad
	move.l d5, d4
	add.l d6, d4
	lsr.l #1, d4
	move.l d4, d7
	lsl.l #ROW_SHIFT, d4
	lea 0(a1, d4.l), a2
	move.l d1, d3
	sub.l Row.Base(a2), d3
	bcs.w lower
	cmp.l Row.Storage+memory.Block.Used(a2), d3
	blo.w found
	move.l d7, d5
	addq.l #1, d5
	bra.w searchLoop
lower
	move.l d7, d6
	bra.w searchLoop
found
	tst.l Row.Pins(a2)
	beq.w bad
	subq.l #1, Row.Pins(a2)
	bra.w done
bad
	tst.l State.Configured(a0)
	beq.w done
	move.l #FAILURE_LEASE, State.Failure(a0)
done
	movem.l (sp)+, d0-d7/a1-a2
	rts
	.bend  ; releaseView

; A0=State. Seal the current append boundary, including resident-only stores.
; D0/CCR=status; all other registers preserved. The next append starts a new row.
seal	.block
	movem.l a4, -(sp)
	movea.l a0, a4
	bsr.w sealActive
	bne.w done
	clr.l State.Active(a4)
done
	movem.l (sp)+, a4
	tst.l d0
	rts
	.bend  ; seal

; A0=State,D1=start,D2=exclusive end of a nonempty committed logical span.
; D0/CCR=status; all other registers preserved. Reject overlapping retirement or
; any intersected pinned row before mutation. Extents are sorted module metadata;
; shared chunks remain until every byte retires. Logical handles never move.
retire	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a4
	clr.l State.Failure(a4)
	move.l d1, d5
	move.l d2, d6
	cmp.l d6, d5
	bhs.w bad
	cmp.l State.LogicalUsed(a4), d6
	bhi.w bad
	movea.l State.Retired+memory.Block.Pointer(a4), a1
	move.l State.Retired+memory.Block.Used(a4), d7
	moveq #0, d4
extentLoop
	cmp.l d7, d4
	bhs.w validateRows
	cmp.l Extent.End(a1), d5
	bhs.w nextExtent
	cmp.l Extent.Start(a1), d6
	bhi.w bad
	bra.w validateRows
nextExtent
	addq.l #EXTENT_BYTES, d4
	addq.l #EXTENT_BYTES, a1
	bra.w extentLoop
validateRows
	movea.l State.Directory+memory.Block.Pointer(a4), a2
	move.l State.Directory+memory.Block.Used(a4), d3
pinLoop
	tst.l d3
	beq.w reserveExtent
	bsr.w intersection
	beq.w nextPin
	tst.l Row.Pins(a2)
	bne.w leased
nextPin
	adda.l #ROW_BYTES, a2
	subi.l #ROW_BYTES, d3
	bra.w pinLoop
reserveExtent
	move.l d7, d0
	addq.l #EXTENT_BYTES, d0
	lea State.Retired(a4), a0
	jsr memory.reserve
	bne.w allocationFailed
	; Seal before publishing metadata; failure retains all logical ownership.
	bsr.w sealActive
	bne.w failed
	clr.l State.Active(a4)
	movea.l State.Retired+memory.Block.Pointer(a4), a1
	adda.l d7, a1
	move.l d7, d3
shift
	cmp.l d4, d3
	bls.w insert
	subq.l #EXTENT_BYTES, a1
	move.l Extent.Start(a1), d0
	move.l d0, Extent.Start+EXTENT_BYTES(a1)
	move.l Extent.End(a1), d0
	move.l d0, Extent.End+EXTENT_BYTES(a1)
	subq.l #EXTENT_BYTES, d3
	bra.w shift
insert
	move.l d5, Extent.Start(a1)
	move.l d6, Extent.End(a1)
	addq.l #EXTENT_BYTES, State.Retired+memory.Block.Used(a4)
	movea.l State.Directory+memory.Block.Pointer(a4), a2
	move.l State.Directory+memory.Block.Used(a4), d3
retireLoop
	tst.l d3
	beq.w good
	bsr.w intersection
	beq.w nextRow
	sub.l d0, Row.Live(a2)
	bne.w nextRow
	move.l Row.Storage+memory.Block.Capacity(a2), d0
	sub.l d0, State.Reserved(a4)
	move.l Row.Storage+memory.Block.Used(a2), d7
	lea Row.Storage(a2), a0
	.CAPTURE_RETAINED_RELEASE memory.Block.Capacity(a0)
	jsr memory.release
	move.l d7, Row.Storage+memory.Block.Used(a2)
	clr.l Row.Backed(a2)
nextRow
	adda.l #ROW_BYTES, a2
	subi.l #ROW_BYTES, d3
	bra.w retireLoop
good
	moveq #0, d0
	bra.w done
leased
	move.l #FAILURE_LEASE, State.Failure(a4)
	bra.w failed
allocationFailed
	move.l #FAILURE_ALLOCATION, State.Failure(a4)
	bra.w failed
bad
	move.l #FAILURE_RANGE, State.Failure(a4)
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; retire

; A0=State. Free every owned chunk and directory, then zero State. Idempotent.
; Preserves all registers; CCR unspecified. No descriptor survives this call.
cleanup	.block
	movem.l d0-d2/a0-a2, -(sp)
	movea.l a0, a2
	movea.l State.Directory+memory.Block.Pointer(a2), a1
	move.l State.Directory+memory.Block.Used(a2), d2
next
	tst.l d2
	beq.w directory
	lea Row.Storage(a1), a0
	.CAPTURE_RETAINED_RELEASE memory.Block.Capacity(a0)
	jsr memory.release
	adda.l #ROW_BYTES, a1
	subi.l #ROW_BYTES, d2
	bra.w next
directory
	lea State.Retired(a2), a0
	jsr memory.release
	lea State.Io(a2), a0
	jsr io.cleanup
	lea State.Directory(a2), a0
	jsr memory.release
	move.w #STATE_BYTES/4-1, d2
clear
	clr.l (a2)+
	dbra d2, clear
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend  ; cleanup
	.priv
; A2=Row,D5=start,D6=end. D0/CCR=intersection byte count; D1 clobbered.
intersection	.block
	move.l Row.Base(a2), d0
	move.l d0, d1
	add.l Row.Storage+memory.Block.Used(a2), d1
	cmp.l d5, d0
	bhs.w startReady
	move.l d5, d0
startReady
	cmp.l d6, d1
	bls.w endReady
	move.l d6, d1
endReady
	sub.l d0, d1
	bhi.w overlap
	moveq #0, d0
	rts
overlap
	move.l d1, d0
	rts
	.bend  ; intersection

; A4=State,D1=global offset. D0/CCR=status,D2=bytes until next retired
; interval (or logical end). Clobbers D5-D7/A1. Binary search sorted extents.
liveSpan	.block
	moveq #0, d5
	move.l State.Retired+memory.Block.Used(a4), d6
	lsr.l #3, d6
	move.l State.LogicalUsed(a4), d2
	sub.l d1, d2
	movea.l State.Retired+memory.Block.Pointer(a4), a1
loop
	cmp.l d6, d5
	bhs.w good
	move.l d5, d7
	add.l d6, d7
	lsr.l #1, d7
	move.l d7, d0
	lsl.l #3, d0
	cmp.l Extent.End(a1, d0.l), d1
	bhs.w higher
	cmp.l Extent.Start(a1, d0.l), d1
	bhs.w bad
	move.l Extent.Start(a1, d0.l), d2
	sub.l d1, d2
	move.l d7, d6
	bra.w loop
higher
	move.l d7, d5
	addq.l #1, d5
	bra.w loop
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; liveSpan

; A4=State. Seal the active writer, retiring it even if it has spare capacity.
; D0/CCR=status; others preserved.
sealActive	.block
	movem.l a3, -(sp)
	move.l State.Active(a4), d0
	beq.w good
	movea.l State.Directory+memory.Block.Pointer(a4), a3
	adda.l d0, a3
	subq.l #1, a3
	bsr.w sealRow
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, a3
	tst.l d0
	rts
	.bend  ; sealActive

; A4=State,A3=Row. Persist immutable original bytes before making row evictable.
; D0/CCR=status; all other registers preserved. No-op for resident-only stores.
sealRow	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.l State.Configured(a4)
	beq.w good
	tst.l Row.Backed(a3)
	bne.w good
	lea State.Io(a4), a0
	move.l Row.Base(a3), d1
	movea.l Row.Storage+memory.Block.Pointer(a3), a1
	move.l Row.Storage+memory.Block.Used(a3), d2
	jsr io.write
	beq.w written
	move.l #FAILURE_IO, State.Failure(a4)
	bra.w done
written
	.STORE_COUNT DiskWrites, #1
	.STORE_COUNT DiskWriteBytes, d2
	move.l #1, Row.Backed(a3)
	move.l a3, d0
	sub.l State.Directory+memory.Block.Pointer(a4), d0
	addq.l #1, d0
	cmp.l State.Active(a4), d0
	bne.w good
	clr.l State.Active(a4)
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; sealRow

; A4=State,D0=requested capacity. Evict unpinned backed rows until it fits.
; D0=requested capacity on success (CCR Z); failure D0=1 (CCR NZ).
; Other registers preserved. Never allocate beyond the configured payload budget.
makeRoom	.block
	movem.l d1-d4/a0/a2-a3, -(sp)
	move.l d0, d4
	cmp.l State.Budget(a4), d4
	bhi.w bad
	move.l State.Directory+memory.Block.Used(a4), d3
	move.l State.Victim(a4), d2
	cmp.l d3, d2
	blo.w loop
	moveq #0, d2
loop
	move.l State.Budget(a4), d0
	sub.l State.Reserved(a4), d0
	cmp.l d4, d0
	bhs.w good
	tst.l d3
	beq.w bad
	movea.l State.Directory+memory.Block.Pointer(a4), a2
	adda.l d2, a2
	tst.l Row.Pins(a2)
	bne.w next
	tst.l Row.Storage+memory.Block.Pointer(a2)
	beq.w next
	movea.l a2, a3
	bsr.w sealRow
	bne.w bad
	move.l Row.Storage+memory.Block.Capacity(a2), d0
	sub.l d0, State.Reserved(a4)
	lea Row.Storage(a2), a0
	.CAPTURE_RETAINED_RELEASE memory.Block.Capacity(a0)
	jsr memory.release
	.STORE_COUNT Evictions, #1
next
	addi.l #ROW_BYTES, d2
	cmp.l State.Directory+memory.Block.Used(a4), d2
	blo.w cursorReady
	moveq #0, d2
cursorReady
	move.l d2, State.Victim(a4)
	subi.l #ROW_BYTES, d3
	bra.w loop
good
	move.l d4, d0
	cmp.l d0, d0
	bra.w done
bad
	tst.l State.Failure(a4)
	bne.w failed
	move.l #FAILURE_BUDGET, State.Failure(a4)
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0/a2-a3
	rts
	.bend  ; makeRoom

; A4=State,A2=Row. Fault an evicted row, preserving Used/Base and all registers.
; D0/CCR=status. Reserve with Used=0 to avoid copying from an absent allocation.
resident	.block
	movem.l d1-d3/a0-a1, -(sp)
	tst.l Row.Live(a2)
	beq.w bad
	tst.l Row.Storage+memory.Block.Pointer(a2)
	bne.w good
	tst.l Row.Backed(a2)
	beq.w bad
	move.l Row.Storage+memory.Block.Used(a2), d3
	.STORE_COUNT Faults, #1
	move.l d3, d0
	bsr.w makeRoom
	bne.w bad
	clr.l Row.Storage+memory.Block.Used(a2)
	lea Row.Storage(a2), a0
	.CAPTURE_GROW_BEGIN
	jsr memory.reserveExact
	.CAPTURE_GROW_END
	move.l d3, Row.Storage+memory.Block.Used(a2)
	tst.l d0
	bne.w bad
	move.l Row.Storage+memory.Block.Capacity(a2), d0
	add.l d0, State.Reserved(a4)
	.STORE_PEAK
	lea State.Io(a4), a0
	move.l Row.Base(a2), d1
	movea.l Row.Storage+memory.Block.Pointer(a2), a1
	move.l d3, d2
	jsr io.read
	beq.w readComplete
	move.l #FAILURE_IO, State.Failure(a4)
	move.l Row.Storage+memory.Block.Capacity(a2), d0
	sub.l d0, State.Reserved(a4)
	lea Row.Storage(a2), a0
	.CAPTURE_RETAINED_RELEASE memory.Block.Capacity(a0)
	jsr memory.release
	bra.w bad
readComplete
	.STORE_COUNT DiskReads, #1
	.STORE_COUNT DiskReadBytes, d3
	bra.w good
bad
	tst.l State.Failure(a4)
	bne.w failed
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
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
; A4=State; passive peak observation, preserves registers and caller's CCR via macro.
observePeak	.block
	move.l d0, -(sp)
	move.l State.Reserved(a4), d0
	cmp.l PeakPayload, d0
	bls.w done
	move.l d0, PeakPayload
done
	move.l (sp)+, d0
	rts
	.bend  ; observePeak
.endif
.endif
.endif
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	.pub
	.section bss, kind=bss
DiskReads	.res byte, 4
DiskReadBytes	.res byte, 4
DiskWrites	.res byte, 4
DiskWriteBytes	.res byte, 4
Faults	.res byte, 4
Evictions	.res byte, 4
PeakPayload	.res byte, 4
	.endsection
.endif
.endif
.endif
	.endmodule
