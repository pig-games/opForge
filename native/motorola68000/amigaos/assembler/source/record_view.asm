; Borrowed packed-record views over contiguous buffers or a bounded page store.
; Optional dependency-order metadata translates logical positions without moving
; payload. Frames own leases only; their buffer and mapping owners must outlive use.
; @opforge-owner: opforge.assembler.source.record_view
	.module opforge.assembler.source.record_view
	.cpu 68020
	.use opforge.assembler.prepare.record_store as store
	.use opforge.assembler.modules.graph as graph
	.pub
Frame	.struct
Bytes	.long ?
Offset	.long ?
Buffer	.long ?
Store	.long ?
PhysicalBytes	.long ?
Lease	.long ?
WindowStart	.long ?
WindowEnd	.long ?
WindowPointer	.long ?
Spans	.long ?
Starts	.long ?
Count	.long ?
MappedIndex	.long ?
MappedStart	.long ?
MappedEnd	.long ?
PhysicalStart	.long ?
.endstruct
FRAME_BYTES = Frame.PhysicalStart+4
STATUS_OK = 0
STATUS_BAD = 1
STATUS_EOF = 2
	.section code, kind=code
; A0=zero/live Frame,A1=borrowed buffer,D0=bytes. D0/CCR=status;
; other registers preserved. Release an existing lease, then clear mapping/cache.
; A nonempty buffer requires a nonzero pointer. Frame owns no payload allocation.
initBuffer	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.l d0, d2
	beq.w valid
	move.l a1, d0
	beq.w bad
	add.l d2, d0
	bcs.w bad
valid
	bsr.w finish
	bne.w done
	bsr.w clear
	move.l a1, Frame.Buffer(a0)
	move.l d2, Frame.Bytes(a0)
	move.l d2, Frame.PhysicalBytes(a0)
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; initBuffer

; A0=zero/live Frame,A1=borrowed configured store State. D0/CCR=status;
; other registers preserved. Snapshot its logical extent; append after init is
; outside this view. Release an existing lease and clear all mapping/cache state.
initStore	.block
	movem.l d1/a0-a1, -(sp)
	move.l a1, d0
	beq.w bad
	bsr.w finish
	bne.w done
	bsr.w clear
	move.l a1, Frame.Store(a0)
	move.l store.State.LogicalUsed(a1), d1
	move.l d1, Frame.Bytes(a0)
	move.l d1, Frame.PhysicalBytes(a0)
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; initStore

; A0=Frame,A1=borrowed graph.Span array,A2=borrowed ordered-start longs,
; D0=span count,D1=ordered bytes. D0/CCR=status; other registers preserved.
; Validate complete contiguous ordered coverage before replacing a mapping.
; Duplicate starts represent empty spans; seek selects the last start <=offset.
; No mapping metadata is owned. finish retains this mapping for later reuse.
map	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a4
	move.l d0, d7
	move.l d1, d6
	moveq #0, d5
	moveq #0, d4
	cmpi.l #graph.MAX_SPANS, d7
	bhi.w bad
	movea.l a1, a3
	tst.l d7
	beq.w empty
	move.l a1, d0
	beq.w bad
	move.l a2, d0
	beq.w bad
	moveq #0, d4
validate
	move.l graph.Span.Start(a3), d2
	move.l graph.Span.End(a3), d3
	cmp.l d2, d3
	blo.w bad
	cmp.l Frame.PhysicalBytes(a4), d3
	bhi.w bad
	cmp.l 0(a2, d4.l), d5
	bne.w bad
	sub.l d2, d3
	add.l d3, d5
	bcs.w bad
	cmp.l d6, d5
	bhi.w bad
	adda.w #graph.SPAN_BYTES, a3
	addq.l #4, d4
	subq.l #1, d7
	bne.w validate
empty
	cmp.l d6, d5
	bne.w bad
	bsr.w finish
	bne.w done
	move.l a1, Frame.Spans(a4)
	move.l a2, Frame.Starts(a4)
	; The validated start-array byte extent gives the original span count.
	move.l d4, d0
	lsr.l #2, d0
	move.l d0, Frame.Count(a4)
	move.l d6, Frame.Bytes(a4)
	clr.l Frame.Offset(a4)
	clr.l Frame.MappedIndex(a4)
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; map

; A0=Frame,D1=logical offset. D0/CCR=0 success,1 invalid,2 exact EOF.
; Success A1=borrowed pointer,D2=remaining contiguous bytes, bounded by module.
; Failure/EOF clear A1/D2. All other registers, including D1/D3, preserved.
; Repeated forward seeks reuse the current page lease and mapped interval.
seek	.block
	movem.l d1/d3-d7/a0/a2-a4, -(sp)
	movea.l a0, a4
	cmp.l Frame.Bytes(a4), d1
	bhi.w bad
	beq.w eof
	move.l d1, Frame.Offset(a4)
	move.l Frame.Bytes(a4), d6
	move.l d1, d5
	tst.l Frame.Count(a4)
	beq.w physical
	move.l Frame.MappedIndex(a4), d0
	beq.w search
	cmp.l Frame.MappedStart(a4), d1
	blo.w search
	cmp.l Frame.MappedEnd(a4), d1
	blo.w mapped
search
	; upper_bound(Starts,offset), then select its preceding nonempty span.
	moveq #0, d3
	move.l Frame.Count(a4), d7
	movea.l Frame.Starts(a4), a2
searchLoop
	cmp.l d7, d3
	bhs.w selected
	move.l d3, d4
	add.l d7, d4
	lsr.l #1, d4
	move.l d4, d0
	lsl.l #2, d0
	cmp.l 0(a2, d0.l), d1
	blo.w lower
	move.l d4, d3
	addq.l #1, d3
	bra.w searchLoop
lower
	move.l d4, d7
	bra.w searchLoop
selected
	tst.l d3
	beq.w bad
	move.l d3, Frame.MappedIndex(a4)
	subq.l #1, d3
	move.l d3, d0
	lsl.l #2, d0
	move.l 0(a2, d0.l), Frame.MappedStart(a4)
	movea.l Frame.Spans(a4), a3
	mulu.w #graph.SPAN_BYTES, d3
	adda.l d3, a3
	move.l graph.Span.Start(a3), Frame.PhysicalStart(a4)
	move.l graph.Span.End(a3), d0
	sub.l graph.Span.Start(a3), d0
	add.l Frame.MappedStart(a4), d0
	move.l d0, Frame.MappedEnd(a4)
mapped
	move.l Frame.MappedEnd(a4), d6
	sub.l Frame.MappedStart(a4), d5
	add.l Frame.PhysicalStart(a4), d5
physical
	sub.l d1, d6
	move.l Frame.Store(a4), d0
	beq.w buffer
	move.l Frame.Lease(a4), d0
	beq.w fault
	cmp.l Frame.WindowStart(a4), d5
	blo.w fault
	cmp.l Frame.WindowEnd(a4), d5
	blo.w cached
fault
	movea.l a4, a0
	bsr.w finish
	bne.w failed
	movea.l Frame.Store(a4), a0
	move.l d5, d1
	jsr store.acquire
	bne.w failed
	move.l d3, Frame.Lease(a4)
	move.l a1, Frame.WindowPointer(a4)
	move.l d5, Frame.WindowStart(a4)
	add.l d5, d2
	move.l d2, Frame.WindowEnd(a4)
cached
	movea.l Frame.WindowPointer(a4), a1
	move.l d5, d0
	sub.l Frame.WindowStart(a4), d0
	adda.l d0, a1
	move.l Frame.WindowEnd(a4), d2
	sub.l d5, d2
	bra.w bounded
buffer
	movea.l Frame.Buffer(a4), a1
	adda.l d5, a1
	move.l Frame.PhysicalBytes(a4), d2
	sub.l d5, d2
bounded
	cmp.l d6, d2
	bls.w good
	move.l d6, d2
good
	moveq #STATUS_OK, d0
	bra.w done
eof
	move.l d1, Frame.Offset(a4)
	moveq #STATUS_EOF, d0
	bra.w noView
bad
	moveq #STATUS_BAD, d0
	bra.w noView
failed
	moveq #STATUS_BAD, d0
noView
	suba.l a1, a1
	moveq #0, d2
done
	movem.l (sp)+, d1/d3-d7/a0/a2-a4
	tst.l d0
	rts
	.bend  ; seek

; A0=Frame. Mark the current leased store page dirty; a buffer is already owned
; by its caller and needs no writeback. D0/CCR=status; others preserved.
changed	.block
	movem.l d1/a0, -(sp)
	move.l Frame.Store(a0), d0
	beq.w good
	move.l Frame.Lease(a0), d1
	beq.w bad
	movea.l d0, a0
	jsr store.commitView
	bra.w done
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; changed

; A0=Frame. Release its current lease, preserving Bytes/Offset and mapping.
; D0/CCR=status; all other registers preserved. Idempotent; owns no payload.
finish	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	move.l Frame.Lease(a1), d1
	beq.w good
	movea.l Frame.Store(a1), a0
	jsr store.releaseView
	bne.w done
	clr.l Frame.Lease(a1)
	clr.l Frame.WindowPointer(a1)
	clr.l Frame.WindowStart(a1)
	clr.l Frame.WindowEnd(a1)
good
	moveq #STATUS_OK, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; finish
	.priv
; A0=Frame. Clear fields after lease release. D0 clobbered; others preserved.
clear	.block
	movem.l d1/a0, -(sp)
	move.w #FRAME_BYTES/4-1, d1
next
	clr.l (a0)+
	dbra d1, next
	movem.l (sp)+, d1/a0
	rts
	.bend  ; clear
	.endsection
	.endmodule
