; Harness-only numeric record materialization. Graph spans refer to original
; packed offsets; source strings are neither inspected nor reconstructed.
; Inputs are GraphSpans/OrderedCount and the original Records/FileSpans blocks.
; Output replaces Records/OriginSpans only after validation and copying.
; D0/CCR=status; other registers preserved. Temporary blocks remain cleanup-owned.
materializeOrder .block
	movem.l d1-d7/a0-a6, -(sp)
	move.l OrderedCount, d7
	beq.w bad
	cmpi.l #$ffff, d7
	bhi.w bad
	move.l d7, d0
	mulu.w #SPAN_BYTES, d0
	lea GraphSpans, a0
	cmp.l memory.Block.Capacity(a0), d0
	bhi.w bad
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a4
	moveq #0, d6
validate
	move.l Span.File(a4), d0
	beq.w bad
	cmp.l SourceCount, d0
	bhi.w bad
	subq.l #1, d0
	mulu.w #SPAN_BYTES, d0
	lea FileSpans, a0
	movea.l memory.Block.Pointer(a0), a1
	adda.l d0, a1
	move.l Span.Start(a4), d4
	cmp.l Span.Start(a1), d4
	blo.w bad
	move.l Span.End(a4), d3
	cmp.l Span.End(a1), d3
	bhi.w bad
	cmp.l d4, d3
	blo.w bad
	lea Records, a0
	cmp.l memory.Block.Used(a0), d3
	bhi.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d4, a1
	move.l d4, d2
record
	cmp.l d3, d2
	beq.w spanValid
	moveq #0, d0
	move.b (a1), d0
	addq.l #1, d0
	cmpi.l #4, d0
	blo.w bad
	add.l d0, d2
	bcs.w bad
	cmp.l d3, d2
	bhi.w bad
	adda.l d0, a1
	bra.w record
spanValid
	sub.l d4, d3
	add.l d3, d6
	bcs.w bad
	adda.w #SPAN_BYTES, a4
	subq.l #1, d7
	bne.w validate
	lea Records, a0
	cmp.l memory.Block.Used(a0), d6
	bhi.w bad
	lea OrderedRecords, a0
	move.l d6, d0
	jsr memory.reserve
	bne.w bad
	move.l d6, memory.Block.Used(a0)
	lea OrderedFiles, a0
	move.l OriginCount, d0
	move.l OrderedCount, d1
	add.l d1, d1
	add.l d1, d0
	bcs.w bad
	cmpi.l #65535, d0
	bhi.w bad
	mulu.w #SPAN_BYTES, d0
	jsr memory.reserve
	bne.w bad
	clr.l memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a5
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l OrderedCount, d7
	moveq #0, d6
copySpan
	move.l d6, d4
	move.l Span.End(a4), d5
	sub.l Span.Start(a4), d5
	lea OrderedRecords, a0
	movea.l memory.Block.Pointer(a0), a1
	adda.l d6, a1
	lea Records, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l Span.Start(a4), a0
	move.l d5, d0
	bsr.w copy
	; Intersect this module's packed range with each physical-origin run.
	lea OriginSpans, a0
	movea.l memory.Block.Pointer(a0), a3
	move.l OriginCount, d3
origins
	tst.l d3
	beq.w originsDone
	move.l Span.Start(a3), d1
	cmp.l Span.End(a4), d1
	bhs.w nextOrigin
	move.l Span.End(a3), d2
	cmp.l Span.Start(a4), d2
	bls.w nextOrigin
	cmp.l Span.Start(a4), d1
	bhs.w startReady
	move.l Span.Start(a4), d1
startReady
	cmp.l Span.End(a4), d2
	bls.w endReady
	move.l Span.End(a4), d2
endReady
	cmp.l d1, d2
	bls.w nextOrigin
	sub.l Span.Start(a4), d1
	add.l d4, d1
	sub.l Span.Start(a4), d2
	add.l d4, d2
	move.l d1, Span.Start(a5)
	move.l d2, Span.End(a5)
	move.l Span.File(a3), Span.File(a5)
	adda.w #SPAN_BYTES, a5
	lea OrderedFiles, a0
	addi.l #SPAN_BYTES, memory.Block.Used(a0)
nextOrigin
	adda.w #SPAN_BYTES, a3
	subq.l #1, d3
	bra.w origins
originsDone
	add.l d5, d6
	adda.w #SPAN_BYTES, a4
	subq.l #1, d7
	bne.w copySpan
	lea Records, a0
	lea OrderedRecords, a1
	bsr.w replaceBlock
	lea OriginSpans, a0
	lea OrderedFiles, a1
	bsr.w replaceBlock
	lea OriginSpans, a0
	move.l memory.Block.Used(a0), d0
	divu.w #SPAN_BYTES, d0
	move.l d0, OriginCount
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; materializeOrder

; A0=old block,A1=replacement. Release old ownership and move descriptor.
; Temporary ownership is cleared so ordinary cleanup cannot double-free it.
replaceBlock .block
	jsr memory.release
	move.l memory.Block.Pointer(a1), memory.Block.Pointer(a0)
	move.l memory.Block.Capacity(a1), memory.Block.Capacity(a0)
	move.l memory.Block.Used(a1), memory.Block.Used(a0)
	clr.l memory.Block.Pointer(a1)
	clr.l memory.Block.Capacity(a1)
	clr.l memory.Block.Used(a1)
	rts
	.bend ; replaceBlock
