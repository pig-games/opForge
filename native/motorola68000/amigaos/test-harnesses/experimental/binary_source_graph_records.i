; Harness-only numeric record materialization. Graph spans refer to original
; packed offsets; source strings are neither inspected nor reconstructed.
; Inputs are GraphSpans/OrderedCount and the original Records/FileSpans blocks.
; Output replaces Records/FileSpans only after complete validation and copying.
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
	move.l OrderedCount, d0
	mulu.w #SPAN_BYTES, d0
	move.l d0, d5
	jsr memory.reserve
	bne.w bad
	move.l d5, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a5
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l OrderedCount, d7
	moveq #0, d6
copySpan
	move.l d6, Span.Start(a5)
	move.l Span.File(a4), Span.File(a5)
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
	add.l d5, d6
	move.l d6, Span.End(a5)
	adda.w #SPAN_BYTES, a4
	adda.w #SPAN_BYTES, a5
	subq.l #1, d7
	bne.w copySpan
	lea Records, a0
	lea OrderedRecords, a1
	bsr.w replaceBlock
	lea FileSpans, a0
	lea OrderedFiles, a1
	bsr.w replaceBlock
	move.l OrderedCount, SpanCount
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
