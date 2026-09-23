; Materialize dependency-ordered packed records and their physical origins.
; @opforge-owner: experimental.amigaos.binary_ordered_records
	.module experimental.amigaos.binary_ordered_records
	.cpu 68020
	.use experimental.amigaos.binary_graph as graph
	.use experimental.amigaos.binary_memory as memory
	.pub
Frame	.struct
OrderCount	.long ?
SourceCount	.long ?
SourceSpans	.long ?
SourceSpanCount	.long ?
Spans	.long ?
Records	.long ?
Origins	.long ?
OriginCount	.long ?
OrderedRecords	.long ?
OrderedOrigins	.long ?
.endstruct
FRAME_BYTES = Frame.OrderedOrigins+4
	.section code, kind=code

; A0=Frame. Span fields are offsets in the original packed Records block;
; origins are offset spans with physical file ordinals. The caller owns all
; Block descriptors and releases temporary allocations on failure. Success
; replaces Records and Origins only after every span and allocation is valid,
; updates Frame.OriginCount, and clears temporary ownership. D0/CCR=status;
; other registers preserved. Source text is never consulted.
materialize	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l Frame.OrderCount(a6), d7
	beq.w bad
	cmpi.l #$ffff, d7
	bhi.w bad
	move.l d7, d0
	mulu.w #graph.SPAN_BYTES, d0
	movea.l Frame.Spans(a6), a0
	cmp.l memory.Block.Capacity(a0), d0
	bhi.w bad
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a4
	moveq #0, d6
validate
	move.l graph.Span.File(a4), d0
	beq.w bad
	cmp.l Frame.SourceCount(a6), d0
	bhi.w bad
	movea.l Frame.SourceSpans(a6), a0
	movea.l memory.Block.Pointer(a0), a2
	move.l Frame.SourceSpanCount(a6), d1
findLoad
	tst.l d1
	beq.w bad
	cmp.l graph.Span.File(a2), d0
	bne.w nextLoad
	move.l graph.Span.Start(a4), d4
	cmp.l graph.Span.Start(a2), d4
	blo.w nextLoad
	move.l graph.Span.End(a4), d3
	cmp.l graph.Span.End(a2), d3
	bhi.w nextLoad
	cmp.l d4, d3
	blo.w bad
	bra.w loadFound
nextLoad
	adda.w #graph.SPAN_BYTES, a2
	subq.l #1, d1
	bra.w findLoad
loadFound
	movea.l Frame.Records(a6), a0
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
	adda.w #graph.SPAN_BYTES, a4
	subq.l #1, d7
	bne.w validate
	movea.l Frame.Records(a6), a0
	cmp.l memory.Block.Used(a0), d6
	bhi.w bad
	movea.l Frame.OrderedRecords(a6), a0
	move.l d6, d0
	jsr memory.reserve
	bne.w bad
	move.l d6, memory.Block.Used(a0)
	movea.l Frame.OrderedOrigins(a6), a0
	move.l Frame.OriginCount(a6), d0
	move.l Frame.OrderCount(a6), d1
	add.l d1, d1
	add.l d1, d0
	bcs.w bad
	cmpi.l #65535, d0
	bhi.w bad
	mulu.w #graph.SPAN_BYTES, d0
	jsr memory.reserve
	bne.w bad
	clr.l memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a5
	movea.l Frame.Spans(a6), a0
	movea.l memory.Block.Pointer(a0), a4
	move.l Frame.OrderCount(a6), d7
	moveq #0, d6
copySpan
	move.l d6, d4
	move.l graph.Span.End(a4), d5
	sub.l graph.Span.Start(a4), d5
	movea.l Frame.OrderedRecords(a6), a0
	movea.l memory.Block.Pointer(a0), a1
	adda.l d6, a1
	movea.l Frame.Records(a6), a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l graph.Span.Start(a4), a0
	move.l d5, d0
	bsr.w copy
	; Intersect this module's packed range with each physical-origin run.
	movea.l Frame.Origins(a6), a0
	movea.l memory.Block.Pointer(a0), a3
	move.l Frame.OriginCount(a6), d3
origins
	tst.l d3
	beq.w originsDone
	move.l graph.Span.Start(a3), d1
	cmp.l graph.Span.End(a4), d1
	bhs.w nextOrigin
	move.l graph.Span.End(a3), d2
	cmp.l graph.Span.Start(a4), d2
	bls.w nextOrigin
	cmp.l graph.Span.Start(a4), d1
	bhs.w startReady
	move.l graph.Span.Start(a4), d1
startReady
	cmp.l graph.Span.End(a4), d2
	bls.w endReady
	move.l graph.Span.End(a4), d2
endReady
	cmp.l d1, d2
	bls.w nextOrigin
	sub.l graph.Span.Start(a4), d1
	add.l d4, d1
	sub.l graph.Span.Start(a4), d2
	add.l d4, d2
	move.l d1, graph.Span.Start(a5)
	move.l d2, graph.Span.End(a5)
	move.l graph.Span.File(a3), graph.Span.File(a5)
	adda.w #graph.SPAN_BYTES, a5
	movea.l Frame.OrderedOrigins(a6), a0
	addi.l #graph.SPAN_BYTES, memory.Block.Used(a0)
nextOrigin
	adda.w #graph.SPAN_BYTES, a3
	subq.l #1, d3
	bra.w origins
originsDone
	add.l d5, d6
	adda.w #graph.SPAN_BYTES, a4
	subq.l #1, d7
	bne.w copySpan
	movea.l Frame.Records(a6), a0
	movea.l Frame.OrderedRecords(a6), a1
	bsr.w replaceBlock
	movea.l Frame.Origins(a6), a0
	movea.l Frame.OrderedOrigins(a6), a1
	bsr.w replaceBlock
	movea.l Frame.Origins(a6), a0
	move.l memory.Block.Used(a0), d0
	divu.w #graph.SPAN_BYTES, d0
	move.l d0, Frame.OriginCount(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; materialize

	.priv
; A0=old Block,A1=replacement. Transfers ownership after releasing old data.
replaceBlock	.block
	jsr memory.release
	move.l memory.Block.Pointer(a1), memory.Block.Pointer(a0)
	move.l memory.Block.Capacity(a1), memory.Block.Capacity(a0)
	move.l memory.Block.Used(a1), memory.Block.Used(a0)
	clr.l memory.Block.Pointer(a1)
	clr.l memory.Block.Capacity(a1)
	clr.l memory.Block.Used(a1)
	rts
	.bend  ; replaceBlock

; A0=source,A1=destination,D0=byte count. D0 preserved; A0/A1 advance.
copy	.block
	move.l d0, -(sp)
	tst.l d0
	beq.w copied
copyByte
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyByte
copied
	move.l (sp)+, d0
	rts
	.bend  ; copy
	.endsection
	.endmodule
