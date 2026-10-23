; Ordered packed-record contract probe, independent of target semantics.
; @opforge-owner: opforge.tests.assembler.ordered_records_probe
	.module opforge.tests.assembler.ordered_records_probe
	.cpu 68020
	.use opforge.assembler.prepare.record_order as ordered
	.use opforge.assembler.modules.graph as graph
	.use opforge.platform.memory as memory
BLOCK_BYTES = memory.Block.Used+4
BLOCK_COUNT = 7
INPUT_BLOCK_COUNT = 4
RECORD_BYTES = 12
PROBE_FAILED = 88
	.pub
	.section code, kind=code
; No inputs. D0/CCR=status; other registers preserved. Every allocation released.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #PROBE_FAILED, d7
	; Identity: fresh ownership for each independent case.
	bsr.w setup
	bne.w finish
	move.l #4, Frame+ordered.Frame.OrderCount
	lea IdentitySpans, a1
	lea Spans, a0
	move.l #4*graph.SPAN_BYTES, d0
	bsr.w load
	lea Frame, a0
	jsr ordered.materialize
	bne.w finish
	lea Records, a0
	lea OriginalRecords, a1
	move.l #12, d0
	bsr.w equal
	bne.w finish
	lea Origins, a0
	lea OriginalOrigins, a1
	move.l #3*graph.SPAN_BYTES, d0
	bsr.w equal
	bne.w finish
	cmpi.l #3, Frame+ordered.Frame.OriginCount
	bne.w finish
	lea ModuleStarts, a0
	lea IdentityStarts, a1
	move.l #4*4, d0
	bsr.w equal
	bne.w finish
	.ifdef OPFORGE_ORDERED_RECORDS_EXPECT_REUSE
	move.l SavedRecords, d0
	cmp.l Records+memory.Block.Pointer, d0
	bne.w finish
	.endif
	lea OrderedRecords, a0
	bsr.w empty
	bne.w finish
	lea OrderedOrigins, a0
	bsr.w empty
	bne.w finish
	bsr.w cleanup
	; Reorder: fresh ownership for each independent case.
	bsr.w setup
	bne.w finish
	move.l #2, Frame+ordered.Frame.OrderCount
	lea ReorderSpans, a1
	lea Spans, a0
	move.l #2*graph.SPAN_BYTES, d0
	bsr.w load
	lea Frame, a0
	jsr ordered.materialize
	bne.w finish
	lea Records, a0
	lea ReorderRecords, a1
	move.l #12, d0
	bsr.w equal
	bne.w finish
	lea Origins, a0
	lea ReorderOrigins, a1
	move.l #3*graph.SPAN_BYTES, d0
	bsr.w equal
	bne.w finish
	cmpi.l #3, Frame+ordered.Frame.OriginCount
	bne.w finish
	lea ModuleStarts, a0
	lea ReorderStarts, a1
	move.l #2*4, d0
	bsr.w equal
	bne.w finish
	move.l SavedRecords, d0
	cmp.l Records+memory.Block.Pointer, d0
	beq.w finish
	lea OrderedRecords, a0
	bsr.w empty
	bne.w finish
	lea OrderedOrigins, a0
	bsr.w empty
	bne.w finish
	bsr.w cleanup
	; Prefix: fresh ownership for each independent case.
	bsr.w setup
	bne.w finish
	move.l #1, Frame+ordered.Frame.OrderCount
	lea PrefixSpans, a1
	lea Spans, a0
	move.l #1*graph.SPAN_BYTES, d0
	bsr.w load
	lea Frame, a0
	jsr ordered.materialize
	bne.w finish
	lea Records, a0
	lea OriginalRecords, a1
	move.l #8, d0
	bsr.w equal
	bne.w finish
	lea Origins, a0
	lea PrefixOrigins, a1
	move.l #2*graph.SPAN_BYTES, d0
	bsr.w equal
	bne.w finish
	cmpi.l #2, Frame+ordered.Frame.OriginCount
	bne.w finish
	lea ModuleStarts, a0
	lea PrefixStarts, a1
	move.l #1*4, d0
	bsr.w equal
	bne.w finish
	move.l SavedRecords, d0
	cmp.l Records+memory.Block.Pointer, d0
	beq.w finish
	lea OrderedRecords, a0
	bsr.w empty
	bne.w finish
	lea OrderedOrigins, a0
	bsr.w empty
	bne.w finish
	bsr.w cleanup
	; BadSpan: fresh ownership for each independent case.
	bsr.w setup
	bne.w finish
	move.l #1, Frame+ordered.Frame.OrderCount
	lea BadSpans, a1
	lea Spans, a0
	move.l #1*graph.SPAN_BYTES, d0
	bsr.w load
	lea Frame, a0
	jsr ordered.materialize
	beq.w finish
	lea OriginalRecords, a1
	bsr.w failureUnchanged
	bne.w finish
	bsr.w cleanup
	; BadRecord: fresh ownership for each independent case.
	bsr.w setup
	bne.w finish
	move.l #4, Frame+ordered.Frame.OrderCount
	lea IdentitySpans, a1
	lea Spans, a0
	move.l #4*graph.SPAN_BYTES, d0
	bsr.w load
	movea.l Records+memory.Block.Pointer, a1
	move.b #1, (a1)
	lea Frame, a0
	jsr ordered.materialize
	beq.w finish
	lea MalformedRecords, a1
	bsr.w failureUnchanged
	bne.w finish
	bsr.w cleanup
	; RecordLimit: fresh ownership for each independent case.
	bsr.w setup
	bne.w finish
	move.l #4, Frame+ordered.Frame.OrderCount
	lea IdentitySpans, a1
	lea Spans, a0
	move.l #4*graph.SPAN_BYTES, d0
	bsr.w load
	move.l #1, Frame+ordered.Frame.RecordLimit
	lea Frame, a0
	jsr ordered.materialize
	beq.w finish
	lea OriginalRecords, a1
	bsr.w failureUnchanged
	bne.w finish
	bsr.w cleanup
	; OriginLimit: fresh ownership for each independent case.
	bsr.w setup
	bne.w finish
	move.l #2, Frame+ordered.Frame.OrderCount
	lea ReorderSpans, a1
	lea Spans, a0
	move.l #2*graph.SPAN_BYTES, d0
	bsr.w load
	move.l #65535, Frame+ordered.Frame.OriginCount
	lea Frame, a0
	jsr ordered.materialize
	beq.w finish
	lea OriginalRecords, a1
	bsr.w failureUnchanged
	bne.w finish
	bsr.w cleanup
	moveq #0, d7
finish
	bsr.w cleanup
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
; Fresh blocks and the unchanged public Frame ABI. D0/CCR=status.
setup	.block
	bsr.w cleanup
	lea Blocks, a0
	moveq #INPUT_BLOCK_COUNT-1, d6
allocate
	move.l #256, d0
	jsr memory.reserve
	bne.w done
	adda.w #BLOCK_BYTES, a0
	dbra d6, allocate
	lea Frame, a0
	move.l #3, ordered.Frame.SourceCount(a0)
	move.l #2, ordered.Frame.SourceSpanCount(a0)
	move.l #SourceSpans, ordered.Frame.SourceSpans(a0)
	move.l #Spans, ordered.Frame.Spans(a0)
	move.l #Records, ordered.Frame.Records(a0)
	move.l #Origins, ordered.Frame.Origins(a0)
	move.l #3, ordered.Frame.OriginCount(a0)
	move.l #OrderedRecords, ordered.Frame.OrderedRecords(a0)
	move.l #OrderedOrigins, ordered.Frame.OrderedOrigins(a0)
	move.l #ModuleStarts, ordered.Frame.ModuleStarts(a0)
	move.l #256, ordered.Frame.RecordLimit(a0)
	lea SourceSpans, a0
	lea LoadSpans, a1
	moveq #2*graph.SPAN_BYTES, d0
	bsr.w load
	lea Records, a0
	lea OriginalRecords, a1
	moveq #RECORD_BYTES, d0
	bsr.w load
	lea Origins, a0
	lea OriginalOrigins, a1
	moveq #3*graph.SPAN_BYTES, d0
	bsr.w load
	move.l Records+memory.Block.Pointer, SavedRecords
	move.l Origins+memory.Block.Pointer, SavedOrigins
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; setup
; Release all caller-owned originals and temporaries, including failed calls.
cleanup	.block
	movem.l d0/d6/a0, -(sp)
	lea Blocks, a0
	moveq #BLOCK_COUNT-1, d6
release
	jsr memory.release
	move.l memory.Block.Pointer(a0), d0
	or.l memory.Block.Capacity(a0), d0
	beq.w released
	moveq #PROBE_FAILED, d7
released
	clr.l memory.Block.Used(a0)
	adda.w #BLOCK_BYTES, a0
	dbra d6, release
	movem.l (sp)+, d0/d6/a0
	rts
	.bend  ; cleanup
; A1=expected packed bytes. Failure retains original ownership and publication.
; D0/CCR=status; D1/A0-A2 clobbered.
failureUnchanged	.block
	move.l SavedRecords, d0
	cmp.l Records+memory.Block.Pointer, d0
	bne.w bad
	move.l SavedOrigins, d0
	cmp.l Origins+memory.Block.Pointer, d0
	bne.w bad
	lea Records, a0
	moveq #RECORD_BYTES, d0
	bsr.w equal
	bne.w bad
	lea Origins, a0
	lea OriginalOrigins, a1
	moveq #3*graph.SPAN_BYTES, d0
	bsr.w equal
	bne.w bad
	tst.l ModuleStarts+memory.Block.Used
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; failureUnchanged
; A0=Block,A1=source,D0=bytes; D1/A2 clobbered.
load	.block
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a2
	move.l d0, d1
copy
	move.b (a1)+, (a2)+
	subq.l #1, d1
	bne.w copy
	rts
	.bend  ; load
; A0=Block,A1=expected,D0=bytes. D0/CCR=status; D1/A1/A2 clobbered.
equal	.block
	cmp.l memory.Block.Used(a0), d0
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	move.l d0, d1
compare
	cmpm.b (a1)+, (a2)+
	bne.w bad
	subq.l #1, d1
	bne.w compare
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; equal
; A0=transferred temporary Block. D0/CCR=status.
empty	.block
	move.l memory.Block.Pointer(a0), d0
	or.l memory.Block.Capacity(a0), d0
	or.l memory.Block.Used(a0), d0
	rts
	.bend  ; empty
	.endsection
	.section data, kind=data
OriginalRecords	.byte 3, 11, 12, 13, 3, 21, 22, 23, 3, 31, 32, 33
MalformedRecords	.byte 1, 11, 12, 13, 3, 21, 22, 23, 3, 31, 32, 33
ReorderRecords	.byte 3, 31, 32, 33, 3, 11, 12, 13, 3, 21, 22, 23
LoadSpans	.long 0, 8, 1, 8, 12, 2
IdentitySpans	.long 0, 4, 1, 4, 4, 1, 4, 8, 1, 8, 12, 2
ReorderSpans	.long 8, 12, 2, 0, 8, 1
PrefixSpans	.long 0, 8, 1
BadSpans	.long 8, 13, 2
OriginalOrigins	.long 0, 4, 1, 4, 8, 3, 8, 12, 2
ReorderOrigins	.long 0, 4, 2, 4, 8, 1, 8, 12, 3
PrefixOrigins	.long 0, 4, 1, 4, 8, 3
IdentityStarts	.long 0, 4, 4, 8
ReorderStarts	.long 0, 4
PrefixStarts	.long 0
	.endsection
	.section bss, kind=bss
Frame	.res byte, ordered.FRAME_BYTES
SavedRecords	.res long, 1
SavedOrigins	.res long, 1
Blocks
SourceSpans	.res byte, BLOCK_BYTES
Spans	.res byte, BLOCK_BYTES
Records	.res byte, BLOCK_BYTES
Origins	.res byte, BLOCK_BYTES
OrderedRecords	.res byte, BLOCK_BYTES
OrderedOrigins	.res byte, BLOCK_BYTES
ModuleStarts	.res byte, BLOCK_BYTES
	.endsection
	.endmodule
