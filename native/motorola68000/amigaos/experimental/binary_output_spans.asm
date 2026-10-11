; Numeric initialized-emission ranges; no source or package string interface.
; @opforge-owner: experimental.amigaos.binary_output_spans
	.module experimental.amigaos.binary_output_spans
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_record_output as records
	.include "memory_telemetry.i"
	.pub
	.section code, kind=code
; A0=memory.Block,D1=address,D2=buffer offset,D3=bytes. Append or coalesce an
; initialized range. Disjoint ranges are stored in address order even when
; module/section execution visits their addresses out of order. Buffer offsets
; are bounded by the caller's validated assembly allocation. D0/CCR=status,
; other registers preserved. Failed growth retains the previous span list.
append	.block
	movem.l d1-d7/a0-a2, -(sp)
	movea.l a0, a2
	tst.l d3
	beq.w good
	.MEMORY_COUNTER_INC Events
	move.l d1, d0
	add.l d3, d0
	; An inclusive last address may be $ffffffff, but may never wrap past it.
	subq.l #1, d0
	cmp.l d1, d0
	blo.w bad
	move.l d2, d0
	add.l d3, d0
	bcs.w bad
	move.l memory.Block.Used(a2), d4
	beq.w fresh
	movea.l memory.Block.Pointer(a2), a1
	adda.l d4, a1
	suba.w #records.SPAN_BYTES, a1
	move.l records.Span.Address(a1), d5
	add.l records.Span.Bytes(a1), d5
	bcs.w bad
	cmp.l d5, d1
	blo.w orderedInsert
	bne.w fresh
	move.l records.Span.Offset(a1), d6
	add.l records.Span.Bytes(a1), d6
	bcs.w bad
	cmp.l d6, d2
	bne.w fresh
	move.l records.Span.Bytes(a1), d0
	add.l d3, d0
	bcs.w bad
	move.l d0, records.Span.Bytes(a1)
	bra.w good
fresh
	move.l d4, d0
	addi.l #records.SPAN_BYTES, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a2), a1
	adda.l d4, a1
	move.l d1, records.Span.Address(a1)
	move.l d2, records.Span.Offset(a1)
	move.l d3, records.Span.Bytes(a1)
	addi.l #records.SPAN_BYTES, memory.Block.Used(a2)
	bra.w good
orderedInsert
	bsr.w insert
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; append
	.priv
; A2=span allocation,D1=address,D2=payload offset,D3=bytes,D4=used bytes.
; Insert a disjoint earlier range. Validate both neighbors before allocating;
; failed growth leaves the previous list intact. D0/CCR=status; clobbers
; D5-D7/A0-A1, protected by append's public preservation boundary.
insert	.block
	movea.l memory.Block.Pointer(a2), a1
	move.l d4, d6
neighbor
	cmp.l records.Span.Address(a1), d1
	blo.w before
	move.l records.Span.Address(a1), d5
	add.l records.Span.Bytes(a1), d5
	bcs.w bad
	cmp.l d5, d1
	blo.w bad  ; overlap with the preceding range
	bne.w next
	move.l records.Span.Offset(a1), d7
	add.l records.Span.Bytes(a1), d7
	bcs.w bad
	cmp.l d7, d2
	bne.w next
	; Coalesce with the previous inserted range too. Otherwise a module
	; visited below the last address would allocate one row per emission.
	move.l d1, d7
	add.l d3, d7
	subq.l #1, d7
	cmpi.l #records.SPAN_BYTES, d6
	beq.w extend  ; no following range
	cmp.l records.SPAN_BYTES+records.Span.Address(a1), d7
	bhs.w bad
extend
	move.l records.Span.Bytes(a1), d7
	add.l d3, d7
	bcs.w bad
	move.l d7, records.Span.Bytes(a1)
	moveq #0, d0
	rts
next
	adda.w #records.SPAN_BYTES, a1
	subi.l #records.SPAN_BYTES, d6
	bne.w neighbor
	bra.w bad  ; append already owns the terminal insertion path
before
	move.l d1, d5
	add.l d3, d5
	subq.l #1, d5
	cmp.l records.Span.Address(a1), d5
	bhs.w bad  ; overlap with the following range
	move.l a1, d7
	sub.l memory.Block.Pointer(a2), d7
	move.l d4, d0
	addi.l #records.SPAN_BYTES, d0
	bcs.w bad
	movea.l a2, a0
	jsr memory.reserve
	bne.w bad
	; Refresh after growth and shift the complete trailing rows backwards.
	movea.l memory.Block.Pointer(a2), a0
	adda.l d4, a0
	lea records.SPAN_BYTES(a0), a1
	move.l d4, d5
	sub.l d7, d5
	lsr.l #2, d5  ; the span layout contains three aligned long fields
shift
	move.l -(a0), -(a1)
	subq.l #1, d5
	bne.w shift
	move.l d1, records.Span.Address(a0)
	move.l d2, records.Span.Offset(a0)
	move.l d3, records.Span.Bytes(a0)
	addi.l #records.SPAN_BYTES, memory.Block.Used(a2)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; insert
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
	.section bss, kind=bss
Events	.res long, 1
	.endsection
.endif
.endif
	.endmodule
