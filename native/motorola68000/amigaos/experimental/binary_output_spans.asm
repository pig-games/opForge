; Numeric last-write-wins initialized-emission ranges; no source interface.
; @opforge-owner: experimental.amigaos.binary_output_spans
.module experimental.amigaos.binary_output_spans
.cpu 68020
.use experimental.amigaos.binary_memory as memory
.use experimental.amigaos.binary_record_output as records
.include "memory_telemetry.i"
.priv
Overlay	.struct
Start	.long ?
After	.long ?
Required	.long ?
End	.long ?
PreviousEnd	.long ?
Located	.long ?
Left	.res records.SPAN_BYTES
Right	.res records.SPAN_BYTES
.endstruct
OVERLAY_BYTES = Overlay.Right+records.SPAN_BYTES
.section code, kind=code
.pub
; A0=exclusively owned memory.Block,D1=address,D2=payload offset,D3=bytes.
; D0/CCR=status (0 success,1 invalid/capacity); all other registers preserved.
; Incoming initialized bytes replace earlier writes. Rows stay address-sorted
; and disjoint; coalescing requires contiguous address AND payload offsets.
; Address intervals use inclusive ends so $ffffffff remains representable.
; Payload exclusive ends may not wrap. Empty incoming spans are no-ops.
; Existing rows must come from this owner and remain immutable between calls.
; Ordinary terminal append/coalescing examines only the last canonical row;
; overlay validates the entire affected table before resizing or mutation.
; Failed validation/reserve preserves rows/Used. No source pointers retained.
append	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #OVERLAY_BYTES, sp
	movea.l a0, a2
	movea.l sp, a6
	tst.l d3
	beq.w good
	.MEMORY_COUNTER_INC Events
	move.l d3, d0
	subq.l #1, d0
	add.l d1, d0
	bcs.w bad
	move.l d0, Overlay.End(a6)
	move.l d2, d0
	add.l d3, d0
	bcs.w bad
	bsr.w blockBounds
	bne.w bad
	move.l memory.Block.Used(a2), d4
	beq.w fresh
	cmpi.l #records.SPAN_BYTES, d4
	blo.w bad
	movea.l memory.Block.Pointer(a2), a1
	adda.l d4, a1
	suba.w #records.SPAN_BYTES, a1
	bsr.w rowBounds
	bne.w bad
	; D0=last inclusive end. No address follows an end of $ffffffff.
	cmp.l d0, d1
	bls.w overlay
	addq.l #1, d0
	cmp.l d0, d1
	bne.w fresh
	move.l records.Span.Offset(a1), d0
	add.l records.Span.Bytes(a1), d0
	cmp.l d0, d2
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
	movea.l a2, a0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a2), a1
	adda.l d4, a1
	move.l d1, records.Span.Address(a1)
	move.l d2, records.Span.Offset(a1)
	move.l d3, records.Span.Bytes(a1)
	addi.l #records.SPAN_BYTES, memory.Block.Used(a2)
	bra.w good
overlay
	bsr.w replace
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #OVERLAY_BYTES, sp
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; append
.priv
; A2=owner. Validate readable table extent; empty allocated storage is legal.
blockBounds	.block
	move.l a2, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l memory.Block.Capacity(a2), d0
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	cmp.l memory.Block.Used(a2), d0
	blo.w bad
	tst.l d0
	beq.w empty
	move.l memory.Block.Pointer(a2), d5
	beq.w bad
	btst #0, d5
	bne.w bad
	add.l d5, d0
	bcs.w bad
	bra.w good
empty
	tst.l memory.Block.Pointer(a2)
	bne.w bad
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; blockBounds
; A1=row. Validate nonempty address/payload intervals; D0=inclusive address end.
; D5 scratch, other registers unchanged.
rowBounds	.block
	move.l records.Span.Bytes(a1), d0
	beq.w bad
	move.l records.Span.Offset(a1), d5
	add.l d0, d5
	bcs.w bad
	subq.l #1, d0
	add.l records.Span.Address(a1), d0
	bcs.w bad
	move.l d0, d5
	move.l d5, d0
	cmp.l d0, d0  ; explicit successful CCR independently of unsigned end
	rts
bad
	moveq #1, d0
	rts
	.bend  ; rowBounds
.include "binary_output_overlay.i"
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
