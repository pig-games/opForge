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
; initialized range. Addresses must increase without overlap; buffer offsets
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
	blo.w bad
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
