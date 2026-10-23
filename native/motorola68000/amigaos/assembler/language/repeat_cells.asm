; Sparse iteration storage. All retained identities are offsets, never pointers.
; @opforge-owner: opforge.assembler.language.repeat_cells
	.module opforge.assembler.language.repeat_cells
	.cpu 68020
	.use opforge.platform.memory as memory
	.use opforge.packages.format as pkg
	.use opforge.assembler.values.values as values
	.use opforge.vm.exprvm.runtime as runtime
	.pub
Cell	.struct
Low	.long ?
High	.long ?
Kind	.long ?
Defined	.byte ?
Section	.byte ?
Reserved	.word ?
	.endstruct
CELL_BYTES = Cell.Reserved+2
CELL_SHIFT = 4
MAX_CELLS = memory.LIMIT/CELL_BYTES
State	.struct
Storage	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Storage+memory.Block.Used+4
OK = 0
INVALID = 1
	.section code, kind=code
; A0=state,D1=iteration count,D2=direct declaration count.
; Append (iterations+one saved enclosing frame) full cell rows.
; D0/CCR=status,D3=offset on success. Other registers preserved.
; Multiplication is checked before allocation, including count=65536.
append	.block
	movem.l d1-d2/d4-d7/a0-a1, -(sp)
	move.l d1, d4
	addq.l #1, d4
	bcs.w bad
	move.l memory.Block.Used(a0), d5
	move.l d5, d3
	move.l d5, d0
	andi.l #CELL_BYTES-1, d0
	bne.w bad
	tst.l d2
	beq.w good
	cmpi.l #MAX_CELLS, d2
	bhi.w bad
	move.l #MAX_CELLS, d0
	divu.l d2, d0
	cmp.l d0, d4
	bhi.w bad
	mulu.l d2, d4
	lsl.l #CELL_SHIFT, d4
	add.l d5, d4
	bcs.w bad
	cmpi.l #memory.LIMIT, d4
	bhi.w bad
	move.l d4, d0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	adda.l d5, a1
	move.l d4, memory.Block.Used(a0)
	sub.l d5, d4
clear
	clr.l (a1)+
	subq.l #4, d4
	bne.w clear
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d2/d4-d7/a0-a1
	tst.l d0
	rts
	.bend  ; append

; A0=state. Discard provisional iterations on a pass-one retry. Allocations
; remain reusable; preparation-owned declaration inventory is separate.
reset	.block
	clr.l memory.Block.Used(a0)
	moveq #OK, d0
	rts
	.bend  ; reset

; A0=state. Release the owner after active lexical overlays have been restored.
; Registers preserved; CCR reflects the caller's D0 status after cleanup.
release	.block
	jsr memory.release
	clr.l memory.Block.Used(a0)
	tst.l d0
	rts
	.bend  ; release

; A0=state,D0=cell byte offset,A1=contiguous BE16 declaration IDs,D1=count,
; A2=context,D2=direction (zero captures, one restores). D0/CCR=status.
; Other registers preserved. The caller owns declaration availability policy;
; this routine copies all semantic state, including compound kind/provenance.
transfer	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d2, d7
	cmpi.l #1, d7
	bhi.w bad
	move.l d0, d3
	andi.l #CELL_BYTES-1, d3
	bne.w bad
	cmpi.l #MAX_CELLS, d1
	bhi.w bad
	move.l d1, d3
	lsl.l #CELL_SHIFT, d3
	add.l d0, d3
	bcs.w bad
	cmp.l memory.Block.Used(a6), d3
	bhi.w bad
	movea.l memory.Block.Pointer(a6), a5
	adda.l d0, a5
	move.l d1, d6
	movea.l a1, a4
	movea.l a2, a3
	tst.l d6
	beq.w good
next
	moveq #0, d1
	move.w (a4)+, d1
	cmp.l pkg.Context.Count(a3), d1
	bhs.w bad
	move.l d1, d5
	lsl.l #3, d5
	movea.l pkg.Context.Values(a3), a1
	adda.l d5, a1
	movea.l pkg.Context.Defined(a3), a2
	movea.l pkg.Context.Owner(a3), a0
	tst.l d7
	bne.w restore
	move.l a0, d0
	beq.w scalarCapture
	jsr values.getKind
	bne.w done
	bra.w captureValue
scalarCapture
	moveq #values.SCALAR, d2
captureValue
	move.l runtime.Value.Low(a1), Cell.Low(a5)
	move.l runtime.Value.High(a1), Cell.High(a5)
	move.l d2, Cell.Kind(a5)
	move.b 0(a2, d1.l), Cell.Defined(a5)
	clr.b Cell.Section(a5)
	movea.l pkg.Context.SectionIds(a3), a0
	move.l a0, d0
	beq.w transferred
	move.b 0(a0, d1.l), Cell.Section(a5)
	bra.w transferred
restore
	move.l Cell.Kind(a5), d2
	move.l a0, d0
	bne.w restoreKind
	tst.l d2
	bne.w bad
	bra.w restoreValue
restoreKind
	jsr values.setKind
	bne.w done
restoreValue
	move.l Cell.Low(a5), runtime.Value.Low(a1)
	move.l Cell.High(a5), runtime.Value.High(a1)
	move.b Cell.Defined(a5), 0(a2, d1.l)
	movea.l pkg.Context.SectionIds(a3), a0
	move.l a0, d0
	beq.w transferred
	move.b Cell.Section(a5), 0(a0, d1.l)
transferred
	adda.w #CELL_BYTES, a5
	subq.l #1, d6
	bne.w next
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; transfer
	.endsection
	.endmodule
