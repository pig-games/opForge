; Scoped root initialization and dependency membership, with no evaluator.
; The shared dependency walk remains the only scalar constant graph solver.
; @opforge-owner: experimental.amigaos.binary_repeat_dependencies
	.module experimental.amigaos.binary_repeat_dependencies
	.cpu 68020
	.use experimental.amigaos.binary_dependency_layout as graph
	.use experimental.amigaos.binary_repeat_inventory as inventory
	.use experimental.amigaos.binary_package as pkg
	.use opasm.amigaos.binary_expression as expression
	.pub
	.section code, kind=code
; A0=graph.Frame. Initialize only directly-owned declarations.
; D0/CCR=status,D1=scalar root count. Other registers preserved.
; Live values/availability are untouched, including pass-one final values.
init	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l graph.Frame.Context(a6), a5
	movea.l graph.Frame.Declarations(a6), a4
	movea.l graph.Frame.States(a6), a3
	movea.l graph.Frame.Offsets(a6), a2
	movea.l graph.Frame.Ids(a6), a1
	move.l graph.Frame.Count(a6), d7
	moveq #0, d1
	tst.l d7
	beq.w good
next
	moveq #0, d6
	move.w (a1)+, d6
	cmp.l pkg.Context.Count(a5), d6
	bhs.w bad
	move.l d6, d5
	lsl.l #inventory.DECL_SHIFT, d5
	moveq #0, d0
	move.w inventory.Decl.Owner(a4, d5.l), d0
	cmp.l graph.Frame.Owner(a6), d0
	bne.w bad
	moveq #graph.LABEL, d3
	cmpi.w #inventory.LABEL, inventory.Decl.Kind(a4, d5.l)
	beq.w state
	moveq #graph.MUTABLE, d3
	cmpi.w #inventory.MUTABLE, inventory.Decl.Kind(a4, d5.l)
	beq.w state
	move.w inventory.Decl.Kind(a4, d5.l), d0
	andi.w #inventory.KIND_MASK, d0
	cmpi.w #inventory.IMMUTABLE, d0
	bne.w bad
	move.l inventory.Decl.Record(a4, d5.l), d4
	cmp.l graph.Frame.RecordBytes(a6), d4
	bhs.w bad
	movea.l graph.Frame.Records(a6), a0
	adda.l d4, a0
	moveq #0, d0
	move.b inventory.PackedRecord.Length(a0), d0
	addq.l #1, d0
	move.l d4, d2
	add.l d0, d2
	bcs.w bad
	cmp.l graph.Frame.RecordBytes(a6), d2
	bhi.w bad
	cmpi.w #inventory.PackedRecord.Rhs+3, d0
	blo.w bad
	move.l d6, d5
	lsl.l #graph.OFFSET_SHIFT, d5
	clr.l graph.Offset.Expression(a2, d5.l)
	moveq #graph.LAYOUT, d3
	cmpi.b #expression.VALUE_TAG, inventory.PackedRecord.Rhs(a0)
	beq.w state
	cmpi.b #expression.COMPILED_TAG, inventory.PackedRecord.Rhs(a0)
	bne.w bad
	addi.l #inventory.PackedRecord.Rhs, d4
	move.l d6, d5
	lsl.l #graph.OFFSET_SHIFT, d5
	move.l d4, graph.Offset.Expression(a2, d5.l)
	addq.l #1, d1
	move.l d6, d5
	lsl.l #inventory.DECL_SHIFT, d5
	move.w inventory.Decl.Kind(a4, d5.l), d0
	andi.w #inventory.CONDITIONAL, d0
	beq.w unconditional
	move.l d6, d5
	lsl.l #graph.OFFSET_SHIFT, d5
	clr.l graph.Offset.Expression(a2, d5.l)
	bra.w state
unconditional
	moveq #graph.PENDING, d3
state
	move.b d3, 0(a3, d6.l)
	subq.l #1, d7
	bne.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; init

; A0=graph.Frame,D0=dependency ID. D2=graph state, other registers preserved.
; Only this owner's immutable nodes participate in DFS. External immutable
; cells may supply an already available value; mutable/label dependencies defer.
; No runtime Defined byte is ever interpreted as a graph visit state.
state	.block
	movem.l d0-d1/d3/a0-a3, -(sp)
	movea.l a0, a3
	movea.l graph.Frame.Context(a3), a2
	movea.l graph.Frame.States(a3), a1
	movea.l graph.Frame.Declarations(a3), a0
	move.l d0, d1
	lsl.l #inventory.DECL_SHIFT, d1
	moveq #graph.MUTABLE, d2
	cmpi.w #inventory.MUTABLE, inventory.Decl.Kind(a0, d1.l)
	beq.w done
	moveq #graph.LABEL, d2
	cmpi.w #inventory.LABEL, inventory.Decl.Kind(a0, d1.l)
	beq.w done
	moveq #0, d3
	move.w inventory.Decl.Owner(a0, d1.l), d3
	cmp.l graph.Frame.Owner(a3), d3
	bne.w external
	move.w inventory.Decl.Kind(a0, d1.l), d3
	andi.w #inventory.KIND_MASK, d3
	cmpi.w #inventory.IMMUTABLE, d3
	bne.w external
	moveq #0, d2
	move.b 0(a1, d0.l), d2
	bra.w done
external
	moveq #0, d2
	move.b 0(a1, d0.l), d2
	cmpi.b #graph.MUTABLE, d2
	beq.w done
	cmpi.b #graph.LABEL, d2
	beq.w done
	movea.l pkg.Context.Defined(a2), a0
	moveq #0, d3
	move.b 0(a0, d0.l), d3
	tst.b d3
	beq.w unavailable
	btst #7, d3
	bne.w layout
	; Available readonly values can still be section-relative aliases. Their
	; declaration must retain affine proof rather than becoming an absolute DAG.
	movea.l pkg.Context.SectionIds(a2), a0
	move.l a0, d3
	beq.w absolute
	tst.b 0(a0, d0.l)
	bne.w layout
absolute
	moveq #graph.ABSOLUTE, d2
	bra.w done
unavailable
	; A known immutable declaration can depend on layout/statement ordering.
	; A source ID with no declaration and no value remains a missing-name error.
	movea.l graph.Frame.Declarations(a3), a0
	tst.w inventory.Decl.Kind(a0, d1.l)
	bne.w layout
	tst.l d2
	beq.w done
layout
	moveq #graph.LAYOUT, d2
done
	movem.l (sp)+, d0-d1/d3/a0-a3
	rts
	.bend  ; state
	.endsection
	.endmodule
