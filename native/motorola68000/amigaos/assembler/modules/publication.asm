; Convert public source addresses to owned module-fragment references.
; @opforge-owner: opforge.assembler.modules.publication
	.module opforge.assembler.modules.publication
	.cpu 68020
	.use opforge.assembler.modules.semantic_exports as interface
	.use opforge.assembler.modules.layout as layout
	.use opforge.platform.memory as memory
	.use opforge.assembler.values.values as values
	.pub
OK = 0
INVALID = 1
	.section code, kind=code
	.pub
; A0=owned semantic memory.Block,A1=completed module layout.State.
; D0/CCR=status; other registers preserved. Mutates only the private semantic
; image. Failure may leave partial changes; its owner must discard the image.
; Exact local label records anchor public scalars. Compound scalar cells lack
; an individual source identity, so section-bearing compound exports reject.
finalize	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a3
	movea.l a1, a5
	move.l memory.Block.Used(a3), d0
	movea.l memory.Block.Pointer(a3), a0
	jsr interface.validate
	bne.w bad
	movea.l memory.Block.Pointer(a3), a6
	movea.l a6, a4
	adda.l interface.Header.Exports(a6), a4
	move.l interface.Header.ExportCount(a6), d7
next
	tst.l d7
	beq.w complete
	move.l interface.Export.Kind(a4), d3
	beq.w scalar
	cmpi.l #interface.FRAGMENT_ADDRESS, d3
	beq.w bad
	movea.l a6, a1
	adda.l interface.Header.Arena(a6), a1
	adda.l interface.Export.Low(a4), a1
	bsr.w absoluteCells
	bne.w bad
	bra.w advance
scalar
	move.l interface.Export.Provenance(a4), d2
	beq.w advance
	tst.l interface.Export.High(a4)
	bne.w bad
	subq.l #1, d2
	move.l interface.Export.Id(a4), d1
	move.l interface.Export.Low(a4), d3
	movea.l a5, a0
	jsr layout.symbol
	bne.w bad
	tst.l d1
	ble.w bad
	move.l #interface.FRAGMENT_ADDRESS, interface.Export.Kind(a4)
	move.l d2, interface.Export.Low(a4)
	move.l d1, interface.Export.High(a4)
	clr.l interface.Export.Provenance(a4)
advance
	adda.w #interface.EXPORT_BYTES, a4
	subq.l #1, d7
	bra.w next
complete
	move.l memory.Block.Used(a3), d0
	movea.l memory.Block.Pointer(a3), a0
	jsr interface.validate
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finalize
	.priv
; A1=validated compound,D3=kind. D0/CCR=status; other registers preserved.
; Existing integer lists/ranges and schema-only rows have no scalar provenance.
absoluteCells	.block
	movem.l d1/d3/a1, -(sp)
	cmpi.l #values.TYPED_LIST, d3
	beq.w typed
	cmpi.l #values.ADDRESS_RECORD, d3
	beq.w ownedAddress
	cmpi.l #values.STRUCT_LITERAL, d3
	bne.w good
	move.l values.Record.Count(a1), d1
	lea values.STRUCT_BASE_BYTES(a1), a1
	bra.w fields
ownedAddress
	move.l values.Record.Count(a1), d1
	lea values.HEADER_BYTES(a1), a1
	tst.l values.Cell.Provenance(a1)
	bne.w bad
	adda.w #values.CELL_BYTES, a1
fields
	tst.l d1
	beq.w good
	addq.l #4, a1
	tst.l values.Cell.Provenance(a1)
	bne.w bad
	adda.w #values.CELL_BYTES, a1
	subq.l #1, d1
	bra.w fields
typed
	move.l values.Record.Count(a1), d1
	lea values.HEADER_BYTES(a1), a1
next
	tst.l d1
	beq.w good
	tst.l values.Cell.Provenance(a1)
	bne.w bad
	adda.w #values.CELL_BYTES, a1
	subq.l #1, d1
	bra.w next
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1/d3/a1
	tst.l d0
	rts
	.bend  ; absoluteCells
	.endsection
	.endmodule
