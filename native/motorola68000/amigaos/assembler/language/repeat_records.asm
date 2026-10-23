; Snapshot one scoped repetition into an offset-addressed typed address record.
; @opforge-owner: opforge.assembler.language.repeat_records
	.module opforge.assembler.language.repeat_records
	.cpu 68020
	.use opforge.assembler.language.repeat_inventory as inventory
	.use opforge.assembler.values.records as records
	.use opforge.assembler.values.values as values
	.use opforge.platform.memory as memory
	.use opforge.packages.format as pkg
	.use opforge.vm.exprvm.runtime as runtime
	.pub
State	.struct
Buffer	.res memory.Block.Used+4
.endstruct
STATE_BYTES = memory.Block.Used+4
	.section code, kind=code
; A0=State,A1=inventory,A2=context,D0=headeroffset,D1=startPC,D2=startSection.
; D0/CCR=status,D3=recordoffset on success; others preserved. Temporary cells
; use one owned buffer; descriptors retain only numeric keys and arena offsets.
capture	.block
	movem.l d1-d2/d4-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	movea.l a2, a4
	move.l d1, d6
	move.l d2, d7
	tst.l pkg.Context.Owner(a4)
	beq.w invalid
	movea.l a1, a0
	jsr inventory.find
	bne.w done
	move.l d2, d4
	move.l d3, d5
	move.l pkg.Context.Count(a4), d0
	cmp.l inventory.State.Symbols(a5), d0
	bhi.w invalid
	move.l inventory.State.MemberKeys(a5), d3
	cmpi.l #(memory.LIMIT-values.CELL_BYTES)/values.FIELD_BYTES, d4
	bhi.w noSpace
	move.l d4, d0
	mulu.l #values.FIELD_BYTES, d0
	addi.l #values.CELL_BYTES, d0
	movea.l a6, a0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	move.l d4, d0
	mulu.l #values.FIELD_BYTES, d0
	addi.l #values.CELL_BYTES, d0
	move.l d0, State.Buffer+memory.Block.Used(a6)
	movea.l State.Buffer+memory.Block.Pointer(a6), a3
	move.l d6, values.Cell.Low(a3)
	clr.l values.Cell.High(a3)
	clr.l values.Cell.Kind(a3)
	move.l d7, values.Cell.Provenance(a3)
	lea values.CELL_BYTES(a3), a2
	movea.l inventory.State.Ids+memory.Block.Pointer(a5), a1
	adda.l d5, a1
	movea.l a1, a5
	moveq #0, d7
	tst.l d4
	beq.w publish
	movea.l d3, a1
	move.l a1, d0
	beq.w invalid
	btst #0, d0
	bne.w invalid
	move.l pkg.Context.Count(a4), d1
	add.l d1, d1
	add.l d1, d0
	bcs.w invalid
	movea.l a1, a3
	tst.l d4
	beq.w publish
	tst.l pkg.Context.Defined(a4)
	beq.w invalid
	tst.l pkg.Context.Values(a4)
	beq.w invalid
next
	tst.l d4
	beq.w publish
	moveq #0, d1
	move.w (a5)+, d1
	cmp.l pkg.Context.Count(a4), d1
	bhs.w invalid
	movea.l pkg.Context.Defined(a4), a0
	tst.b 0(a0, d1.l)
	beq.w skipped
	move.l d1, d5
	add.l d5, d5
	moveq #0, d6
	move.w 0(a3, d5.l), d6
	cmpi.w #$ffff, d6
	beq.w skipped
	move.l d6, values.Field.Member(a2)
	movea.l pkg.Context.Owner(a4), a0
	move.l a0, d0
	beq.w invalid
	jsr values.getKind
	bne.w done
	move.l d2, values.Field.Value+values.Cell.Kind(a2)
	move.l d1, d5
	lsl.l #3, d5
	movea.l pkg.Context.Values(a4), a0
	move.l runtime.Value.Low(a0, d5.l), values.Field.Value+values.Cell.Low(a2)
	move.l runtime.Value.High(a0, d5.l), values.Field.Value+values.Cell.High(a2)
	moveq #0, d6
	movea.l pkg.Context.SectionIds(a4), a0
	move.l a0, d0
	beq.w provenanceReady
	move.b 0(a0, d1.l), d6
	ext.w d6
	ext.l d6
provenanceReady
	move.l d6, values.Field.Value+values.Cell.Provenance(a2)
	adda.l #values.FIELD_BYTES, a2
	addq.l #1, d7
skipped
	subq.l #1, d4
	bra.w next
publish
	movea.l pkg.Context.Owner(a4), a0
	movea.l State.Buffer+memory.Block.Pointer(a6), a1
	lea values.CELL_BYTES(a1), a2
	move.l d7, d0
	move.l d7, d1
	mulu.l #values.FIELD_BYTES, d1
	jsr records.appendRecord
	bne.w done
	move.l d2, d3
	bra.w done
invalid
	moveq #values.MALFORMED, d0
	bra.w done
noSpace
	moveq #values.STORAGE, d0
done
	movem.l (sp)+, d1-d2/d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; capture
; A0=State. Other registers and D0/CCR status preserved; frees scratch only.
release	.block
	jsr memory.release
	clr.l State.Buffer+memory.Block.Used(a0)
	tst.l d0
	rts
	.bend  ; release
	.endsection
	.endmodule
