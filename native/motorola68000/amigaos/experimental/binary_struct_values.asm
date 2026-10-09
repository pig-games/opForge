; Nominal struct schemas and literals in the shared value owner's arena.
; @opforge-owner: experimental.amigaos.binary_struct_values
	.module experimental.amigaos.binary_struct_values
	.cpu 68020
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_memory as memory
	.pub
TYPE = values.STRUCT_TYPE
LITERAL = values.STRUCT_LITERAL
HEADER_BYTES = values.STRUCT_BASE_BYTES
FIELD_BYTES = values.STRUCT_FIELD_BYTES
NOMINAL = values.StructRecord.NOMINAL
SIZE = values.StructRecord.SIZE
FIELDS = HEADER_BYTES
	.section code, kind=code
; A0=Owner,D0=nominal ID,D1=size,A1=external SchemaField array,D3=count,
; D4=source bytes. D0/CCR=status,D2=offset; others preserved. No source may
; alias the destination block. Type schemas contain no pointers or child values.
appendType	.block
	movem.l d1/d3-d7/a0-a4, -(sp)
	lea values.Owner.Arena(a0), a2
	bra.w typeAppend
	.bend  ; appendType
; Same ABI as appendType; offset addresses retained Owner.Seeds.
appendSeed	.block
	movem.l d1/d3-d7/a0-a4, -(sp)
	lea values.Owner.Seeds(a0), a2
	bra.w typeAppend
	.bend  ; appendSeed
; A0=Owner,D0=nominal ID,A1=external Field array,D1=count,D3=field bytes.
; D0/CCR=status,D2=offset; others preserved. Scalar cells retain provenance.
appendLiteral	.block
	movem.l d1/d3-d7/a0-a4, -(sp)
	move.l d1, d5
	move.l d3, d4
	moveq #values.FIELD_BYTES, d6
	moveq #LITERAL, d7
	moveq #0, d1
	lea values.Owner.Arena(a0), a2
	bra.w append
	.bend  ; appendLiteral
	.priv
typeAppend	.block
	move.l d3, d5
	moveq #FIELD_BYTES, d6
	moveq #TYPE, d7
	bra.w append
	.bend  ; typeAppend
; Saved public register set is common to all three construction entry points.
append	.block
	move.l d0, d3
	cmp.l values.Owner.Count(a0), d3
	bhs.w invalid
	movea.l a0, a4
	movea.l a1, a3
	cmpi.l #(memory.LIMIT-HEADER_BYTES)/values.FIELD_BYTES, d5
	bhi.w noSpace
	move.l d5, d0
	mulu.l d6, d0
	cmp.l d4, d0
	bhi.w invalid
	move.l d0, d4
	movea.l a2, a1
	move.l d3, -(sp)
	jsr values.block
	move.l (sp)+, d3
	tst.l d0
	bne.w done
	tst.l d4
	beq.w checked
	move.l a3, d0
	beq.w invalid
	btst #0, d0
	bne.w invalid
	add.l d4, d0
	bcs.w invalid
	movea.l a3, a1
	move.l d5, d0
validate
	move.l (a1), d2
	cmp.l values.Owner.Count(a4), d2
	bhs.w invalid
	movea.l a3, a0
unique
	cmpa.l a1, a0
	beq.w fieldValue
	cmp.l (a0), d2
	beq.w invalid
	adda.l d6, a0
	bra.w unique
fieldValue
	cmpi.l #TYPE, d7
	bne.w literalValue
	move.l values.StructField.Offset(a1), d2
	add.l values.StructField.SIZE(a1), d2
	bcs.w invalid
	cmp.l d1, d2
	bhi.w invalid
	bra.w nextField
literalValue
	tst.l values.Field.Value+values.Cell.Kind(a1)
	bne.w invalid
	move.l values.Field.Value+values.Cell.Provenance(a1), d2
	cmpi.l #-1, d2
	beq.w nextField
	cmpi.l #values.MAX_SECTION_ID, d2
	bhi.w invalid
nextField
	adda.l d6, a1
	subq.l #1, d0
	bne.w validate
checked
	move.l memory.Block.Pointer(a2), d0
	beq.w separate
	move.l d0, d2
	add.l memory.Block.Capacity(a2), d2
	cmpa.l d2, a3
	bhs.w separate
	move.l a3, d2
	add.l d4, d2
	cmp.l d0, d2
	bhi.w invalid
separate
	move.l memory.Block.Used(a2), d2
	move.l d2, d0
	andi.l #7, d0
	bne.w invalid
	addi.l #HEADER_BYTES+7, d4
	andi.l #$fffffff8, d4
	add.l d2, d4
	bcs.w noSpace
	cmpi.l #memory.LIMIT, d4
	bhi.w noSpace
	movea.l a2, a0
	move.l d4, d0
	jsr memory.reserve
	bne.w noSpace
	movea.l memory.Block.Pointer(a2), a1
	adda.l d2, a1
	move.l d7, (a1)+
	move.l d5, (a1)+
	move.l d3, (a1)+
	move.l d1, (a1)+
	mulu.l d6, d5
	lsr.l #2, d5
copy
	tst.l d5
	beq.w padding
	move.l (a3)+, (a1)+
	subq.l #1, d5
	bra.w copy
padding
	movea.l memory.Block.Pointer(a2), a3
	adda.l d4, a3
clear
	cmpa.l a3, a1
	beq.w commit
	clr.l (a1)+
	bra.w clear
commit
	move.l d4, memory.Block.Used(a2)
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
	bra.w done
noSpace
	moveq #values.STORAGE, d0
done
	movem.l (sp)+, d1/d3-d7/a0-a4
	tst.l d0
	rts
	.bend  ; append
	.pub
; A0=Owner,D1=seed offset. D0/CCR=status,D2=type Arena offset; others preserved.
materialize	.block
	movem.l d1/d3/a1, -(sp)
	lea values.Owner.Seeds(a0), a1
	jsr values.block
	bne.w done
	move.l memory.Block.Used(a1), d0
	movea.l memory.Block.Pointer(a1), a1
	moveq #TYPE, d3
	jsr values.copyRecord
done
	movem.l (sp)+, d1/d3/a1
	tst.l d0
	rts
	.bend  ; materialize
; A0=Owner,D0=descriptor offset,D1=member ID. D0/CCR=status,D2-D5=Cell;
; others preserved. Types yield absolute offsets; literals yield scalar cells.
member	.block
	movem.l d1/d6-d7/a1, -(sp)
	move.l d1, d7
	move.l d0, d1
	jsr values.view
	bne.w done
	move.l values.Record.Kind(a1), d0
	cmpi.l #TYPE, d0
	beq.w typeMember
	cmpi.l #LITERAL, d0
	bne.w invalid
	moveq #values.FIELD_BYTES, d6
	bra.w fields
typeMember
	moveq #FIELD_BYTES, d6
fields
	move.l values.Record.Count(a1), d1
	lea HEADER_BYTES(a1), a1
scan
	tst.l d1
	beq.w bounds
	cmp.l (a1), d7
	beq.w found
	adda.l d6, a1
	subq.l #1, d1
	bra.w scan
found
	cmpi.l #FIELD_BYTES, d6
	bne.w literalResult
	move.l values.StructField.Offset(a1), d2
	moveq #0, d3
	moveq #0, d4
	moveq #0, d5
	bra.w good
literalResult
	move.l values.Field.Value+values.Cell.Low(a1), d2
	move.l values.Field.Value+values.Cell.High(a1), d3
	move.l values.Field.Value+values.Cell.Kind(a1), d4
	bne.w invalid
	move.l values.Field.Value+values.Cell.Provenance(a1), d5
	cmpi.l #-1, d5
	beq.w good
	cmpi.l #values.MAX_SECTION_ID, d5
	bhi.w invalid
good
	moveq #values.OK, d0
	bra.w done
bounds
	moveq #values.BOUNDS, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1/d6-d7/a1
	tst.l d0
	rts
	.bend  ; member
; A0=Owner,D0=literal offset. Match the assembler's absolute field conversion.
; D0/CCR=status; others preserved. Raw shared VM construction remains unrestricted.
assemblerLiteral	.block
	movem.l d1-d3/a1, -(sp)
	move.l d0, d1
	jsr values.view
	bne.w done
	cmpi.l #LITERAL, values.Record.Kind(a1)
	bne.w invalid
	move.l values.Record.Count(a1), d2
	lea HEADER_BYTES(a1), a1
field
	tst.l d2
	beq.w good
	tst.l values.Field.Value+values.Cell.Kind(a1)
	bne.w invalid
	tst.l values.Field.Value+values.Cell.Provenance(a1)
	bne.w invalid
	adda.l #values.FIELD_BYTES, a1
	subq.l #1, d2
	bra.w field
good
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d3/a1
	tst.l d0
	rts
	.bend  ; assemblerLiteral
; A0=destination Owner,A1=external seed block,D0=bytes. Install before replay.
; D0/CCR=status; others preserved. Resets and rewinds retain these schemas.
installSeeds	.block
	movem.l d1-d6/a0-a3, -(sp)
	movea.l a0, a3
	movea.l a1, a2
	move.l d0, d6
	cmpi.l #memory.LIMIT, d6
	bhi.w invalid
	lea values.Owner.Seeds(a3), a1
	jsr values.block
	bne.w done
	tst.l d6
	beq.w validated
	move.l a2, d4
	beq.w invalid
	btst #0, d4
	bne.w invalid
	add.l d6, d4
	bcs.w invalid
	move.l memory.Block.Pointer(a1), d0
	beq.w separate
	cmp.l d0, d4
	bls.w separate
	add.l memory.Block.Capacity(a1), d0
	cmp.l a2, d0
	bhi.w invalid
separate
	move.l d6, d5
	movea.l a2, a1
scan
	tst.l d5
	beq.w validated
	cmpi.l #HEADER_BYTES, d5
	blo.w invalid
	cmpi.l #TYPE, values.Record.Kind(a1)
	bne.w invalid
	jsr values.descriptorBytes
	bne.w done
	cmp.l d5, d2
	bhi.w invalid
	adda.l d2, a1
	sub.l d2, d5
	bra.w scan
validated
	lea values.Owner.Seeds(a3), a0
	move.l d6, d0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	move.l d6, d1
copy
	tst.l d1
	beq.w commit
	move.b (a2)+, (a1)+
	subq.l #1, d1
	bra.w copy
commit
	move.l d6, memory.Block.Used(a0)
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d6/a0-a3
	tst.l d0
	rts
	.bend  ; installSeeds
; A0=Owner,A1=nominal binder callback (D0=old ID => D0=status,D1=new ID,
; all other registers preserved), A6=callback context. D0/CCR=status;
; other registers preserved. Finalize nominal IDs in seeds and copied snapshots.
remapNominals	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	lea values.Owner.Seeds(a5), a0
	bsr.w remapBlock
	bne.w done
	lea values.Owner.Arena(a5), a0
	bsr.w remapBlock
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; remapNominals
	.priv
; A0=Block,A4=callback,A6=context. Preserve A4-A6 and D1-D7.
remapBlock	.block
	movem.l d1-d7/a0-a3, -(sp)
	movea.l a0, a1
	jsr values.block
	bne.w done
	move.l memory.Block.Used(a1), d7
	movea.l memory.Block.Pointer(a1), a3
record
	tst.l d7
	beq.w good
	cmpi.l #values.HEADER_BYTES, d7
	blo.w bad
	movea.l a3, a1
	jsr values.descriptorBytes
	bne.w done
	cmp.l d7, d2
	bhi.w bad
	move.l d2, d6
	cmpi.l #TYPE, values.Record.Kind(a3)
	blo.w next
	cmpi.l #LITERAL, values.Record.Kind(a3)
	bhi.w bad
	move.l NOMINAL(a3), d0
	jsr (a4)
	tst.l d0
	bne.w done
	move.l d1, NOMINAL(a3)
next
	adda.l d6, a3
	sub.l d6, d7
	bra.w record
good
	moveq #values.OK, d0
	bra.w done
bad
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; remapBlock
	.pub
; A0=Owner,D0=offset. D0/CCR=status; other registers preserved.
validate	.block
	movem.l d1/d3/a1, -(sp)
	move.l d0, d1
	jsr values.view
	bne.w done
	cmpi.l #TYPE, values.Record.Kind(a1)
	blo.w invalid
	cmpi.l #LITERAL, values.Record.Kind(a1)
	bhi.w invalid
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1/d3/a1
	tst.l d0
	rts
	.bend  ; validate
	.endsection
	.endmodule
