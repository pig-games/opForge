; Bounded immutable semantic-interface readers.
	.pub
; A0=image,D0=exact readable bytes. D0/CCR=OK/INVALID; others preserved.
; Validates canonical bounded regions, IDs, names and every exported descriptor
; through values.copyRecord. Temporary validation ownership is always released.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -values.OWNER_BYTES(sp), sp
	movea.l sp, a3
	movea.l sp, a1
	moveq #values.OWNER_BYTES/4-1, d1
clearOwner
	clr.l (a1)+
	dbra d1, clearOwner
	movea.l a0, a6
	move.l a0, d1
	beq.w bad
	btst #0, d1
	bne.w bad
	add.l d0, d1
	bcs.w bad
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	cmpi.l #MAGIC, Header.Magic(a6)
	bne.w bad
	cmp.l Header.Bytes(a6), d0
	bne.w bad
	cmpi.l #HEADER_BYTES, Header.Exports(a6)
	bne.w bad
	move.l Header.ExportCount(a6), d1
	cmpi.l #memory.LIMIT/EXPORT_BYTES, d1
	bhi.w bad
	mulu.l #EXPORT_BYTES, d1
	addi.l #HEADER_BYTES, d1
	cmp.l Header.Members(a6), d1
	bne.w bad
	move.l Header.MemberCount(a6), d2
	cmpi.l #memory.LIMIT/MEMBER_BYTES, d2
	bhi.w bad
	mulu.l #MEMBER_BYTES, d2
	add.l d2, d1
	bcs.w bad
	cmp.l Header.Names(a6), d1
	bne.w bad
	add.l Header.NameBytes(a6), d1
	bcs.w bad
	addq.l #7, d1
	bcs.w bad
	andi.l #$fffffff8, d1
	cmp.l Header.Arena(a6), d1
	bne.w bad
	move.l Header.ArenaBytes(a6), d2
	add.l d1, d2
	bcs.w bad
	cmp.l d0, d2
	bne.w bad
	move.l Header.ExportCount(a6), d7
	lea HEADER_BYTES(a6), a4
	moveq #0, d5
exports
	tst.l d7
	beq.w membersBegin
	tst.l d5
	beq.w firstExport
	move.l Export.Id(a4), d0
	cmp.l d6, d0
	bls.w bad
firstExport
	moveq #1, d5
	move.l Export.Id(a4), d6
	cmpi.l #65535, d6
	bhi.w bad
	tst.l Export.NameBytes(a4)
	bne.w publicName
	tst.l Export.Name(a4)
	bne.w bad
	cmpi.l #values.STRUCT_TYPE, Export.Kind(a4)
	bne.w bad
	bra.w exportValue
publicName
	bsr.w serializedName
	bne.w bad
exportValue
	move.l Export.Kind(a4), d3
	cmpi.l #FRAGMENT_ADDRESS, d3
	beq.w fragment
	cmpi.l #values.STRUCT_LITERAL, d3
	bhi.w bad
	tst.l d3
	beq.w scalar
	tst.l Export.High(a4)
	bne.w bad
	tst.l Export.Provenance(a4)
	bne.w bad
	movea.l a6, a1
	adda.l Header.Arena(a6), a1
	move.l Header.ArenaBytes(a6), d0
	move.l Export.Low(a4), d1
	movea.l a3, a0
	jsr values.copyRecord
	bne.w bad
	movea.l a6, a1
	adda.l Header.Arena(a6), a1
	adda.l Export.Low(a4), a1
	; Keep traversal ownership explicit after the bounded copy validation.
	cmp.l values.Record.Kind(a1), d3
	bne.w bad
	bsr.w compoundCells
	bne.w bad
	bsr.w closure
	bne.w bad
	bra.w nextExport
fragment
	tst.l Export.Provenance(a4)
	bne.w bad
	tst.l Export.High(a4)
	beq.w bad
	cmpi.l #65535, Export.High(a4)
	bhi.w bad
	cmpi.l #memory.LIMIT, Export.Low(a4)
	bhi.w bad
	bra.w nextExport
scalar
	cmpi.l #8, Export.Provenance(a4)
	bhi.w bad
	tst.l Export.Provenance(a4)
	beq.w nextExport
	tst.l Export.High(a4)
	bne.w bad
nextExport
	adda.w #EXPORT_BYTES, a4
	subq.l #1, d7
	bra.w exports
membersBegin
	move.l Header.MemberCount(a6), d7
	moveq #0, d5
members
	tst.l d7
	beq.w good
	tst.l d5
	beq.w firstMember
	move.l Member.Id(a4), d0
	cmp.l d6, d0
	bls.w bad
firstMember
	moveq #1, d5
	move.l Member.Id(a4), d6
	cmpi.l #65535, d6
	bhi.w bad
	bsr.w serializedName
	bne.w bad
	movea.l a6, a0
	adda.l Member.Name(a4), a0
	move.l Member.NameBytes(a4), d0
bare
	cmpi.b #'.', (a0)+
	beq.w bad
	subq.l #1, d0
	bne.w bare
	adda.w #MEMBER_BYTES, a4
	subq.l #1, d7
	bra.w members
good
	moveq #OK, d0
	bra.w cleanup
bad
	moveq #INVALID, d0
cleanup
	move.l d0, -(sp)
	movea.l a3, a0
	jsr values.release
	move.l (sp)+, d0
	lea values.OWNER_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate
	.priv
; A6=image,A4=Export/Member (shared name layout). D0/CCR=status; others kept.
serializedName	.block
	movem.l d1-d2/a0, -(sp)
	move.l Export.Name(a4), d1
	cmp.l Header.Names(a6), d1
	blo.w bad
	move.l Header.Names(a6), d2
	add.l Header.NameBytes(a6), d2
	move.l Export.NameBytes(a4), d0
	add.l d0, d1
	bcs.w bad
	cmp.l d2, d1
	bhi.w bad
	movea.l a6, a0
	adda.l Export.Name(a4), a0
	bsr.w inputName
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	rts
	.bend  ; serializedName

; A1=already validated compound,D3=kind. Enforce safe scalar provenance even
; though the general values transport also admits its internal unsafe sentinel.
; D0/CCR=status; other registers preserved.
compoundCells	.block
	movem.l d1-d3/a1, -(sp)
	move.l values.Record.Count(a1), d1
	cmpi.l #values.TYPED_LIST, d3
	beq.w list
	cmpi.l #values.ADDRESS_RECORD, d3
	beq.w address
	cmpi.l #values.STRUCT_LITERAL, d3
	bne.w good
	lea values.STRUCT_BASE_BYTES(a1), a1
	moveq #values.FIELD_BYTES, d3
	bra.w fields
address
	lea values.HEADER_BYTES(a1), a1
	bsr.w cell
	bne.w done
	adda.w #values.CELL_BYTES, a1
	moveq #values.FIELD_BYTES, d3
fields
	tst.l d1
	beq.w good
	addq.l #4, a1
	bsr.w cell
	bne.w done
	adda.w #values.CELL_BYTES, a1
	subq.l #1, d1
	bra.w fields
list
	lea values.HEADER_BYTES(a1), a1
next
	tst.l d1
	beq.w good
	bsr.w cell
	bne.w done
	adda.w #values.CELL_BYTES, a1
	subq.l #1, d1
	bra.w next
good
	moveq #OK, d0
done
	movem.l (sp)+, d1-d3/a1
	tst.l d0
	rts
cell
	cmpi.l #8, values.Cell.Provenance(a1)
	bhi.w malformed
	tst.l values.Cell.Provenance(a1)
	beq.w valid
	tst.l values.Cell.High(a1)
	bne.w malformed
valid
	moveq #OK, d0
	rts
malformed
	moveq #INVALID, d0
	rts
	.bend  ; compoundCells

; A6=bounded image,A1=bounded compound,D3=kind. Every nominal schema and
; member key must belong to this same image. Other registers preserved.
closure	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.l d3, d4
	movea.l a1, a3
	move.l values.Record.Count(a3), d7
	cmpi.l #values.STRUCT_TYPE, d4
	bhs.w nominal
	cmpi.l #values.ADDRESS_RECORD, d4
	bne.w good
	lea values.RECORD_BASE_BYTES(a3), a3
	moveq #values.FIELD_BYTES, d5
	bra.w field
nominal
	move.l values.StructRecord.Nominal(a3), d1
	move.l Header.ExportCount(a6), d2
	movea.l a6, a0
	adda.l Header.Exports(a6), a0
findSchema
	tst.l d2
	beq.w bad
	cmp.l Export.Id(a0), d1
	beq.w schema
	adda.w #EXPORT_BYTES, a0
	subq.l #1, d2
	bra.w findSchema
schema
	cmpi.l #values.STRUCT_TYPE, Export.Kind(a0)
	bne.w bad
	; The target row may occur later, so bound it before reading its nominal.
	move.l Export.Low(a0), d0
	andi.l #7, d0
	bne.w bad
	move.l Header.ArenaBytes(a6), d0
	subi.l #values.STRUCT_BASE_BYTES, d0
	bcs.w bad
	cmp.l Export.Low(a0), d0
	blo.w bad
	movea.l a6, a2
	adda.l Header.Arena(a6), a2
	adda.l Export.Low(a0), a2
	cmpi.l #values.STRUCT_TYPE, values.Record.Kind(a2)
	bne.w bad
	move.l values.StructRecord.Nominal(a2), d0
	cmp.l d1, d0
	bne.w bad
	lea values.STRUCT_BASE_BYTES(a3), a3
	moveq #values.STRUCT_FIELD_BYTES, d5
	cmpi.l #values.STRUCT_TYPE, d4
	beq.w field
	moveq #values.FIELD_BYTES, d5
field
	tst.l d7
	beq.w good
	move.l (a3), d1
	move.l Header.MemberCount(a6), d2
	movea.l a6, a0
	adda.l Header.Members(a6), a0
findMember
	tst.l d2
	beq.w bad
	cmp.l Member.Id(a0), d1
	beq.w nextField
	adda.w #MEMBER_BYTES, a0
	subq.l #1, d2
	bra.w findMember
nextField
	adda.l d5, a3
	subq.l #1, d7
	bra.w field
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; closure

	.pub
; A0=image,D0=exact bytes,D1=local ID. D0/CCR=OK,NOT_FOUND,INVALID;
; A1=borrowed serialized row on success, zero otherwise. Others preserved.
; Rows/names/value offsets remain valid until caller releases the image owner.
findExport	.block
	move.l d2, -(sp)
	moveq #0, d2
	bsr.w find
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; findExport
findMember	.block
	move.l d2, -(sp)
	moveq #1, d2
	bsr.w find
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; findMember

; A0=owned memory.Block. D0/CCR=status; others preserved. Invalidates views.
release	.block
	jsr memory.release
	clr.l memory.Block.Used(a0)
	moveq #OK, d0
	rts
	.bend  ; release
	.priv
find	.block
	movem.l d1-d5/a0/a2, -(sp)
	movea.l a0, a2
	bsr.w validate
	bne.w malformed
	move.l Header.ExportCount(a2), d3
	move.l Header.Exports(a2), d4
	moveq #EXPORT_BYTES, d5
	tst.l d2
	beq.w ready
	move.l Header.MemberCount(a2), d3
	move.l Header.Members(a2), d4
	moveq #MEMBER_BYTES, d5
ready
	movea.l a2, a1
	adda.l d4, a1
next
	tst.l d3
	beq.w missing
	cmp.l Export.Id(a1), d1
	beq.w found
	blo.w missing
	adda.l d5, a1
	subq.l #1, d3
	bra.w next
found
	moveq #OK, d0
	bra.w done
missing
	moveq #NOT_FOUND, d0
	bra.w clear
malformed
	moveq #INVALID, d0
clear
	suba.l a1, a1
done
	movem.l (sp)+, d1-d5/a0/a2
	tst.l d0
	rts
	.bend  ; find
