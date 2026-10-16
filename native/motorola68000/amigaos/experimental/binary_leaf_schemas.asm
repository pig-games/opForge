; Pointer-free declaration-leaf schema validation and identity relocation.
; @opforge-owner: experimental.amigaos.binary_leaf_schemas
	.module experimental.amigaos.binary_leaf_schemas
	.cpu 68020
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_memory as memory
	.pub
OK = 0
MALFORMED = 1
	.section code, kind=code

; A0=external schema arena,D0=exact bytes,D1=local dictionary count.
; D0/CCR=status; other registers preserved. Read-only, allocation-free: every
; record must be a complete plain schema with bounded local nominal/member IDs,
; unique member keys, valid geometry and canonical zero alignment padding.
validate	.block
	movem.l d1-d7/a0-a4, -(sp)
	move.l d0, d7
	move.l d1, d5
	cmpi.l #65535, d5
	bhi.w bad
	cmpi.l #memory.LIMIT, d7
	bhi.w bad
	tst.l d7
	beq.w good
	move.l a0, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	add.l d7, d0
	bcs.w bad
record
	tst.l d7
	beq.w good
	cmpi.l #values.STRUCT_BASE_BYTES, d7
	blo.w bad
	cmpi.l #values.STRUCT_TYPE, values.Record.Kind(a0)
	bne.w bad
	movea.l a0, a1
	jsr values.descriptorBytes
	bne.w bad
	cmp.l d7, d2
	bhi.w bad
	move.l d2, d6
	move.l values.StructRecord.Nominal(a0), d0
	cmp.l d5, d0
	bhs.w bad
	move.l values.Record.Count(a0), d4
	lea values.STRUCT_BASE_BYTES(a0), a2
field
	tst.l d4
	beq.w padding
	move.l values.StructField.Member(a2), d0
	cmp.l d5, d0
	bhs.w bad
	lea values.STRUCT_BASE_BYTES(a0), a3
unique
	cmpa.l a2, a3
	beq.w geometry
	cmp.l values.StructField.Member(a3), d0
	beq.w bad
	adda.w #values.STRUCT_FIELD_BYTES, a3
	bra.w unique
geometry
	move.l values.StructField.Offset(a2), d0
	add.l values.StructField.Size(a2), d0
	bcs.w bad
	cmp.l values.StructRecord.Size(a0), d0
	bhi.w bad
	adda.w #values.STRUCT_FIELD_BYTES, a2
	subq.l #1, d4
	bra.w field
padding
	movea.l a0, a4
	adda.l d6, a4
padByte
	cmpa.l a4, a2
	beq.w next
	tst.b (a2)+
	bne.w bad
	bra.w padByte
next
	movea.l a4, a0
	sub.l d6, d7
	bra.w record
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; validate

; A0=writable private arena,D0=bytes,A1=local-to-current long ID map,
; D1=map entries. D0/CCR=status; other registers preserved. Validate the entire
; arena and map before writing anything. Map entries must fit source word IDs.
; Nominal and bare member identities both relocate; geometry never changes.
remap	.block
	movem.l d1-d7/a0-a5, -(sp)
	move.l d0, d7
	movea.l a1, a3
	bsr.w validate
	bne.w done
	tst.l d1
	beq.w record
	move.l a3, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l d1, d2
	lsl.l #2, d2
	add.l d2, d0
	bcs.w bad
	; The relocation map must not be overwritten by this private arena.
	cmp.l a0, d0
	bls.w separate
	move.l a0, d0
	add.l d7, d0
	cmp.l a3, d0
	bhi.w bad
separate
	movea.l a3, a2
	move.l d1, d2
checkMap
	tst.l d2
	beq.w checkAliases
	cmpi.l #65535, (a2)+
	bhi.w bad
	subq.l #1, d2
	bra.w checkMap
checkAliases
	movea.l a0, a5
	move.l d7, d5
checkRecord
	tst.l d5
	beq.w record
	movea.l a5, a1
	jsr values.descriptorBytes
	move.l d2, d6
	move.l values.Record.Count(a5), d4
	lea values.STRUCT_BASE_BYTES(a5), a2
checkField
	tst.l d4
	beq.w nextCheckedRecord
	move.l values.StructField.Member(a2), d0
	lsl.l #2, d0
	move.l 0(a3, d0.l), d3
	lea values.STRUCT_BASE_BYTES(a5), a4
checkEarlier
	cmpa.l a2, a4
	beq.w nextCheckedField
	move.l values.StructField.Member(a4), d0
	lsl.l #2, d0
	cmp.l 0(a3, d0.l), d3
	beq.w bad
	adda.w #values.STRUCT_FIELD_BYTES, a4
	bra.w checkEarlier
nextCheckedField
	adda.w #values.STRUCT_FIELD_BYTES, a2
	subq.l #1, d4
	bra.w checkField
nextCheckedRecord
	adda.l d6, a5
	sub.l d6, d5
	bra.w checkRecord
record
	tst.l d7
	beq.w good
	movea.l a0, a1
	jsr values.descriptorBytes
	move.l d2, d6
	move.l values.StructRecord.Nominal(a0), d0
	lsl.l #2, d0
	move.l 0(a3, d0.l), values.StructRecord.Nominal(a0)
	move.l values.Record.Count(a0), d4
	lea values.STRUCT_BASE_BYTES(a0), a2
field
	tst.l d4
	beq.w next
	move.l values.StructField.Member(a2), d0
	lsl.l #2, d0
	move.l 0(a3, d0.l), values.StructField.Member(a2)
	adda.w #values.STRUCT_FIELD_BYTES, a2
	subq.l #1, d4
	bra.w field
next
	adda.l d6, a0
	sub.l d6, d7
	bra.w record
good
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; remap

; A0=validated local schema arena,D0=bytes,D1=local seed offset,D2=destination
; seed base. D0/CCR=status,D1=relocated offset on success; others preserved.
; Reject interior offsets and overflow. The caller validates the arena before
; this lookup and appends it intact at an eight-byte-aligned destination base.
relocateOffset	.block
	movem.l d2-d5/a0-a1, -(sp)
	move.l d0, d5
	move.l d1, d4
	move.l d2, d3
	andi.l #7, d2
	bne.w bad
scan
	tst.l d5
	beq.w bad
	tst.l d4
	beq.w found
	cmpi.l #values.STRUCT_BASE_BYTES, d5
	blo.w bad
	movea.l a0, a1
	jsr values.descriptorBytes
	bne.w bad
	cmp.l d5, d2
	bhi.w bad
	sub.l d2, d4
	bcs.w bad
	sub.l d2, d5
	adda.l d2, a0
	bra.w scan
found
	add.l d3, d1
	bcs.w bad
	cmpi.l #memory.LIMIT, d1
	bhs.w bad
	moveq #OK, d0
	bra.w done
bad
	moveq #MALFORMED, d0
done
	movem.l (sp)+, d2-d5/a0-a1
	tst.l d0
	rts
	.bend  ; relocateOffset
	.endsection
	.endmodule
