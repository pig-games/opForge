; Copy a retained semantic value through caller-owned numeric identity maps.
; @opforge-owner: experimental.amigaos.binary_module_remap
	.module experimental.amigaos.binary_module_remap
	.cpu 68020
	.use experimental.amigaos.binary_module_interface as interface
	.use experimental.amigaos.binary_values as values
	.pub
Frame	.struct
Image	.long ?
Bytes	.long ?
Owner	.long ?
Resolve	.long ?
Context	.long ?
Export	.long ?  ; borrowed exact serialized export row
Low	.long ?
High	.long ?
Kind	.long ?
Provenance	.long ?
	.endstruct
FRAME_BYTES = Frame.Provenance+4
NOMINAL = 0
MEMBER = 1
SECTION = 2
OK = 0
INVALID = 1
	.section code, kind=code
	.pub
; A0=Frame with borrowed immutable validated interface, its exact Export row,
; and destination values.Owner. Scope.install validates the whole interface once;
; its closure validation remains valid throughout this copy and all callbacks.
; D0/CCR=status; other registers preserved. Outputs clear on any failure.
; Resolve: A0=Context,D0=namespace,D1=source ID -> D0=status,D1=destination
; ID; D2-D7/A1-A6 preserved. It must not grow or mutate the destination arena,
; mutate the source image/Frame, or invalidate either borrowed source pointer.
; Names are resolved by the caller once, before copying; private nominal IDs
; need numeric map slots only. Scalar section addresses retain their Low word;
; placement/fragment relocation belongs to the caller's layout owner.
; The copied descriptor owns all data. No source pointer survives. Failure may
; leave unused destination arena bytes; its existing owner remains releasable.
copy	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	bsr.w clear
	movea.l Frame.Owner(a5), a0
	move.l a0, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l Frame.Resolve(a5), d0
	beq.w bad
	btst #0, d0
	bne.w bad
	movea.l Frame.Image(a5), a0
	; Check the borrowed extent and exact row membership, not semantic closure.
	move.l a0, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l Frame.Bytes(a5), d2
	cmpi.l #interface.HEADER_BYTES, d2
	blo.w bad
	add.l d2, d0
	bcs.w bad
	cmp.l interface.Header.Bytes(a0), d2
	bne.w bad
	movea.l Frame.Export(a5), a1
	move.l a1, d3
	sub.l a0, d3
	bcs.w bad
	subi.l #interface.EXPORT_BYTES, d2
	bcs.w bad
	cmp.l d2, d3
	bhi.w bad
	sub.l interface.Header.Exports(a0), d3
	bcs.w bad
	move.l d3, d0
	moveq #interface.EXPORT_BYTES, d1
	divu.l d1, d0
	cmp.l interface.Header.ExportCount(a0), d0
	bhs.w bad
	mulu.l #interface.EXPORT_BYTES, d0
	cmp.l d3, d0
	bne.w bad
	move.l interface.Export.Kind(a1), d3
	cmpi.l #interface.FRAGMENT_ADDRESS, d3
	beq.w bad
	move.l d3, Frame.Kind(a5)
	move.l interface.Export.Low(a1), Frame.Low(a5)
	move.l interface.Export.High(a1), Frame.High(a5)
	move.l interface.Export.Provenance(a1), Frame.Provenance(a5)
	tst.l d3
	bne.w compound
	move.l Frame.Provenance(a5), d1
	beq.w good
	moveq #SECTION, d0
	bsr.w resolve
	bne.w bad
	cmpi.l #values.MAX_SECTION_ID, d1
	bhi.w bad
	move.l d1, Frame.Provenance(a5)
	bra.w good
compound
	movea.l Frame.Image(a5), a1
	move.l interface.Header.ArenaBytes(a1), d0
	adda.l interface.Header.Arena(a1), a1
	move.l Frame.Low(a5), d1
	movea.l Frame.Owner(a5), a0
	jsr values.copyRecord
	bne.w bad
	move.l d2, Frame.Low(a5)
	move.l d2, d1
	jsr values.view
	bne.w bad
	movea.l a5, a1
	bsr.w record
	bne.w bad

good
	moveq #OK, d0
	bra.w done
bad
	bsr.w clear
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; copy
; A0=destination Owner,D2=fresh validated copied offset,A1=resolver Frame.
; D0/CCR=status; other registers preserved. Rewrites numeric IDs/provenance
; in place without cloning. Caller owns partial mutation on mapping failure.
record	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a5
	move.l d2, d1
	jsr values.view
	bne.w bad
	movea.l a1, a4
	move.l values.Record.Count(a4), d7
	move.l values.Record.Kind(a4), d6
	cmpi.l #values.TYPED_LIST, d6
	blo.w good
	cmpi.l #values.STRUCT_TYPE, d6
	bhs.w mapNominal
	lea values.HEADER_BYTES(a4), a4
	cmpi.l #values.ADDRESS_RECORD, d6
	bne.w list
	bsr.w cell
	bne.w bad
	adda.w #values.CELL_BYTES, a4
	bra.w fields
mapNominal
	move.l values.StructRecord.Nominal(a4), d1
	moveq #NOMINAL, d0
	bsr.w resolve
	bne.w bad
	cmpi.l #65535, d1
	bhi.w bad
	move.l d1, values.StructRecord.Nominal(a4)
	lea values.STRUCT_BASE_BYTES(a4), a4
fields
	tst.l d7
	beq.w good
	move.l (a4), d1
	moveq #MEMBER, d0
	bsr.w resolve
	bne.w bad
	cmpi.l #65535, d1
	bhi.w bad
	move.l d1, (a4)
	cmpi.l #values.STRUCT_TYPE, d6
	beq.w schemaField
	addq.l #4, a4
	bsr.w cell
	bne.w bad
	adda.w #values.CELL_BYTES, a4
	bra.w nextField
schemaField
	adda.w #values.STRUCT_FIELD_BYTES, a4
nextField
	subq.l #1, d7
	bra.w fields
list
	tst.l d7
	beq.w good
	bsr.w cell
	bne.w bad
	adda.w #values.CELL_BYTES, a4
	subq.l #1, d7
	bra.w list
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; record

	.priv
; A4=validated scalar Cell,A5=Frame. D0/CCR=status; other registers kept.
cell	.block
	move.l d1, -(sp)
	move.l values.Cell.Provenance(a4), d1
	beq.w good
	moveq #SECTION, d0
	bsr.w resolve
	bne.w done
	cmpi.l #values.MAX_SECTION_ID, d1
	bhi.w bad
	move.l d1, values.Cell.Provenance(a4)
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; cell

; A5=Frame,D0=namespace,D1=source identity. D0=status,D1=mapped identity.
resolve	.block
	movem.l a0/a2, -(sp)
	movea.l Frame.Context(a5), a0
	movea.l Frame.Resolve(a5), a2
	jsr (a2)
	movem.l (sp)+, a0/a2
	tst.l d0
	rts
	.bend  ; resolve

; Clear publication only; destination ownership never transfers on failure.
clear	.block
	clr.l Frame.Low(a5)
	clr.l Frame.High(a5)
	clr.l Frame.Kind(a5)
	clr.l Frame.Provenance(a5)
	rts
	.bend  ; clear
	.endsection
	.endmodule
