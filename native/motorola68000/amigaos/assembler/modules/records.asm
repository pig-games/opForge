; Internal prepared controls for saved-image fragments and outgoing identities.
; @opforge-owner: opforge.assembler.modules.records
; No source-language syntax, target semantics, payload bytes or process pointers.
	.module opforge.assembler.modules.records
	.cpu 68020
	.use opforge.assembler.source.records as source
	.use opforge.platform.memory as memory
	.pub
OP_FRAGMENT = 26
OP_REFERENCES = 27
OP_OWNERS = 28
Fragment	.struct
Header	.res source.RECORD_HEADER_BYTES
Opcode	.byte ?
Module	.word ?
Id	.word ?
	.endstruct
FRAGMENT_BYTES = Fragment.Id+2
ReferenceList	.struct
Header	.res source.RECORD_HEADER_BYTES
Opcode	.byte ?
Count	.byte ?
	.endstruct
REFERENCE_HEADER_BYTES = ReferenceList.Count+1
REFERENCE_LIMIT = (source.MAX_LINE-REFERENCE_HEADER_BYTES)/2
OK = 0
INVALID = 1
	.section code, kind=code
; A0=owned destination Block,D1=module u16,D2=fragment1..65535,D3=line u16.
; D0/CCR=status; other registers preserved. Appends one nine-byte record.
; Failure preserves Used and existing bytes; owned storage may grow.
appendFragment	.block
	movem.l d1-d5/a0-a2, -(sp)
	cmpi.l #65535, d1
	bhi.w bad
	tst.l d2
	beq.w bad
	cmpi.l #65535, d2
	bhi.w bad
	cmpi.l #65535, d3
	bhi.w bad
	move.l memory.Block.Used(a0), d4
	move.l d4, d0
	addi.l #FRAGMENT_BYTES, d0
	bcs.w bad
	bsr.w reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l d4, a2
	move.b #FRAGMENT_BYTES-1, source.RecordHeader.Length(a2)
	move.b #source.FLAG_LAYOUT, source.RecordHeader.Flags(a2)
	move.w d3, source.RecordHeader.Line(a2)
	move.b #OP_FRAGMENT, Fragment.Opcode(a2)
	move.w d1, Fragment.Module(a2)
	move.w d2, Fragment.Id(a2)
	addi.l #FRAGMENT_BYTES, memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d5/a0-a2
	tst.l d0
	rts
	.bend  ; appendFragment

; A0=destination Block,A1=nonoverlapping readable u32 global IDs,D1=count,
; D3=line u16. Count0..REFERENCE_LIMIT; every global ID must fit unsigned16.
; D0/CCR=status; other registers preserved. Validate all IDs before reserve or
; output writes. Appends one bounded record; input must not borrow destination.
appendReferences	.block
	moveq #OP_REFERENCES, d0
	bra.w appendIdentityList
	.bend  ; appendReferences

; Same bounded list framing as appendReferences; declares reachability ownership
; without publishing address labels or introducing execution-time statements.
appendOwners	.block
	moveq #OP_OWNERS, d0
	bra.w appendIdentityList
	.bend  ; appendOwners
	.priv
; D0=owned numeric-list opcode; remaining inputs are the public list ABI.
appendIdentityList	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.l d0, d7
	cmpi.l #REFERENCE_LIMIT, d1
	bhi.w bad
	cmpi.l #65535, d3
	bhi.w bad
	move.l d1, d0
	lsl.l #2, d0
	move.l a1, d2
	tst.l d1
	beq.w extentReady
	tst.l d2
	beq.w bad
extentReady
	add.l d0, d2
	bcs.w bad
	movea.l a1, a2
	move.l d1, d5
check
	tst.l d5
	beq.w checked
	cmpi.l #65535, (a2)+
	bhi.w bad
	subq.l #1, d5
	bra.w check
checked
	move.l d1, d6
	add.l d6, d6
	addi.l #REFERENCE_HEADER_BYTES, d6
	move.l memory.Block.Used(a0), d4
	move.l d4, d0
	add.l d6, d0
	bcs.w bad
	bsr.w reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a2
	adda.l d4, a2
	move.l d6, d0
	subq.l #1, d0
	move.b d0, source.RecordHeader.Length(a2)
	move.b #source.FLAG_LAYOUT, source.RecordHeader.Flags(a2)
	move.w d3, source.RecordHeader.Line(a2)
	move.b d7, ReferenceList.Opcode(a2)
	move.b d1, ReferenceList.Count(a2)
	adda.w #REFERENCE_HEADER_BYTES, a2
	move.l d1, d5
copy
	tst.l d5
	beq.w good
	move.l (a1)+, d0
	move.w d0, (a2)+
	subq.l #1, d5
	bra.w copy
good
	add.l d6, memory.Block.Used(a0)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; appendIdentityList
	.pub

; A0=readable record,D0=exact bytes. Success D1=module,D2=fragment.
; D0/CCR=status; remaining registers preserved. Failure preserves D1/D2.
fragmentView	.block
	movem.l d3-d4, -(sp)
	cmpi.l #FRAGMENT_BYTES, d0
	bne.w bad
	bsr.w header
	bne.w bad
	cmpi.b #OP_FRAGMENT, Fragment.Opcode(a0)
	bne.w bad
	moveq #0, d4
	move.w Fragment.Id(a0), d4
	beq.w bad
	moveq #0, d1
	move.w Fragment.Module(a0), d1
	move.l d4, d2
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d3-d4
	tst.l d0
	rts
	.bend  ; fragmentView

; A0=readable record,D0=exact bytes. Success D1=count,A1=packed BE16 IDs.
; D0/CCR=status; remaining registers preserved. Failure preserves D1/A1.
; This borrowed view expires when the record moves or its lease is released.
referencesView	.block
	move.l d2, -(sp)
	moveq #OP_REFERENCES, d2
	bsr.w identityListView
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; referencesView

; Same list result ABI; only OP_OWNERS is accepted.
ownersView	.block
	move.l d2, -(sp)
	moveq #OP_OWNERS, d2
	bsr.w identityListView
	move.l (sp)+, d2
	tst.l d0
	rts
	.bend  ; ownersView
	.priv
; D2=expected owned list opcode. Public wrappers preserve the caller's D2.
identityListView	.block
	movem.l d2-d4, -(sp)
	cmpi.l #REFERENCE_HEADER_BYTES, d0
	blo.w bad
	cmpi.l #source.MAX_LINE, d0
	bhi.w bad
	move.l d0, d4
	bsr.w header
	bne.w bad
	cmp.b ReferenceList.Opcode(a0), d2
	bne.w bad
	moveq #0, d2
	move.b ReferenceList.Count(a0), d2
	cmpi.l #REFERENCE_LIMIT, d2
	bhi.w bad
	move.l d2, d3
	add.l d3, d3
	addi.l #REFERENCE_HEADER_BYTES, d3
	cmp.l d4, d3
	bne.w bad
	move.l d2, d1
	lea REFERENCE_HEADER_BYTES(a0), a1
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d4
	tst.l d0
	rts
	.bend  ; identityListView
; A0=Block,D0=required bytes. Preserve all arguments except D0/CCR=status.
reserve	.block
	move.l d1, -(sp)
	move.l #source.RECORD_LIMIT, d1
	jsr memory.reserveBounded
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; reserve

; A0=readable record,D0=exact checked bytes (at least common header/opcode).
; D0/CCR=status,D3 clobbered. No pointer wrapping or oversized packed framing.
header	.block
	move.l a0, d3
	beq.w bad
	add.l d0, d3
	bcs.w bad
	moveq #0, d3
	move.b source.RecordHeader.Length(a0), d3
	addq.l #1, d3
	cmp.l d0, d3
	bne.w bad
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a0)
	beq.w bad
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; header
	.endsection
	.endmodule
