; Immutable typed cells and address records in the values owner's arena.
; @opforge-owner: experimental.amigaos.binary_record_values
	.module experimental.amigaos.binary_record_values
	.cpu 68020
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_memory as memory
	.pub
TYPED_LIST = values.TYPED_LIST
ADDRESS_RECORD = values.ADDRESS_RECORD
CELL_BYTES = values.CELL_BYTES
FIELD_BYTES = values.FIELD_BYTES
	.priv
; Stack layout of the shared append prologue's MOVEM register save.
AppendSaved	.struct
SourceBytes	.long ?
Scratch3	.long ?
Count	.long ?
BaseBytes	.long ?
Stride	.long ?
Kind	.long ?
Owner	.long ?
DefaultCell	.long ?
Fields	.long ?
Source	.long ?
OwnerScratch	.long ?
.endstruct
	.pub
	.section code, kind=code
; Construction API: A0=Owner,D0=count. D0/CCR=status,D2=offset.
; Others preserved. Initializes every cell to absolute scalar0; publish after set.
createList	.block
	movem.l d1/d3-d5/a0-a1, -(sp)
	move.l d0, d4
	cmpi.l #(memory.LIMIT-values.HEADER_BYTES)/CELL_BYTES, d4
	bhi.w noSpace
	lea values.Owner.Arena(a0), a1
	jsr values.block
	bne.w done
	move.l memory.Block.Used(a1), d2
	move.l d2, d0
	andi.l #7, d0
	bne.w invalid
	move.l d4, d5
	lsl.l #4, d5
	addi.l #values.HEADER_BYTES, d5
	add.l d2, d5
	bcs.w noSpace
	cmpi.l #memory.LIMIT, d5
	bhi.w noSpace
	movea.l a1, a0
	move.l d5, d0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.l #TYPED_LIST, (a1)+
	move.l d4, (a1)+
	lsl.l #2, d4
	tst.l d4
	beq.w commit
clear
	clr.l (a1)+
	subq.l #1, d4
	bne.w clear
commit
	move.l d5, memory.Block.Used(a0)
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
	bra.w done
noSpace
	moveq #values.STORAGE, d0
done
	movem.l (sp)+, d1/d3-d5/a0-a1
	tst.l d0
	rts
	.bend  ; createList
; Construction API: A0=Owner,D0=listoffset,D1=index,A1=Cell.
; D0/CCR=status; others preserved. Never reserve/relocate; complete before publish.
set	.block
	movem.l d1-d4/a1-a2, -(sp)
	move.l d0, d4
	move.l d1, d2
	movea.l a1, a2
	move.l a1, d0
	beq.w invalid
	btst #0, d0
	bne.w invalid
	addi.l #CELL_BYTES, d0
	bcs.w invalid
	bsr.w validateCell
	bne.w done
	move.l d4, d1
	jsr values.view
	bne.w done
	cmpi.l #TYPED_LIST, values.Record.Kind(a1)
	bne.w invalid
	cmp.l values.Record.Count(a1), d2
	bhs.w bounds
	lsl.l #4, d2
	lea values.HEADER_BYTES(a1, d2.l), a1
	move.l values.Cell.Low(a2), d1
	move.l values.Cell.High(a2), d2
	move.l values.Cell.Kind(a2), d3
	move.l values.Cell.Provenance(a2), d4
	move.l d1, values.Cell.Low(a1)
	move.l d2, values.Cell.High(a1)
	move.l d3, values.Cell.Kind(a1)
	move.l d4, values.Cell.Provenance(a1)
	moveq #values.OK, d0
	bra.w done
bounds
	moveq #values.BOUNDS, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d4/a1-a2
	tst.l d0
	rts
	.bend  ; set
; A0=Owner,A1=external Cell array,D0=count,D1=sourcebytes.
; D0/CCR=status,D2=offset; others preserved. Source must not alias Arena.
appendList	.block
	movem.l d1/d3-d7/a0-a4, -(sp)
	moveq #TYPED_LIST, d7
	moveq #CELL_BYTES, d6
	moveq #values.HEADER_BYTES, d5
	bra.w append
	.bend  ; appendList
; A0=Owner,A1=external default Cell,A2=external Field array,D0=count,D1=fieldbytes.
; D0/CCR=status,D2=offset; others preserved. Sources must not alias Arena.
appendRecord	.block
	movem.l d1/d3-d7/a0-a4, -(sp)
	moveq #ADDRESS_RECORD, d7
	moveq #FIELD_BYTES, d6
	moveq #values.RECORD_BASE_BYTES, d5
	bra.w append
	.bend  ; appendRecord
; A0=Owner,D0=offset,D1=index. D0/CCR=status,D2-D5=Cell; others preserved.
get	.block
	movem.l d1/d6/a1, -(sp)
	move.l d1, d6
	move.l d0, d1
	jsr values.view
	bne.w done
	cmpi.l #TYPED_LIST, values.Record.Kind(a1)
	bne.w invalid
	cmp.l values.Record.Count(a1), d6
	bhs.w bounds
	lsl.l #4, d6
	lea values.HEADER_BYTES(a1, d6.l), a1
	bsr.w readCell
	bra.w done
invalid
	moveq #values.MALFORMED, d0
	bra.w done
bounds
	moveq #values.BOUNDS, d0
done
	movem.l (sp)+, d1/d6/a1
	tst.l d0
	rts
	.bend  ; get
; A0=Owner,D0=offset,D1=member ID. D0/CCR=status,D2-D5=Cell; others preserved.
member	.block
	movem.l d1/d6-d7/a1, -(sp)
	move.l d1, d7
	move.l d0, d1
	jsr values.view
	bne.w done
	cmpi.l #ADDRESS_RECORD, values.Record.Kind(a1)
	bne.w invalid
	move.l values.Record.Count(a1), d6
	lea values.RECORD_BASE_BYTES(a1), a1
scan
	tst.l d6
	beq.w bounds
	cmp.l values.Field.Member(a1), d7
	beq.w found
	adda.l #FIELD_BYTES, a1
	subq.l #1, d6
	bra.w scan
found
	lea values.Field.Value(a1), a1
	bsr.w readCell
	bra.w done
invalid
	moveq #values.MALFORMED, d0
	bra.w done
bounds
	moveq #values.BOUNDS, d0
done
	movem.l (sp)+, d1/d6-d7/a1
	tst.l d0
	rts
	.bend  ; member
; A0=Owner,D0=offset. D0/CCR=status,D2-D5=default Cell; others preserved.
project	.block
	movem.l d1/a1, -(sp)
	move.l d0, d1
	jsr values.view
	bne.w done
	cmpi.l #ADDRESS_RECORD, values.Record.Kind(a1)
	bne.w invalid
	lea values.HEADER_BYTES(a1), a1
	bsr.w readCell
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; project
; A0=Owner,A1=Cell. D0/CCR=status; all other registers preserved.
; Compound cells name an existing descriptor in this Owner, with no provenance.
validateCell	.block
	movem.l d1-d3/a1, -(sp)
	cmpi.l #ADDRESS_RECORD, values.Cell.Kind(a1)
	bhi.w invalid
	move.l values.Cell.Provenance(a1), d0
	cmpi.l #-1, d0
	beq.w provenanceReady
	cmpi.l #8, d0
	bhi.w invalid
provenanceReady
	tst.l values.Cell.Kind(a1)
	beq.w good
	tst.l d0
	bne.w invalid
	tst.l values.Cell.High(a1)
	bne.w invalid
	move.l values.Cell.Kind(a1), d2
	move.l values.Cell.Low(a1), d1
	jsr values.view
	bne.w done
	cmp.l values.Record.Kind(a1), d2
	bne.w invalid
good
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d3/a1
	tst.l d0
	rts
	.bend  ; validateCell
; A0=Owner,D0=descriptor offset. D0/CCR=status; others preserved.
; Validates every cell without recursively walking compound children.
validate	.block
	movem.l d1-d3/d6-d7/a1-a2, -(sp)
	move.l d0, d1
	jsr values.view
	bne.w done
	move.l values.Record.Kind(a1), d7
	move.l values.Record.Count(a1), d6
	lea values.HEADER_BYTES(a1), a1
	cmpi.l #TYPED_LIST, d7
	beq.w cells
	cmpi.l #ADDRESS_RECORD, d7
	bne.w invalid
	bsr.w validateCell
	bne.w done
	adda.l #CELL_BYTES, a1
cells
	tst.l d6
	beq.w good
	cmpi.l #ADDRESS_RECORD, d7
	bne.w ready
	addq.l #4, a1
ready
	bsr.w validateCell
	bne.w done
	adda.l #CELL_BYTES, a1
	subq.l #1, d6
	bra.w cells
good
	moveq #values.OK, d0
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1-d3/d6-d7/a1-a2
	tst.l d0
	rts
	.bend  ; validate
; A0=Owner,D0=typed-list offset. D0/CCR=status,D2=count; others preserved.
length	.block
	movem.l d1/d3/a1, -(sp)
	move.l d0, d1
	jsr values.view
	bne.w done
	cmpi.l #TYPED_LIST, values.Record.Kind(a1)
	bne.w invalid
	move.l values.Record.Count(a1), d2
	bra.w done
invalid
	moveq #values.MALFORMED, d0
done
	movem.l (sp)+, d1/d3/a1
	tst.l d0
	rts
	.bend  ; length
	.priv
readCell	.block
	bsr.w validateCell
	bne.w done
	move.l values.Cell.Low(a1), d2
	move.l values.Cell.High(a1), d3
	move.l values.Cell.Kind(a1), d4
	move.l values.Cell.Provenance(a1), d5
done
	tst.l d0
	rts
	.bend  ; readCell
; Shared append engine. Saved public registers are restored on every return.
append	.block
	move.l d0, d4
	cmpi.l #TYPED_LIST, d7
	bne.w recordLimit
	cmpi.l #(memory.LIMIT-values.HEADER_BYTES)/CELL_BYTES, d4
	bhi.w noSpace
	bra.w sizeReady
recordLimit
	cmpi.l #(memory.LIMIT-values.RECORD_BASE_BYTES)/FIELD_BYTES, d4
	bhi.w noSpace
sizeReady
	move.l d4, d3
	mulu.l d6, d3
	cmp.l d1, d3
	bhi.w invalid
	movea.l a0, a4
	movea.l a1, a3
	cmpi.l #ADDRESS_RECORD, d7
	bne.w listSource
	move.l a1, d0
	beq.w invalid
	btst #0, d0
	bne.w invalid
	addi.l #CELL_BYTES, d0
	bcs.w invalid
	bsr.w validateCell
	bne.w done
	movea.l a2, a3
listSource
	tst.l d3
	beq.w checked
	move.l a3, d0
	beq.w invalid
	btst #0, d0
	bne.w invalid
	add.l d3, d0
	bcs.w invalid
	movea.l a3, a1
	move.l d4, d2
validate
	cmpi.l #ADDRESS_RECORD, d7
	bne.w cellReady
	addq.l #4, a1
cellReady
	bsr.w validateCell
	bne.w done
	adda.l #CELL_BYTES, a1
	subq.l #1, d2
	bne.w validate
checked
	lea values.Owner.Arena(a4), a1
	move.l d3, -(sp)
	jsr values.block
	move.l (sp)+, d3
	tst.l d0
	bne.w done
	; Reject external sources intersecting the arena before reserve can relocate it.
	move.l memory.Block.Pointer(a1), d0
	beq.w separate
	move.l d0, d1
	add.l memory.Block.Capacity(a1), d1
	move.l a3, d2
	cmp.l d1, d2
	bhs.w fieldsSeparate
	move.l d4, d2
	mulu.l d6, d2
	move.l a3, d1
	add.l d1, d2
	cmp.l d0, d2
	bhi.w invalid
fieldsSeparate
	move.l d0, d1
	add.l memory.Block.Capacity(a1), d1
	cmpi.l #ADDRESS_RECORD, d7
	bne.w separate
	move.l AppendSaved.DefaultCell(sp), d2
	cmp.l d1, d2
	bhs.w separate
	addi.l #CELL_BYTES, d2
	cmp.l d0, d2
	bhi.w invalid
separate
	move.l memory.Block.Used(a1), d2
	move.l d2, d0
	andi.l #7, d0
	bne.w invalid
	add.l d5, d3
	addq.l #7, d3
	andi.l #$fffffff8, d3
	add.l d2, d3
	bcs.w noSpace
	cmpi.l #memory.LIMIT, d3
	bhi.w noSpace
	movea.l a1, a0
	move.l d3, d0
	jsr memory.reserve
	tst.l d0
	bne.w noSpace
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.l d7, (a1)+
	move.l d4, (a1)+
	cmpi.l #ADDRESS_RECORD, d7
	bne.w payload
	movea.l AppendSaved.DefaultCell(sp), a2
	move.l (a2)+, (a1)+
	move.l (a2)+, (a1)+
	move.l (a2)+, (a1)+
	move.l (a2)+, (a1)+
payload
	mulu.l d6, d4
	lsr.l #2, d4
	tst.l d4
	beq.w copied
copy
	move.l (a3)+, (a1)+
	subq.l #1, d4
	bne.w copy
copied
	move.l a1, d0
	sub.l memory.Block.Pointer(a0), d0
	cmp.l d3, d0
	beq.w commit
	clr.l (a1)
commit
	move.l d3, memory.Block.Used(a0)
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
	.endsection
	.endmodule
