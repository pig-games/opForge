; Artifact descriptors over owned packed records; no source names are looked up.
; @opforge-owner: experimental.amigaos.binary_output_plan
	.module experimental.amigaos.binary_output_plan
	.cpu 68020
	.use experimental.amigaos.binary_sections as sections
	.pub
BIN = 1
HUNK = 2
PRG = 3
ITEM = 0
END = 1
INVALID = 2
Cursor	.struct
Records	.long ?
Bytes	.long ?
Offset	.long ?
	.endstruct
CURSOR_BYTES = Cursor.Offset+4
	.section code, kind=code
	.pub
; A0=Cursor. Advance over records and return one validated output descriptor.
; D0/CCR=ITEM/END/INVALID. ITEM: D1=format,D2=count,A1=u16 slots,A2=path,D3=path
; bytes. Views last until the caller releases packed records. D4-D7/A0/A3-A6
; preserved. Only runtime Cursor has pointers; persisted descriptors do not.
next	.block
	movem.l d4-d7/a3-a6, -(sp)
	movea.l a0, a6
	movea.l Cursor.Records(a6), a3
	move.l Cursor.Bytes(a6), d7
	move.l Cursor.Offset(a6), d6
record
	cmp.l d7, d6
	beq.w finished
	bhi.w bad
	movea.l a3, a4
	adda.l d6, a4
	moveq #0, d4
	move.b (a4), d4
	addq.w #1, d4
	cmpi.w #4, d4
	blo.w bad
	move.l d7, d0
	sub.l d6, d0
	cmp.l d0, d4
	bhi.w bad
	add.l d4, d6
	move.l d6, Cursor.Offset(a6)
	btst #4, 1(a4)
	beq.w record
	cmpi.w #5, d4
	blo.w bad
	cmpi.b #20, 4(a4)
	beq.w descriptor
	cmpi.b #21, 4(a4)
	bne.w record
descriptor
	cmpi.w #9, d4
	blo.w bad
	moveq #0, d2
	move.b 5(a4), d2
	beq.w bad
	cmpi.w #8, d2
	bhi.w bad
	move.l d2, d0
	add.l d0, d0
	addq.l #8, d0
	cmp.l d4, d0
	bhs.w bad
	lea 6(a4), a1
	movea.l a1, a5
	moveq #0, d5
	move.l d2, d0
slots
	moveq #0, d1
	move.w (a5)+, d1
	cmpi.w #8, d1
	bhs.w bad
	btst d1, d5
	bne.w bad
	bset d1, d5
	subq.w #1, d0
	bne.w slots
	moveq #0, d1
	move.b (a5)+, d1
	cmpi.b #20, 4(a4)
	bne.w flatKind
	cmpi.w #HUNK, d1
	bne.w bad
	bra.w path
flatKind
	cmpi.w #BIN, d1
	beq.w path
	cmpi.w #PRG, d1
	bne.w bad
path
	moveq #0, d3
	move.b (a5)+, d3
	beq.w bad
	move.l d2, d0
	add.l d0, d0
	add.l d3, d0
	addq.l #8, d0
	cmp.l d4, d0
	bne.w bad
	movea.l a5, a2
	move.l d3, d0
pathBytes
	move.b (a5)+, d5
	cmpi.b #32, d5
	blo.w bad
	subq.w #1, d0
	bne.w pathBytes
	moveq #ITEM, d0
	bra.w done
finished
	moveq #END, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
	.bend  ; next

; Resolve a bin/PRG selection against completed placed layout, never Hunk PCs.
; A0=sections.State,A1=u16 slot words,D0=count,D1=total flat buffer bytes.
; Returns D0/CCR=status,D1=buffer offset,D2=bytes,D3=load address. Preserves
; D4-D7/A0-A6. Only one/two placed contiguous sections exist in this subset.
range	.block
	movem.l d4-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d1, d7
	move.l d0, d6
	beq.w bad
	cmpi.w #2, d6
	bhi.w bad
	move.w sections.State.Mode(a6), d0
	beq.w bad
	cmpi.w #sections.HUNK_MODE, d0
	beq.w bad
	moveq #0, d4
	moveq #0, d5
	moveq #0, d3
	moveq #0, d2
slot
	moveq #0, d0
	move.w (a1)+, d0
	cmpi.w #1, d0
	bhi.w bad
	tst.w d0
	bne.w second
	lea sections.FIRST(a6), a0
	bra.w ready
second
	move.w sections.State.Mode(a6), d1
	cmpi.w #3, d1
	beq.w twoSlots
	cmpi.w #4, d1
	bne.w bad
twoSlots
	lea sections.SECOND(a6), a0
ready
	move.l sections.Slot.Base(a0), d1
	move.l sections.Slot.After(a0), d0
	cmp.l d1, d0
	blo.w bad
	tst.w d5
	bne.w continuation
	move.l d1, d3
	move.l d1, d4
	sub.l sections.FIRST_BASE(a6), d4
	bcs.w bad
	bra.w size
continuation
	move.l d3, d5
	add.l d2, d5
	bcs.w bad
	cmp.l d1, d5
	bne.w bad
size
	sub.l d1, d0
	add.l d0, d2
	bcs.w bad
	moveq #1, d5
	subq.w #1, d6
	bne.w slot
	move.l d4, d0
	add.l d2, d0
	bcs.w bad
	cmp.l d7, d0
	bhi.w bad
	move.l d4, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; range
	.endsection
	.endmodule
