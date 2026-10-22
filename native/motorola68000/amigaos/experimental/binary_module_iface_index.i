; Public spelling indexes: full keys followed by leaf keys, ordinal tie-breaks.
	.priv
; A0=completed destination image. Populate and heapsort without scratch storage.
; D0/CCR=OK; other registers preserved. Names and export rows are immutable.
indexBuild	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a6, a4
	adda.l Header.Index(a6), a4
	movea.l a6, a0
	adda.l Header.Exports(a6), a0
	move.l Header.IndexCount(a6), d6
	move.l d6, d0
	lsl.l #2, d0
	movea.l a4, a1
	adda.l d0, a1
	moveq #0, d1
	move.l Header.ExportCount(a6), d7
fill
	tst.l d7
	beq.w sort
	tst.l Export.NameBytes(a0)
	beq.w next
	move.l d1, (a4)+
	move.l d1, (a1)+
next
	addq.l #1, d1
	adda.w #EXPORT_BYTES, a0
	subq.l #1, d7
	bra.w fill
sort
	movea.l a6, a4
	adda.l Header.Index(a6), a4
	moveq #0, d5
	bsr.w indexSort
	move.l d6, d0
	lsl.l #2, d0
	adda.l d0, a4
	moveq #1, d5
	bsr.w indexSort
	moveq #OK, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; indexBuild

; A6=image,A4=array,D6=count,D5=leaf flag. Other registers preserved.
indexSort	.block
	movem.l d0-d4/d7/a0-a1, -(sp)
	cmpi.l #2, d6
	blo.w done
	move.l d6, d4
	move.l d6, d7
	lsr.l #1, d7
heap
	subq.l #1, d7
	move.l d7, d3
	bsr.w indexSift
	tst.l d7
	bne.w heap
extract
	subq.l #1, d4
	move.l d4, d0
	lsl.l #2, d0
	movea.l a4, a0
	adda.l d0, a0
	move.l (a4), d0
	move.l (a0), (a4)
	move.l d0, (a0)
	moveq #0, d3
	bsr.w indexSift
	cmpi.l #1, d4
	bhi.w extract
done
	movem.l (sp)+, d0-d4/d7/a0-a1
	rts
	.bend  ; indexSort

; Sift root D3 in max heap [0,D4); A4/D5/A6 as indexSort. Registers preserved.
indexSift	.block
	movem.l d0-d3/d7/a0-a1, -(sp)
next
	move.l d3, d7
	add.l d7, d7
	addq.l #1, d7
	cmp.l d4, d7
	bhs.w done
	move.l d7, d0
	addq.l #1, d0
	cmp.l d4, d0
	bhs.w child
	lsl.l #2, d0
	movea.l a4, a1
	adda.l d0, a1
	move.l d7, d0
	lsl.l #2, d0
	movea.l a4, a0
	adda.l d0, a0
	move.l (a0), d1
	move.l (a1), d2
	bsr.w indexCompare
	tst.l d0
	bpl.w child
	addq.l #1, d7
child
	move.l d3, d0
	lsl.l #2, d0
	movea.l a4, a0
	adda.l d0, a0
	move.l d7, d0
	lsl.l #2, d0
	movea.l a4, a1
	adda.l d0, a1
	move.l (a0), d1
	move.l (a1), d2
	bsr.w indexCompare
	tst.l d0
	bpl.w done
	move.l d2, (a0)
	move.l d1, (a1)
	move.l d7, d3
	bra.w next
done
	movem.l (sp)+, d0-d3/d7/a0-a1
	rts
	.bend  ; indexSift

; A6=image,D1/D2=valid public ordinals,D5=leaf flag. D0 signed comparison;
; others preserved. Fold ASCII A..Z only; equal keys use unsigned ordinals.
indexCompare	.block
	movem.l d1-d4/d6-d7/a0-a3, -(sp)
	move.l d1, d0
	mulu.l #EXPORT_BYTES, d0
	movea.l a6, a0
	adda.l Header.Exports(a6), a0
	adda.l d0, a0
	move.l Export.NameBytes(a0), d3
	move.l Export.Name(a0), d0
	movea.l a6, a0
	adda.l d0, a0
	move.l d2, d0
	mulu.l #EXPORT_BYTES, d0
	movea.l a6, a1
	adda.l Header.Exports(a6), a1
	adda.l d0, a1
	move.l Export.NameBytes(a1), d4
	move.l Export.Name(a1), d0
	movea.l a6, a1
	adda.l d0, a1
	tst.l d5
	beq.w compare
	movea.l a0, a2
	move.l d3, d6
leftLeaf
	cmpi.b #'.', (a2)+
	bne.w leftNext
	movea.l a2, a0
	move.l d6, d3
	subq.l #1, d3
leftNext
	subq.l #1, d6
	bne.w leftLeaf
	movea.l a1, a2
	move.l d4, d6
rightLeaf
	cmpi.b #'.', (a2)+
	bne.w rightNext
	movea.l a2, a1
	move.l d6, d4
	subq.l #1, d4
rightNext
	subq.l #1, d6
	bne.w rightLeaf
compare
	tst.l d3
	beq.w leftEnd
	tst.l d4
	beq.w greater
	moveq #0, d0
	move.b (a0)+, d0
	bsr.w indexFold
	move.l d0, d7
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w indexFold
	cmp.l d0, d7
	blo.w less
	bhi.w greater
	subq.l #1, d3
	subq.l #1, d4
	bra.w compare
leftEnd
	tst.l d4
	bne.w less
	cmp.l d2, d1
	blo.w less
	bhi.w greater
	moveq #0, d0
	bra.w done
less
	moveq #-1, d0
	bra.w done
greater
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/d6-d7/a0-a3
	tst.l d0
	rts
	.bend  ; indexCompare

; D0=unsigned byte. Return its ASCII fold, no other registers changed.
indexFold	.block
	cmpi.b #'A', d0
	blo.w done
	cmpi.b #'Z', d0
	bhi.w done
	addi.b #'a'-'A', d0
done
	rts
	.bend  ; indexFold

; A0=image with validated rows/names/regions. D0/CCR=OK/INVALID; others kept.
; Each array must contain exactly the public rows in strict key/ordinal order.
; The comparator is a total order on ordinals: strict sortedness precludes any
; repeated ordinal, including nonadjacent duplicates. Public count then proves
; completeness without allocating a coverage map or quadratic scans.
indexValidate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l Header.ExportCount(a6), d7
	movea.l a6, a0
	adda.l Header.Exports(a6), a0
	moveq #0, d6
count
	tst.l d7
	beq.w counted
	tst.l Export.NameBytes(a0)
	beq.w next
	addq.l #1, d6
next
	adda.w #EXPORT_BYTES, a0
	subq.l #1, d7
	bra.w count
counted
	cmp.l Header.IndexCount(a6), d6
	bne.w bad
	movea.l a6, a4
	adda.l Header.Index(a6), a4
	moveq #0, d5
array
	move.l d6, d7
	moveq #0, d3
entry
	tst.l d7
	beq.w nextArray
	move.l (a4)+, d2
	cmp.l Header.ExportCount(a6), d2
	bhs.w bad
	move.l d2, d0
	mulu.l #EXPORT_BYTES, d0
	movea.l a6, a0
	adda.l Header.Exports(a6), a0
	adda.l d0, a0
	tst.l Export.NameBytes(a0)
	beq.w bad
	tst.l d3
	beq.w first
	bsr.w indexCompare
	tst.l d0
	bpl.w bad
first
	move.l d2, d1
	moveq #1, d3
	subq.l #1, d7
	bra.w entry
nextArray
	tst.l d5
	bne.w good
	moveq #1, d5
	bra.w array
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; indexValidate
