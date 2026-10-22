; Name lookup over the stored OFMS full/leaf indexes. Indexes contain export
; ordinals, never pointers or duplicate strings. The producer validates their
; ordering/completeness before publication; every borrowed range is bounded here.
; A0=State,A1=spelling,D1=positive bytes<=255. D0/CCR=status;
; D1=entity on success. Other registers preserved. ASCII folded exact match;
; bare queries use the leaf index. Distinct matching entities are ambiguous.
lookupName	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d1, d6
	beq.w bad
	cmpi.l #values.NAME_LIMIT, d6
	bhi.w bad
	move.l a1, d0
	beq.w bad
	add.l d6, d0
	bcs.w bad
	moveq #0, d5
	movea.l a5, a2
	move.l d6, d0
input
	cmpi.b #'.', (a2)+
	bne.w inputNext
	moveq #1, d5
inputNext
	subq.l #1, d0
	bne.w input
	; Lower bound locates the entire equal-key range, including ambiguities.
	moveq #0, d3
	move.l State.Values+values.Header.IndexCount(a6), d7
search
	cmp.l d7, d3
	bhs.w range
	move.l d7, d4
	sub.l d3, d4
	lsr.l #1, d4
	add.l d3, d4
	bsr.w rowCompare
	bne.w bad
	tst.l d1
	bmi.w lower
	move.l d4, d7
	bra.w search
lower
	move.l d4, d3
	addq.l #1, d3
	bra.w search
range
	move.l d3, d4
	moveq #0, d3
	suba.l a4, a4
	move.l State.Values+values.Header.IndexCount(a6), d7
next
	cmp.l d7, d4
	bhs.w finished
	bsr.w rowCompare
	bne.w bad
	tst.l d1
	bne.w finished
	tst.l d3
	beq.w found
	cmpa.l d2, a4
	bne.w bad
	bra.w advance
found
	moveq #1, d3
	movea.l d2, a4
advance
	addq.l #1, d4
	bra.w next
finished
	tst.l d3
	beq.w missing
	move.l a4, d1
	moveq #OK, d0
	bra.w done
missing
	moveq #NOT_FOUND, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts

; A6=reader,A5=query,D6=query bytes,D5=full(1)/leaf(0),D4=index position.
; D0/CCR=status,D1=-1/0/+1 comparing indexed name to query,D2=entity.
; Others preserved. Copy all row fields before requesting another reader lease.
rowCompare
	movem.l d3-d7/a0/a2-a6, -(sp)
	move.l d4, d2
	lsl.l #2, d2
	tst.l d5
	bne.w fullIndex
	move.l State.Values+values.Header.IndexCount(a6), d0
	lsl.l #2, d0
	add.l d0, d2
fullIndex
	add.l State.Values+values.Header.Index(a6), d2
	moveq #4, d3
	movea.l a6, a0
	bsr.w valueRead
	bne.w malformed
	move.l (a1), d2
	cmp.l State.Values+values.Header.ExportCount(a6), d2
	bhs.w malformed
	mulu.l #values.EXPORT_BYTES, d2
	add.l State.Values+values.Header.Exports(a6), d2
	moveq #values.EXPORT_BYTES, d3
	bsr.w valueRead
	bne.w malformed
	move.l values.Export.Id(a1), d7
	move.l values.Export.NameBytes(a1), d3
	beq.w malformed
	cmpi.l #values.NAME_LIMIT, d3
	bhi.w malformed
	move.l values.Export.Name(a1), d2
	cmp.l State.Values+values.Header.Names(a6), d2
	blo.w malformed
	move.l d2, d0
	add.l d3, d0
	bcs.w malformed
	move.l State.Values+values.Header.Names(a6), d1
	add.l State.Values+values.Header.NameBytes(a6), d1
	cmp.l d1, d0
	bhi.w malformed
	bsr.w valueRead
	bne.w malformed
	movea.l a1, a2
	tst.l d5
	bne.w compare
	move.l d3, d0
	movea.l a1, a3
leaf
	cmpi.b #'.', (a3)+
	bne.w leafNext
	movea.l a3, a2
	move.l d0, d3
	subq.l #1, d3
leafNext
	subq.l #1, d0
	bne.w leaf
compare
	movea.l a5, a3
	move.l d3, d4
	cmp.l d6, d4
	bls.w character
	move.l d6, d4
character
	tst.l d4
	beq.w lengths
	moveq #0, d0
	move.b (a2)+, d0
	bsr.w folded
	move.l d0, d2
	moveq #0, d0
	move.b (a3)+, d0
	bsr.w folded
	cmp.b d0, d2
	blo.w less
	bhi.w greater
	subq.l #1, d4
	bra.w character
lengths
	cmp.l d6, d3
	blo.w less
	bhi.w greater
	moveq #0, d1
	bra.w compared
less
	moveq #-1, d1
	bra.w compared
greater
	moveq #1, d1
compared
	move.l d7, d2
	moveq #OK, d0
	bra.w compareDone
malformed
	moveq #INVALID, d0
compareDone
	movem.l (sp)+, d3-d7/a0/a2-a6
	tst.l d0
	rts
folded
	cmpi.b #'A', d0
	blo.w unchanged
	cmpi.b #'Z', d0
	bhi.w unchanged
	addi.b #'a'-'A', d0
unchanged
	rts
	.bend  ; lookupName
