; Numeric region and placement lowering within binary_section_prepare.
; @opforge-owner: experimental.amigaos.binary_section_prepare

; A2/A3=bounded packed operands,A4=section state,A5=record,A6=scope.
; Regions retain only an index, inclusive bounds and alignment after preparation.
; D0/CCR=status; line owns caller preservation.
prepareRegion	.block
	tst.w State.Active(a4)
	bne.w bad
	bsr.w name
	bne.w bad
	move.w d1, d6
	bsr.w regionSlot
	bmi.w bad
	move.w d0, d7
	cmpa.l a3, a2
	bhs.w bad
	cmpi.b #4, (a2)+
	bne.w bad
	bsr.w literal
	bne.w bad
	move.l d1, -(sp)
	cmpa.l a3, a2
	bhs.w popBad
	cmpi.b #4, (a2)+
	bne.w popBad
	bsr.w literal
	bne.w popBad
	move.l d1, -(sp)
	bsr.w optionalAlignment
	bne.w pairBad
	lea 14(a5), a0
	bsr.w writeLong
	move.l (sp)+, d1
	lea 9(a5), a0
	bsr.w writeLong
	move.l (sp)+, d1
	lea 5(a5), a0
	bsr.w writeLong
	move.b d7, 13(a5)
	moveq #22, d5
	tst.w d7
	bne.w second
	move.w d6, State.Region(a4)
	ori.w #4, State.Seen(a4)
	moveq #4, d5
	bra.w ready
second
	cmpi.w #1, d7
	bne.w ready
	move.w d6, State.SecondRegion(a4)
	ori.w #32, State.Seen(a4)
	moveq #8, d5
ready
	move.b #17, (a5)
	move.b #source.FLAG_LAYOUT, 1(a5)
	move.b d5, 4(a5)
	moveq #0, d0
	rts
pairBad
	addq.l #4, sp
popBad
	addq.l #4, sp
bad
	moveq #1, d0
	rts
	.bend  ; prepareRegion

; Placement names reserve numeric slots; declaration and geometry validation
; occur after discovery. Flat controls retain their existing first/second tags.
preparePlace	.block
	tst.w State.Active(a4)
	bne.w bad
	bsr.w name
	bne.w bad
	move.w d1, d6
	bsr.w slot
	bmi.w bad
	move.w d0, d7
	bsr.w name
	bne.w bad
	lea InWord(pc), a0
	moveq #2, d0
	bsr.w matches
	bne.w bad
	bsr.w name
	bne.w bad
	bsr.w regionSlot
	bmi.w bad
	move.w d0, d4
	bsr.w optionalAlignment
	bne.w bad
	lea 7(a5), a0
	bsr.w writeLong
	move.b d7, 5(a5)
	move.b d4, 6(a5)
	moveq #23, d5
	move.w State.Concrete(a4), d0
	beq.w second
	move.w d6, d1
	bsr.w sameLeaf
	bne.w second
	moveq #5, d5
	ori.w #8, State.Seen(a4)
	bra.w ready
second
	move.w State.Second(a4), d0
	beq.w ready
	move.w d6, d1
	bsr.w sameLeaf
	bne.w ready
	moveq #9, d5
	ori.w #64, State.Seen(a4)
ready
	move.b #10, (a5)
	move.b #source.FLAG_LAYOUT, 1(a5)
	move.b d5, 4(a5)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; preparePlace

; Optional trailing ,align=N; default one. D1=value,D0/CCR=status.
optionalAlignment	.block
	cmpa.l a3, a2
	beq.w default
	cmpi.b #4, (a2)+
	bne.w bad
	bsr.w name
	bne.w bad
	lea AlignWord(pc), a0
	moveq #5, d0
	bsr.w matches
	bne.w bad
	bsr.w alignmentValue
	bne.w bad
	cmpa.l a3, a2
	bne.w bad
	moveq #0, d0
	rts
default
	moveq #1, d1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; optionalAlignment

; A2/A3=packed =literal. Positive power-of-two alignment, D1=value.
alignmentValue	.block
	cmpa.l a3, a2
	bhs.w bad
	cmpi.b #34, (a2)+
	bne.w bad
	bsr.w literal
	bne.w bad
	tst.l d1
	beq.w bad
	move.l d1, d0
	subq.l #1, d0
	and.l d1, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; alignmentValue

; Decode one already normalized u32 literal. D1=value,D0/CCR=status.
literal	.block
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #5, d0
	blo.w bad
	cmpi.b #2, (a2)+
	bne.w bad
	move.l (a2)+, d1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; literal

; D1=value,A0=destination. Write four big-endian bytes and advance A0.
; Preserves D1; CCR unspecified. The owning module requires 68020.
writeLong	.block
	move.l d1, (a0)+
	rts
	.bend  ; writeLong

; D1=region name,A4=section state,A6=scope. D0=index or -1.
regionSlot	.block
	movem.l d1-d4, -(sp)
	move.w d1, d4
	moveq #0, d3
search
	cmp.w State.RegionCount(a4), d3
	bhs.w create
	move.w d3, d2
	add.w d2, d2
	moveq #0, d0
	move.w REGION_NAMES(a4, d2.w), d0
	move.w d4, d1
	bsr.w sameLeaf
	beq.w found
	addq.w #1, d3
	bra.w search
create
	cmpi.w #8, d3
	bhs.w full
	move.w d3, d2
	add.w d2, d2
	move.w d4, REGION_NAMES(a4, d2.w)
	addq.w #1, State.RegionCount(a4)
found
	moveq #0, d0
	move.w d3, d0
	bra.w done
full
	moveq #-1, d0
done
	movem.l (sp)+, d1-d4
	tst.l d0
	rts
	.bend  ; regionSlot
