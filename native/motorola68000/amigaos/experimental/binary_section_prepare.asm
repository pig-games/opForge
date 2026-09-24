; Lower bounded section syntax to numeric packed control records.
; Names are compared only during preparation; execution sees no source strings.
; @opforge-owner: experimental.amigaos.binary_section_prepare
	.module experimental.amigaos.binary_section_prepare
	.cpu 68020
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as names
	.use experimental.amigaos.binary_source as source
	.pub
State	.struct
First	.word ?
Concrete	.word ?
Region	.word ?
Active	.word ?
Seen	.word ?
MapOwner	.word ?
MapModule	.word ?
MapLogical	.word ?
MapConcrete	.word ?
Second	.word ?
SecondRegion	.word ?
	.endstruct
SCRATCH_BYTES = State.SecondRegion+2
; Seen bits: logical, first concrete/region/place, second concrete/region/place.
; Packed control opcodes 7/8/9 extend the original six with the second
; concrete section, region and placement.
CONTROL_SECTION = 1
CONTROL_ENDSECTION = 2
CONTROL_REGION = 3
CONTROL_PLACE = 4
	.section code, kind=code
	.pub

; A0=State. Clear preparation-only section identities.
begin	.block
	clr.w State.First(a0)
	clr.w State.Concrete(a0)
	clr.w State.Region(a0)
	clr.w State.Active(a0)
	clr.w State.Seen(a0)
	clr.w State.MapOwner(a0)
	clr.w State.MapModule(a0)
	clr.w State.MapLogical(a0)
	clr.w State.MapConcrete(a0)
	clr.w State.Second(a0)
	clr.w State.SecondRegion(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=writer record,A1=scope state,A2=section state,D0=1..4.
; Supports one mapped pair, or two concrete sections in adjacent regions.
; Rewrites controls to [header,opcode,optional u32 start,u32 end].
; D0/CCR=status; other registers preserved.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	move.l d0, d7
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	cmpi.w #9, d0
	blo.w bad
	movea.l a5, a3
	adda.w d0, a3
	lea 4(a5), a2
	cmpi.b #7, (a2)
	bne.w bad  ; labels on section controls are a later layout slice
	adda.w #5, a2
	cmpi.l #CONTROL_SECTION, d7
	beq.w section
	cmpi.l #CONTROL_ENDSECTION, d7
	beq.w endsection
	cmpi.l #CONTROL_REGION, d7
	beq.w region
	cmpi.l #CONTROL_PLACE, d7
	beq.w place
	bra.w bad
section
	tst.w State.Active(a4)
	bne.w bad
	bsr.w name
	bne.w bad
	move.w d1, d6
	moveq #2, d5  ; concrete control opcode
	cmpa.l a3, a2
	beq.w sectionName
	cmpi.b #4, (a2)+
	bne.w bad
	bsr.w name
	bne.w bad
	lea LogicalWord(pc), a0
	moveq #7, d0
	bsr.w matches
	bne.w bad
	moveq #1, d5  ; logical control opcode
sectionName
	cmpa.l a3, a2
	bne.w bad
	tst.w State.MapModule(a4)
	bne.w mappedName
	moveq #0, d0
	move.w State.First(a4), d0
	beq.w firstName
	move.w d6, d1
	bsr.w sameLeaf
	beq.w matched
	cmpi.w #2, d5
	bne.w bad
	moveq #0, d0
	move.w State.Second(a4), d0
	beq.w secondName
	move.w d6, d1
	bsr.w sameLeaf
	bne.w bad
	bra.w secondMatched
secondName
	tst.w State.MapModule(a4)
	bne.w bad
	move.w d6, State.Second(a4)
secondMatched
	moveq #7, d5
	bra.w matched
firstName
	move.w d6, State.First(a4)
	bra.w matched
mappedName
	moveq #0, d0
	cmpi.w #1, d5
	bne.w mappedConcrete
	move.w State.MapLogical(a4), d0
	bra.w mappedCompare
mappedConcrete
	move.w State.MapConcrete(a4), d0
mappedCompare
	move.w d6, d1
	bsr.w sameLeaf
	bne.w bad
	move.l d6, d0
	sub.w layout.State.Base(a6), d0
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a1
	adda.l d0, a1
	move.w names.Entry.Owner(a1), d0
	cmpi.w #1, d5
	bne.w mappedOwner
	cmp.w State.MapModule(a4), d0
	bne.w bad
	bra.w matched
mappedOwner
	cmp.w State.MapOwner(a4), d0
	bne.w bad
	moveq #6, d5  ; runtime schedules this concrete body before mapped content
matched
	cmpi.w #1, d5
	beq.w logical
	cmpi.w #7, d5
	beq.w secondConcrete
	bra.w concrete
logical
	move.w State.Seen(a4), d0
	btst #0, d0
	bne.w bad
	ori.w #1, State.Seen(a4)
	move.w #1, State.Active(a4)
	bra.w control
concrete
	move.w State.Seen(a4), d0
	btst #1, d0
	bne.w bad
	ori.w #2, State.Seen(a4)
	move.w d6, State.Concrete(a4)
	move.w #2, State.Active(a4)
	bra.w control
secondConcrete
	move.w State.Seen(a4), d0
	btst #4, d0
	bne.w bad
	ori.w #16, State.Seen(a4)
	move.w #7, State.Active(a4)
	bra.w control
endsection
	cmpa.l a3, a2
	bne.w bad
	tst.w State.Active(a4)
	beq.w bad
	clr.w State.Active(a4)
	moveq #3, d5
	bra.w control
region
	tst.w State.Active(a4)
	bne.w bad
	move.w State.Seen(a4), d0
	btst #2, d0
	bne.w nextRegion
	moveq #4, d5
	bra.w regionName
nextRegion
	btst #5, d0
	bne.w bad
	tst.w State.MapModule(a4)
	bne.w bad
	moveq #8, d5
regionName
	bsr.w name
	bne.w bad
	cmpi.w #4, d5
	bne.w storeSecondRegion
	move.w d1, State.Region(a4)
	bra.w regionBounds
storeSecondRegion
	move.w d1, State.SecondRegion(a4)
regionBounds
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #12, d0
	bne.w bad
	cmpi.b #4, (a2)+
	bne.w bad
	cmpi.b #2, (a2)
	bne.w bad
	lea 1(a2), a0
	lea 5(a5), a1
	moveq #3, d2
startBytes
	move.b (a0)+, (a1)+
	dbra d2, startBytes
	adda.w #5, a2
	cmpi.b #4, (a2)+
	bne.w bad
	cmpi.b #2, (a2)
	bne.w bad
	lea 1(a2), a0
	moveq #3, d2
endBytes
	move.b (a0)+, (a1)+
	dbra d2, endBytes
	adda.w #5, a2
	cmpa.l a3, a2
	bne.w bad
	cmpi.w #4, d5
	bne.w seenSecondRegion
	ori.w #4, State.Seen(a4)
	bra.w regionControl
seenSecondRegion
	ori.w #32, State.Seen(a4)
regionControl
	move.b #12, (a5)
	move.b #source.FLAG_LAYOUT, 1(a5)
	move.b d5, 4(a5)
	bra.w ok
place
	tst.w State.Active(a4)
	bne.w bad
	moveq #0, d0
	move.w State.Concrete(a4), d0
	beq.w bad
	move.l d0, d6
	bsr.w name
	bne.w bad
	cmp.w d6, d1
	bne.w secondPlace
	move.w State.Seen(a4), d0
	btst #2, d0
	beq.w bad
	btst #3, d0
	bne.w bad
	move.w State.Region(a4), d6
	moveq #5, d5
	bra.w placeRegion
secondPlace
	cmp.w State.Second(a4), d1
	bne.w bad
	move.w State.Seen(a4), d0
	btst #3, d0  ; the first placement fixes contiguous output order
	beq.w bad
	btst #5, d0
	beq.w bad
	btst #6, d0
	bne.w bad
	move.w State.SecondRegion(a4), d6
	moveq #9, d5
placeRegion
	bsr.w name
	bne.w bad
	lea InWord(pc), a0
	moveq #2, d0
	bsr.w matches
	bne.w bad
	bsr.w name
	bne.w bad
	cmp.w d6, d1
	bne.w bad
	cmpa.l a3, a2
	bne.w bad
	cmpi.w #5, d5
	bne.w secondPlaced
	ori.w #8, State.Seen(a4)
	bra.w control
secondPlaced
	ori.w #64, State.Seen(a4)
control
	move.b #4, (a5)
	move.b #source.FLAG_LAYOUT, 1(a5)
	move.b d5, 4(a5)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=first token after the imported module,A1=scope state,A2=section state,
; A4=record end,D0=imported module index. Accept one trailing
; `map { logical -> concrete }` and return A4 at the start of that suffix.
; The ordinary import parser then sees the unchanged prefix. D0/CCR=status;
; all other registers preserved.
importMap	.block
	movem.l d1-d7/a0-a3/a5-a6, -(sp)
	movea.l a1, a6
	movea.l a2, a5
	move.l d0, d7
	movea.l a4, a3
	movea.l a4, a2
	suba.w #16, a2
	cmpa.l a0, a2
	blo.w noMap
	move.l a2, d4
	cmpi.b #1, (a2)
	bhi.w noMap
	bsr.w name
	bne.w noMap
	lea MapWord(pc), a0
	moveq #3, d0
	bsr.w matches
	bne.w noMap
	cmpi.b #12, (a2)+
	bne.w badMap
	bsr.w name
	bne.w badMap
	move.w d1, d5
	cmpi.b #19, (a2)+
	bne.w badMap
	cmpi.b #37, (a2)+
	bne.w badMap
	bsr.w name
	bne.w badMap
	move.w d1, d6
	cmpi.b #13, (a2)+
	bne.w badMap
	cmpa.l a3, a2
	bne.w badMap
	tst.w State.MapModule(a5)
	bne.w badMap
	tst.w State.Second(a5)
	bne.w badMap
	tst.w State.SecondRegion(a5)
	bne.w badMap
	move.w State.Seen(a5), d0
	andi.w #3, d0
	bne.w badMap  ; mapping must precede section declarations
	move.w layout.State.Current(a6), d0
	beq.w badMap
	move.w d0, State.MapOwner(a5)
	move.w d7, d0
	addq.w #1, d0
	move.w d0, State.MapModule(a5)
	move.w d5, State.MapLogical(a5)
	move.w d6, State.MapConcrete(a5)
	movea.l d4, a4
noMap
	moveq #0, d0
	bra.w mapDone
badMap
	moveq #1, d0
mapDone
	movem.l (sp)+, d1-d7/a0-a3/a5-a6
	tst.l d0
	rts
	.bend  ; importMap

; A0=section state. A mapped case requires both named sections by completion.
finish	.block
	tst.w State.Second(a0)
	beq.w noSecond
	move.w State.Seen(a0), d0
	andi.w #$007a, d0  ; both concrete sections, second region and both places
	cmpi.w #$007a, d0
	bne.w badFinish
noSecond
	tst.w State.SecondRegion(a0)
	beq.w mapFinish
	tst.w State.Second(a0)
	beq.w badFinish
mapFinish
	tst.w State.MapModule(a0)
	beq.w finished
	move.w State.Seen(a0), d0
	andi.w #3, d0
	cmpi.w #3, d0
	bne.w badFinish
finished
	moveq #0, d0
	rts
badFinish
	moveq #1, d0
	rts
	.bend  ; finish

	.priv
; A2=name token,A3=end,A6=scope state. D1=source ID,A2 advances.
name	.block
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #4, d0
	blo.w bad
	cmpi.b #1, (a2)
	bhi.w bad
	tst.b 3(a2)
	bne.w bad
	moveq #0, d1
	move.w 1(a2), d1
	moveq #0, d0
	move.w layout.State.Base(a6), d0
	cmp.l d0, d1
	blo.w bad
	sub.l d0, d1
	cmp.w layout.State.Count(a6), d1
	bhs.w bad
	add.w d0, d1
	addq.l #4, a2
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; name

; D1=source ID,A0=lowercase word,D0=length,A6=scope state.
; Compare only the unqualified leaf and preserve inputs except D0.
matches	.block
	movem.l d1-d4/a0-a3, -(sp)
	move.l d0, d4
	movea.l a0, a3
	bsr.w leaf
	bne.w no
	cmp.l d4, d2
	bne.w no
character
	moveq #0, d3
	move.b (a1)+, d3
	bsr.w fold
	cmp.b (a3)+, d3
	bne.w no
	subq.l #1, d2
	bne.w character
	moveq #0, d0
	bra.w matched
no
	moveq #1, d0
matched
	movem.l (sp)+, d1-d4/a0-a3
	tst.l d0
	rts
	.bend  ; matches

; D0,D1=source IDs,A6=scope state. D0/CCR=zero for equal leaves.
sameLeaf	.block
	movem.l d1-d5/a0-a3, -(sp)
	move.l d1, d5
	move.l d0, d1
	bsr.w leaf
	bne.w different
	movea.l a1, a3
	move.l d2, d4
	move.l d5, d1
	bsr.w leaf
	bne.w different
	cmp.l d4, d2
	bne.w different
	movea.l a1, a2
compare
	moveq #0, d3
	move.b (a3)+, d3
	bsr.w fold
	move.l d3, d5
	moveq #0, d3
	move.b (a2)+, d3
	bsr.w fold
	cmp.b d5, d3
	bne.w different
	subq.l #1, d4
	bne.w compare
	moveq #0, d0
	bra.w compared
different
	moveq #1, d0
compared
	movem.l (sp)+, d1-d5/a0-a3
	tst.l d0
	rts
	.bend  ; sameLeaf

; D1=source ID,A6=scope state. A1=leaf,D2=length,D0/CCR=status.
leaf	.block
	sub.w layout.State.Base(a6), d1
	bcs.w bad
	cmp.w layout.State.Count(a6), d1
	bhs.w bad
	lsl.l #4, d1
	lea layout.ENTRIES(a6), a1
	adda.l d1, a1
	moveq #0, d2
	move.w names.Entry.Length(a1), d2
	moveq #0, d3
	move.w names.Entry.Leaf(a1), d3
	sub.l d3, d2
	beq.w bad
	moveq #0, d1
	move.w names.Entry.Name(a1), d1
	add.l d3, d1
	lea layout.ARENA(a6), a1
	adda.l d1, a1
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; leaf

; D3=ASCII byte. Fold A-Z; no other case mapping is part of source names.
fold	.block
	cmpi.b #'A', d3
	blo.w done
	cmpi.b #'Z', d3
	bhi.w done
	addi.b #32, d3
done
	rts
	.bend  ; fold

LogicalWord	.byte "logical"
InWord	.byte "in"
MapWord	.byte "map"
	.align 2  ; keep the next module's instructions word-aligned
	.endsection
	.endmodule
