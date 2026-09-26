; Lower bounded section syntax to numeric packed control records.
; Names are compared only during preparation; execution sees no source strings.
; @opforge-owner: experimental.amigaos.binary_section_prepare
	.module experimental.amigaos.binary_section_prepare
	.cpu 68020
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as names
	.use experimental.amigaos.binary_source as source
	.pub
Map	.struct
Owner	.word ?
Module	.word ?
Logical	.word ?
Concrete	.word ?
	.endstruct
MAP_BYTES = Map.Concrete+2
State	.struct
First	.word ?
Concrete	.word ?
Region	.word ?
Active	.word ?
Seen	.word ?
Second	.word ?
SecondRegion	.word ?
MapCount	.word ?
SlotCount	.word ?
Declared	.word ?
OutputCount	.word ?
OutputSeen	.word ?
Selected	.word ?
	.endstruct
MAPS = State.MapCount+2
SLOT_NAMES = MAPS+2*MAP_BYTES
OUTPUT_SLOTS = SLOT_NAMES+8*2
SCRATCH_BYTES = OUTPUT_SLOTS+8
; Seen bits: first logical/concrete/region/place, second concrete/region/place,
; second logical. Packed control opcodes 10/11 are second logical/concrete.
CONTROL_SECTION = 1
CONTROL_ENDSECTION = 2
CONTROL_REGION = 3
CONTROL_PLACE = 4
	.section code, kind=code
	.pub

; A0=State. Clear preparation-only section identities.
begin	.block
	move.l a0, -(sp)
	clr.w State.First(a0)
	clr.w State.Concrete(a0)
	clr.w State.Region(a0)
	clr.w State.Active(a0)
	clr.w State.Seen(a0)
	clr.w State.Second(a0)
	clr.w State.SecondRegion(a0)
	clr.w State.MapCount(a0)
	clr.w State.SlotCount(a0)
	clr.w State.Declared(a0)
	clr.w State.OutputCount(a0)
	clr.w State.OutputSeen(a0)
	clr.w State.Selected(a0)
	lea MAPS(a0), a0
	moveq #(SCRATCH_BYTES-MAPS)/2-1, d0
clearMaps
	clr.w (a0)+
	dbra d0, clearMaps
	movea.l (sp)+, a0
	moveq #0, d0
	rts
	.bend  ; begin

; A0=writer record,A1=scope state,A2=section state,D0=1..4.
; Supports up to two mapped pairs or two concrete sections in adjacent regions.
; Rewrites section opens to [header,opcode,kind] (1=code,2=data,3=bss).
; Region controls carry optional u32 start,u32 end after the opcode.
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
	moveq #1, d4  ; default section kind is code
	moveq #0, d2  ; seen logical/kind options
sectionOption
	cmpa.l a3, a2
	beq.w sectionName
	cmpi.b #4, (a2)+
	bne.w bad
	bsr.w name
	bne.w bad
	lea LogicalWord(pc), a0
	moveq #7, d0
	bsr.w matches
	bne.w kindOption
	btst #0, d2
	bne.w bad
	bset #0, d2
	moveq #1, d5  ; logical control opcode
	bra.w sectionOption
kindOption
	lea KindWord(pc), a0
	moveq #4, d0
	bsr.w matches
	bne.w bad
	btst #1, d2
	bne.w bad
	bset #1, d2
	cmpa.l a3, a2
	beq.w bad
	cmpi.b #34, (a2)+  ; =
	bne.w bad
	bsr.w name
	bne.w bad
	lea CodeWord(pc), a0
	moveq #4, d0
	bsr.w matches
	beq.w sectionOption
	lea DataWord(pc), a0
	moveq #4, d0
	bsr.w matches
	bne.w bssKind
	moveq #2, d4
	bra.w sectionOption
bssKind
	lea BssWord(pc), a0
	moveq #3, d0
	bsr.w matches
	bne.w bad
	moveq #3, d4
	bra.w sectionOption
sectionName
	cmpa.l a3, a2
	bne.w bad
	tst.w State.MapCount(a4)
	bne.w mappedName
	tst.w State.OutputSeen(a4)
	beq.w flatSectionName
	cmpi.w #2, d5
	bne.w bad
	move.w d6, d1
	bsr.w declareSlot
	bmi.w bad
	move.w d0, d7
	moveq #12, d5
	bra.w matched
flatSectionName
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
	bne.w extraName
	bra.w secondMatched
extraName
	move.w d6, d1
	bsr.w declareSlot
	bmi.w bad
	move.w d0, d7
	moveq #12, d5
	bra.w matched
secondName
	tst.w State.MapCount(a4)
	bne.w bad
	move.w d6, d1
	bsr.w declareSlot
	bmi.w bad
	move.w d0, d7
	move.w d6, State.Second(a4)
secondMatched
	moveq #7, d5
	cmpi.w #1, d7
	beq.w matched
	moveq #12, d5
	bra.w matched
firstName
	move.w d6, d1
	cmpi.w #1, d5
	bne.w firstConcreteSlot
	bsr.w slot
	bra.w firstSlotReady
firstConcreteSlot
	bsr.w declareSlot
firstSlotReady
	bmi.w bad
	move.w d0, d7
	move.w d6, State.First(a4)
	cmpi.w #1, d5
	beq.w matched
	tst.w d7
	beq.w matched
	moveq #12, d5
	bra.w matched
mappedName
	moveq #0, d3
mapNameLoop
	cmp.w State.MapCount(a4), d3
	bhs.w bad
	move.l d3, d0
	lsl.l #3, d0
	lea MAPS(a4), a0
	adda.l d0, a0
	moveq #0, d0
	cmpi.w #1, d5
	bne.w mapConcreteName
	move.w Map.Logical(a0), d0
	bra.w mapCompare
mapConcreteName
	move.w Map.Concrete(a0), d0
mapCompare
	move.w d6, d1
	bsr.w sameLeaf
	bne.w nextMapName
	move.l d6, d0
	sub.w layout.State.Base(a6), d0
	lsl.l #4, d0
	movea.l layout.ENTRIES_POINTER(a6), a1
	adda.l d0, a1
	move.w names.Entry.Owner(a1), d0
	cmpi.w #1, d5
	bne.w mapConcreteOwner
	cmp.w Map.Module(a0), d0
	bne.w nextMapName
	tst.w d3
	beq.w matched
	moveq #10, d5
	bra.w matched
mapConcreteOwner
	cmp.w Map.Owner(a0), d0
	bne.w nextMapName
	tst.w d3
	beq.w firstMappedConcrete
	moveq #11, d5
	bra.w matched
firstMappedConcrete
	moveq #6, d5
	bra.w matched
nextMapName
	addq.w #1, d3
	bra.w mapNameLoop
matched
	cmpi.w #1, d5
	beq.w logical
	cmpi.w #10, d5
	beq.w secondLogical
	cmpi.w #12, d5
	beq.w extraConcrete
	cmpi.w #7, d5
	beq.w secondConcrete
	cmpi.w #11, d5
	beq.w secondConcrete
	bra.w concrete
logical
	move.w State.Seen(a4), d0
	btst #0, d0
	bne.w bad
	ori.w #1, State.Seen(a4)
	move.w #1, State.Active(a4)
	bra.w sectionControl
secondLogical
	move.w State.Seen(a4), d0
	btst #7, d0
	bne.w bad
	ori.w #128, State.Seen(a4)
	move.w #10, State.Active(a4)
	bra.w sectionControl
concrete
	move.w State.Seen(a4), d0
	btst #1, d0
	bne.w bad
	tst.w State.MapCount(a4)
	bne.w firstConcreteReady
	btst #0, d0
	beq.w firstConcreteReady
	move.w d6, d1
	bsr.w declareSlot
	bmi.w bad
	move.w d0, d7
firstConcreteReady
	ori.w #2, State.Seen(a4)
	move.w d6, State.Concrete(a4)
	move.w #2, State.Active(a4)
	bra.w sectionControl
secondConcrete
	move.w State.Seen(a4), d0
	btst #4, d0
	bne.w bad
	ori.w #16, State.Seen(a4)
	move.w d6, State.Second(a4)
	move.w d5, State.Active(a4)
	bra.w sectionControl
extraConcrete
	move.w #12, State.Active(a4)
	bra.w sectionControl
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
	bra.w control
sectionControl
	move.b #5, (a5)
	move.b #source.FLAG_LAYOUT, 1(a5)
	move.b d5, 4(a5)
	move.b d4, 5(a5)
	cmpi.w #12, d5
	bne.w ok
	move.b #6, (a5)
	move.b d7, 6(a5)
	bra.w ok
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

; A0=writer record,A1=scope state,A2=section state. Lower an explicit
; Hunk output selection to numeric section slots. Names may be registered
; before their declarations; finish verifies that every selected slot opens.
; The output path belongs to the CLI caller and is not retained here.
output	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	tst.w State.OutputSeen(a4)
	bne.w outputBad
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	movea.l a5, a3
	adda.w d0, a3
	lea 9(a5), a2  ; directive identifier is a five-byte token
	cmpa.l a3, a2
	bhs.w outputBad
	cmpi.b #3, (a2)+
	bne.w outputBad
	moveq #0, d0
	move.b (a2)+, d0
	adda.w d0, a2
	cmpa.l a3, a2
	bhi.w outputBad
	cmpi.b #4, (a2)+
	bne.w outputBad
	bsr.w name
	bne.w outputBad
	lea FormatWord(pc), a0
	moveq #6, d0
	bsr.w matches
	bne.w outputBad
	cmpi.b #34, (a2)+
	bne.w outputBad
	bsr.w name
	bne.w outputBad
	lea HunkWord(pc), a0
	moveq #4, d0
	bsr.w matches
	bne.w outputBad
	cmpi.b #4, (a2)+
	bne.w outputBad
	bsr.w name
	bne.w outputBad
	lea SectionsWord(pc), a0
	moveq #8, d0
	bsr.w matches
	bne.w outputBad
	cmpi.b #34, (a2)+
	bne.w outputBad
	moveq #0, d6  ; selected-slot bitset
	moveq #0, d7  ; selection count
outputSection
	cmpi.w #8, d7
	bhs.w outputBad
	bsr.w name
	bne.w outputBad
	bsr.w slot
	bmi.w outputBad
	btst d0, d6
	bne.w outputBad
	bset d0, d6
	move.b d0, OUTPUT_SLOTS(a4, d7.w)
	addq.w #1, d7
	cmpa.l a3, a2
	beq.w outputReady
	cmpi.b #4, (a2)+
	bne.w outputBad
	bra.w outputSection
outputReady
	move.w d7, State.OutputCount(a4)
	move.w #1, State.OutputSeen(a4)
	move.w d6, State.Selected(a4)
	move.w d7, d0
	addq.w #5, d0
	move.b d0, (a5)  ; record length minus one
	move.b #source.FLAG_LAYOUT, 1(a5)
	move.b #20, 4(a5)
	move.b d7, 5(a5)
	moveq #0, d6
outputCopy
	move.b OUTPUT_SLOTS(a4, d6.w), d0
	move.b d0, 6(a5, d6.w)
	addq.w #1, d6
	cmp.w d7, d6
	blo.w outputCopy
	moveq #0, d0
	bra.w outputDone
outputBad
	moveq #1, d0
outputDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; output

; A0=first token after the imported module,A1=scope state,A2=section state,
; A4=record end,D0=imported module index. Accept a trailing
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
	tst.w State.Second(a5)
	bne.w badMap
	move.w State.Seen(a5), d0
	andi.w #$0093, d0
	bne.w badMap  ; mapping must precede section declarations
	moveq #0, d0
	move.w State.MapCount(a5), d0
	cmpi.w #2, d0
	bhs.w badMap
	lsl.l #3, d0
	lea MAPS(a5), a0
	adda.l d0, a0
	move.w layout.State.Current(a6), d0
	beq.w badMap
	move.w d0, Map.Owner(a0)
	move.w d7, d0
	addq.w #1, d0
	move.w d0, Map.Module(a0)
	move.w d5, Map.Logical(a0)
	move.w d6, Map.Concrete(a0)
	addq.w #1, State.MapCount(a5)
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
	movem.l d1, -(sp)
	tst.w State.OutputSeen(a0)
	beq.w flatFinish
	tst.w State.MapCount(a0)
	bne.w badFinish
	move.w State.Selected(a0), d1
	and.w State.Declared(a0), d1
	cmp.w State.Selected(a0), d1
	bne.w badFinish
	bra.w finished
flatFinish
	move.w State.MapCount(a0), d1
	cmpi.w #2, d1
	bne.w secondCheck
	tst.w State.Second(a0)
	beq.w badFinish
secondCheck
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
	tst.w d1
	beq.w finished
	move.w State.Seen(a0), d0
	andi.w #3, d0
	cmpi.w #3, d0
	bne.w badFinish
	cmpi.w #1, d1
	beq.w oneMap
	cmpi.w #2, d1
	bne.w badFinish
	move.w State.Seen(a0), d0
	btst #7, d0
	beq.w badFinish
	bra.w finished
oneMap
	tst.w State.Second(a0)
	bne.w badFinish
finished
	moveq #0, d0
	bra.w finishDone
badFinish
	moveq #1, d0

finishDone
	movem.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; finish

	.priv
; D1=source ID,A4=section state,A6=scope state. Return slot 0..7 in D0,
; or -1 when the bounded table is full. Preserve all other registers.
slot	.block
	movem.l d1-d4/a0, -(sp)
	move.w d1, d4
	moveq #0, d3
slotSearch
	cmp.w State.SlotCount(a4), d3
	bhs.w slotNew
	move.w d3, d2
	add.w d2, d2
	moveq #0, d0
	move.w SLOT_NAMES(a4, d2.w), d0
	move.w d4, d1
	bsr.w sameLeaf
	beq.w slotFound
	addq.w #1, d3
	bra.w slotSearch
slotNew
	cmpi.w #8, d3
	bhs.w slotFull
	move.w d3, d2
	add.w d2, d2
	move.w d4, SLOT_NAMES(a4, d2.w)
	addq.w #1, State.SlotCount(a4)
slotFound
	moveq #0, d0
	move.w d3, d0
	bra.w slotDone
slotFull
	moveq #-1, d0
slotDone
	movem.l (sp)+, d1-d4/a0
	tst.l d0
	rts
	.bend  ; slot

; D1=source ID,A4=section state,A6=scope state. Mark one opened section.
; Reopening a section is valid; return its slot or -1 for a full table.
declareSlot	.block
	movem.l d1-d2, -(sp)
	bsr.w slot
	bmi.w declareDone
	move.w State.Declared(a4), d2
	bset d0, d2
	move.w d2, State.Declared(a4)
declareDone
	movem.l (sp)+, d1-d2
	tst.l d0
	rts
	.bend  ; declareSlot

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
	movea.l layout.ENTRIES_POINTER(a6), a1
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
	movea.l layout.ARENA_POINTER(a6), a1
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
KindWord	.byte "kind"
CodeWord	.byte "code"
DataWord	.byte "data"
BssWord	.byte "bss"
InWord	.byte "in"
MapWord	.byte "map"
FormatWord	.byte "format"
HunkWord	.byte "hunk"
SectionsWord	.byte "sections"
	.align 2  ; keep the next module's instructions word-aligned
	.endsection
	.endmodule
