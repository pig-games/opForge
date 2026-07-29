; Native layout arithmetic owner.

	.module opasm.amigaos.layout
	.cpu 68020
	.use opasm.amigaos.engine as eng

OPASM_LAYOUT_NAME_CAPACITY = 32
OPASM_LAYOUT_REGION_CAPACITY = 8
OPASM_LAYOUT_SECTION_CAPACITY = 16
OPASM_LAYOUT_INDEX_NONE = $ffff

	.section code, kind=code
	.pub

; Reset all layout state for a new assembly session.
; Outputs: D0.L = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetStateV1	.block
	clr.w OpasmLayoutRegionCount
	clr.w OpasmLayoutSectionCount
	clr.w OpasmLayoutSectionActive
	move.w #OPASM_LAYOUT_INDEX_NONE, OpasmLayoutActiveSectionIndex
	clr.w OpasmLayoutScratchNameLen
	clr.b OpasmLayoutScratchName
	clr.w OpasmLayoutPlaceSectionNameLen
	clr.b OpasmLayoutPlaceSectionName
	clr.w OpasmLayoutPlaceRegionNameLen
	clr.b OpasmLayoutPlaceRegionName
	moveq #0, d0
	rts
	.bend  ; resetStateV1

; Prepare retained layout state for pass two.
; Outputs: D0.L = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
beginPassTwoV1	.block
	clr.w OpasmLayoutSectionActive
	moveq #0, d0
	rts
	.bend  ; beginPassTwoV1

; Close the active section and retain its pass-one byte size.
; Outputs: D0.L = 0 on success, 1 when no section is active.
; Clobbers: D0-D5/A0/CCR.
; CCR: reflects D0 on return.
processEndsectionV1	.block
	movem.l d1-d5/a0, -(sp)
	tst.w OpasmLayoutSectionActive
	beq.w fail
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.s clearActive
	jsr eng.opasmEngineGetSessionCurrentPcV1
	move.l d0, d1
	moveq #0, d5
	move.w OpasmLayoutActiveSectionIndex, d5
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d5
	beq.w fail
	lea OpasmLayoutSectionStartPcs.l, a0
	bsr.w longTablePtrV1
	sub.l (a0), d1
	lea OpasmLayoutSectionSizes.l, a0
	bsr.w longTablePtrV1
	move.l d1, (a0)

clearActive
	clr.w OpasmLayoutSectionActive
	move.w #OPASM_LAYOUT_INDEX_NONE, OpasmLayoutActiveSectionIndex
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d5/a0
	rts
	.bend  ; processEndsectionV1

; Append one already-validated region to layout-owned state.
; Inputs: A0/D0 = name and length; D1 = start; D2 = end; D3 = alignment.
; Outputs: D0.L = 0 on success, 1 when region capacity is exhausted.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
appendRegionV1	.block
	movem.l d4-d6/a0-a2, -(sp)
	cmp.l d1, d2
	blo.w fail
	moveq #0, d6
	move.w OpasmLayoutRegionCount, d6
	cmpi.w #OPASM_LAYOUT_REGION_CAPACITY, d6
	bhs.w fail
	movea.l d3, a2
	move.l d2, d4
	move.l d1, d3
	lea OpasmLayoutRegionNames.l, a1
	move.l d6, d2
	lsl.l #5, d2
	lea 0(a1, d2.w), a1
	jsr copyNameBytesV1
	lea OpasmLayoutRegionNameLens.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w d0, (a0)
	lea OpasmLayoutRegionStarts.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l d3, (a0)
	lea OpasmLayoutRegionEnds.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l d4, (a0)
	lea OpasmLayoutRegionCursors.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l d3, (a0)
	lea OpasmLayoutRegionAligns.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l a2, d0
	move.l d0, (a0)
	lea OpasmLayoutRegionCount.l, a0
	addq.w #1, (a0)
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d4-d6/a0-a2
	rts
	.bend  ; appendRegionV1

; Append and activate one pass-one section with its established starting PC.
; Inputs: A0/D0 = name and length; D1 = alignment; D2 = starting PC.
; Outputs: D0.L = 0 on success, 1 when section capacity is exhausted.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
appendSectionV1	.block
	movem.l d3-d6/a0-a2, -(sp)
	tst.w OpasmLayoutSectionActive
	bne.w fail
	moveq #0, d6
	move.w OpasmLayoutSectionCount, d6
	cmpi.w #OPASM_LAYOUT_SECTION_CAPACITY, d6
	bhs.w fail
	movea.l d1, a2
	move.l d2, d3
	lea OpasmLayoutSectionNames.l, a1
	move.l d6, d2
	lsl.l #5, d2
	lea 0(a1, d2.w), a1
	jsr copyNameBytesV1
	lea OpasmLayoutSectionNameLens.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w d0, (a0)
	lea OpasmLayoutSectionPlacedFlags.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	clr.w (a0)
	lea OpasmLayoutSectionAligns.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l a2, d0
	move.l d0, (a0)
	lea OpasmLayoutSectionSizes.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	clr.l (a0)
	lea OpasmLayoutSectionBases.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	clr.l (a0)
	lea OpasmLayoutSectionStartPcs.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l d3, (a0)
	move.w #1, OpasmLayoutSectionActive
	move.w d6, OpasmLayoutActiveSectionIndex
	lea OpasmLayoutSectionCount.l, a0
	addq.w #1, (a0)
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d3-d6/a0-a2
	rts
	.bend  ; appendSectionV1

; Activate an existing section for pass two at the supplied current PC.
; Inputs: D0.L = current PC; D5.W = section index.
; Outputs: D0.L = 0 on success, 1 for an invalid section index.
; Clobbers: D0-D2/D5/A0/CCR.
; CCR: reflects D0 on return.
beginSectionPassTwoV1	.block
	cmpi.w #OPASM_LAYOUT_SECTION_CAPACITY, d5
	bhs.w fail
	cmp.w OpasmLayoutSectionCount, d5
	bhs.w fail
	move.w #1, OpasmLayoutSectionActive
	move.w d5, OpasmLayoutActiveSectionIndex
	move.l d0, d1
	lea OpasmLayoutSectionStartPcs.l, a0
	jsr longTablePtrV1
	move.l d1, (a0)
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; beginSectionPassTwoV1

; Place one unplaced section in one region using the established alignment rules.
; Inputs: D0.L = requested alignment; stored place indices select the section and region.
;         Caller has confirmed no section is active.
; Outputs: D0.L = 0 on success, 1 for invalid index, duplicate, overflow, or range failure.
; Clobbers: D0-D6/A0/CCR.
; CCR: reflects D0 on return.
placeSectionV1	.block
	move.l d0, d3
	moveq #0, d5
	move.w OpasmLayoutPlaceSectionIndex, d5
	moveq #0, d6
	move.w OpasmLayoutPlaceRegionIndex, d6
	move.w d5, d4
	cmpi.w #OPASM_LAYOUT_SECTION_CAPACITY, d4
	bhs.w fail
	cmp.w OpasmLayoutSectionCount, d4
	bhs.w fail
	cmpi.w #OPASM_LAYOUT_REGION_CAPACITY, d6
	bhs.w fail
	cmp.w OpasmLayoutRegionCount, d6
	bhs.w fail
	lea OpasmLayoutSectionPlacedFlags.l, a0
	move.w d4, d5
	jsr wordTablePtrV1
	tst.w (a0)
	bne.w fail
	lea OpasmLayoutRegionAligns.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	cmp.l (a0), d3
	bhs.s haveRegionAlign
	move.l (a0), d3

haveRegionAlign
	lea OpasmLayoutSectionAligns.l, a0
	move.w d4, d5
	jsr longTablePtrV1
	cmp.l (a0), d3
	bhs.s haveSectionAlign
	move.l (a0), d3

haveSectionAlign
	lea OpasmLayoutRegionCursors.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l (a0), d1
	move.l d3, d0
	move.w d4, OpasmLayoutPlaceSectionIndex
	bsr.w alignCursorV1
	bne.w fail
	moveq #0, d4
	move.w OpasmLayoutPlaceSectionIndex, d4
	lea OpasmLayoutSectionSizes.l, a0
	move.w d4, d5
	jsr longTablePtrV1
	move.l (a0), d2
	beq.s store
	move.l d1, d0
	add.l d2, d0
	bcs.w fail
	subq.l #1, d0
	lea OpasmLayoutRegionEnds.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	cmp.l (a0), d0
	bhi.w fail
	addq.l #1, d0
	lea OpasmLayoutRegionCursors.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l d0, (a0)
	bra.s storeBase

store
	lea OpasmLayoutRegionCursors.l, a0
	move.w d6, d5
	jsr longTablePtrV1
	move.l d1, (a0)

storeBase
	lea OpasmLayoutSectionBases.l, a0
	move.w d4, d5
	jsr longTablePtrV1
	move.l d1, (a0)
	lea OpasmLayoutSectionPlacedFlags.l, a0
	move.w d4, d5
	jsr wordTablePtrV1
	move.w #1, (a0)
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; placeSectionV1
; Return whether a section remains active after a layout transition.
; Outputs: D0.L = 0 when clear, 1 when active.
; Clobbers: D0/CCR.
sectionActiveV1	.block
	moveq #0, d0
	tst.w OpasmLayoutSectionActive
	beq.s return
	moveq #1, d0
return
	rts
	.bend  ; sectionActiveV1

; Find a region by a caller-supplied bounded name.
; Inputs: A0/D0 = name and length.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D3/D5/A0-A1/CCR.
; CCR: reflects D0 on return.
findRegionByNameV1	.block
	movem.l d1-d3/a0-a1, -(sp)
	movea.l a0, a1
	move.l d0, d1
	moveq #0, d5

loop
	cmp.w OpasmLayoutRegionCount, d5
	bhs.s missing
	lea OpasmLayoutRegionNameLens.l, a0
	jsr wordTablePtrV1
	moveq #0, d0
	move.w (a0), d0
	lea OpasmLayoutRegionNames.l, a0
	moveq #0, d2
	move.w d5, d2
	lsl.l #5, d2
	lea 0(a0, d2.w), a0
	jsr namesMatchV1
	beq.s found
	addq.w #1, d5
	bra.s loop

found
	movem.l (sp)+, d1-d3/a0-a1
	moveq #0, d0
	rts

missing
	movem.l (sp)+, d1-d3/a0-a1
	moveq #1, d0
	rts
	.bend  ; findRegionByNameV1

; Find a section by a caller-supplied bounded name.
; Inputs: A0/D0 = name and length.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D3/D5/A0-A1/CCR.
; CCR: reflects D0 on return.
findSectionByNameV1	.block
	movem.l d1-d3/a0-a1, -(sp)
	movea.l a0, a1
	move.l d0, d1
	moveq #0, d5

loop
	cmp.w OpasmLayoutSectionCount, d5
	bhs.s missing
	lea OpasmLayoutSectionNameLens.l, a0
	jsr wordTablePtrV1
	moveq #0, d0
	move.w (a0), d0
	lea OpasmLayoutSectionNames.l, a0
	moveq #0, d2
	move.w d5, d2
	lsl.l #5, d2
	lea 0(a0, d2.w), a0
	jsr namesMatchV1
	beq.s found
	addq.w #1, d5
	bra.s loop

found
	movem.l (sp)+, d1-d3/a0-a1
	moveq #0, d0
	rts

missing
	movem.l (sp)+, d1-d3/a0-a1
	moveq #1, d0
	rts
	.bend  ; findSectionByNameV1

; Return whether the current scratch region overlaps an existing region.
; Outputs: D0.L = 0 when clear, 1 on overlap.
; Clobbers: D0-D6/A0/CCR.
; CCR: reflects D0 on return.
scratchRegionOverlapsV1	.block
	moveq #0, d6

loop
	cmp.w OpasmLayoutRegionCount, d6
	bhs.s clear
	move.w d6, d5
	lea OpasmLayoutRegionEnds.l, a0
	jsr longTablePtrV1
	move.l OpasmLayoutScratchStart, d0
	cmp.l (a0), d0
	bhi.s next
	lea OpasmLayoutRegionStarts.l, a0
	jsr longTablePtrV1
	move.l (a0), d0
	cmp.l OpasmLayoutScratchEnd, d0
	bhi.s next
	moveq #1, d0
	rts

next
	addq.w #1, d6
	bra.s loop

clear
	moveq #0, d0
	rts
	.bend  ; scratchRegionOverlapsV1

; Return the placed-flag field for a section index.
; Inputs: D5.W = section index.
; Outputs: A0 = placed-flag field pointer.
; Clobbers: D2/A0/CCR.
; CCR: reflects D2 after index scaling.
sectionPlacedPtrV1	.block
	lea OpasmLayoutSectionPlacedFlags.l, a0
	jsr wordTablePtrV1
	rts
	.bend  ; sectionPlacedPtrV1

; Return the base field for a section index.
; Inputs: D5.W = section index.
; Outputs: A0 = base field pointer.
; Clobbers: D2/A0/CCR.
; CCR: reflects D2 after index scaling.
sectionBasePtrV1	.block
	lea OpasmLayoutSectionBases.l, a0
	jsr longTablePtrV1
	rts
	.bend  ; sectionBasePtrV1

; Return the region-name scratch buffer.
; Outputs: A0 = scratch buffer pointer.
; Clobbers: A0.
scratchNamePtrV1	.block
	lea OpasmLayoutScratchName.l, a0
	rts
	.bend  ; scratchNamePtrV1

; Store the current scratch-name length.
; Inputs: D0.W = name length.
; Clobbers: none.
setScratchNameLenV1	.block
	move.w d0, OpasmLayoutScratchNameLen
	rts
	.bend  ; setScratchNameLenV1

; Return the current scratch name.
; Outputs: A0 = name pointer; D0.L = name length.
; Clobbers: D0/A0/CCR.
getScratchNameV1	.block
	lea OpasmLayoutScratchName.l, a0
	moveq #0, d0
	move.w OpasmLayoutScratchNameLen, d0
	rts
	.bend  ; getScratchNameV1

; Store the parsed region start/end values.
; Inputs: D0.L = value; D1.W = 0 start, 1 end.
; Outputs: D0.L = 0 on success, 1 for an invalid selector.
; Clobbers: D0/CCR.
setScratchRegionBoundV1	.block
	tst.w d1
	beq.s start
	cmpi.w #1, d1
	bne.s fail
	move.l d0, OpasmLayoutScratchEnd
	moveq #0, d0
	rts
start
	move.l d0, OpasmLayoutScratchStart
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; setScratchRegionBoundV1

; Return the current scratch region request.
; Outputs: A0/D0 = name and length; D1 = start; D2 = end.
; Clobbers: D0-D2/A0/CCR.
getScratchRegionRequestV1	.block
	lea OpasmLayoutScratchName.l, a0
	moveq #0, d0
	move.w OpasmLayoutScratchNameLen, d0
	move.l OpasmLayoutScratchStart, d1
	move.l OpasmLayoutScratchEnd, d2
	rts
	.bend  ; getScratchRegionRequestV1

; Return the parsed `.place` section-name buffer.
; Outputs: A0 = name buffer pointer.
; Clobbers: A0.
placeSectionNamePtrV1	.block
	lea OpasmLayoutPlaceSectionName.l, a0
	rts
	.bend  ; placeSectionNamePtrV1

; Return the parsed `.place` region-name buffer.
; Outputs: A0 = name buffer pointer.
; Clobbers: A0.
placeRegionNamePtrV1	.block
	lea OpasmLayoutPlaceRegionName.l, a0
	rts
	.bend  ; placeRegionNamePtrV1

; Store one parsed `.place` name length.
; Inputs: D0.W = length; D1.W = 0 section, 1 region.
; Outputs: D0.L = 0 on success, 1 for an invalid selector.
; Clobbers: D0/CCR.
setPlaceNameLenV1	.block
	tst.w d1
	beq.s section
	cmpi.w #1, d1
	bne.s fail
	move.w d0, OpasmLayoutPlaceRegionNameLen
	moveq #0, d0
	rts
section
	move.w d0, OpasmLayoutPlaceSectionNameLen
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; setPlaceNameLenV1

; Store one resolved `.place` index.
; Inputs: D0.W = index; D1.W = 0 section, 1 region.
; Outputs: D0.L = 0 on success, 1 for an invalid selector.
; Clobbers: D0/CCR.
setPlaceIndexV1	.block
	tst.w d1
	beq.s section
	cmpi.w #1, d1
	bne.s fail
	move.w d0, OpasmLayoutPlaceRegionIndex
	moveq #0, d0
	rts
section
	move.w d0, OpasmLayoutPlaceSectionIndex
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; setPlaceIndexV1

; Find the current scratch region/section name.
; Inputs: D1.W = 0 region, 1 section.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D5/A0-A1/CCR.
findScratchNameV1	.block
	lea OpasmLayoutScratchName.l, a0
	moveq #0, d0
	move.w OpasmLayoutScratchNameLen, d0
	tst.w d1
	bne.s section
	jsr findRegionByNameV1
	rts
section
	jsr findSectionByNameV1
	rts
	.bend  ; findScratchNameV1

; Find the current `.place` region/section name.
; Inputs: D1.W = 0 region, 1 section.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D5/A0-A1/CCR.
findPlaceNameV1	.block
	tst.w d1
	bne.s section
	lea OpasmLayoutPlaceRegionName.l, a0
	moveq #0, d0
	move.w OpasmLayoutPlaceRegionNameLen, d0
	jsr findRegionByNameV1
	rts
section
	lea OpasmLayoutPlaceSectionName.l, a0
	moveq #0, d0
	move.w OpasmLayoutPlaceSectionNameLen, d0
	jsr findSectionByNameV1
	rts
	.bend  ; findPlaceNameV1

; Align a cursor with Rust-compatible positive-integer align_up.
; Inputs: D0.L = align; D1.L = cursor.
; Outputs: D0.L = status; D1.L = aligned cursor.
; Clobbers: D0/D2-D4/CCR.
; CCR: reflects D0 on return.
alignCursorV1	.block
	tst.l d0
	beq.s fail
	cmpi.l #1, d0
	bls.s ok
	move.l d0, d4
	moveq #0, d3
	move.l d1, d2
	divu.l d4, d3:d2
	tst.l d3
	beq.s ok
	move.l d4, d2
	sub.l d3, d2
	move.l d1, d3
	add.l d2, d3
	bcs.s fail
	move.l d3, d1
ok
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; alignCursorV1

; Compute power-of-two `.align` padding.
; Inputs: D0.L = current PC; D3.L = requested alignment.
; Outputs: D0.L = status; D3.L = padding.
; Clobbers: D0/D2/D4-D5/CCR.
; CCR: reflects D0 on return.
alignPadV1	.block
	move.l d0, d2
	move.l d3, d4
	beq.s fail
	move.l d4, d0
	subq.l #1, d0
	move.l d0, d5
	and.l d4, d0
	bne.s fail
	move.l d2, d0
	and.l d5, d0
	beq.s aligned
	move.l d4, d3
	sub.l d0, d3
	moveq #0, d0
	rts
aligned
	clr.l d3
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; alignPadV1

; Copy a bounded layout name into a table slot.
; Inputs: A0 = source; A1 = destination; D0.W = byte count.
; Outputs: destination bytes plus trailing NUL.
; Clobbers: D0-D1/A0-A1/CCR.
; CCR: reflects D0.W on return.
copyNameBytesV1	.block
	move.w d0, d1
	beq.s done
loop
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.s loop
done
	clr.b (a1)
	rts
	.bend  ; copyNameBytesV1

; Index into a word table.
; Inputs: A0 = table base; D5.W = index.
; Outputs: A0 = indexed field pointer.
; Clobbers: D2/A0/CCR.
; CCR: reflects D2.L after index scaling.
wordTablePtrV1	.block
	moveq #0, d2
	move.w d5, d2
	lsl.l #1, d2
	lea 0(a0, d2.w), a0
	rts
	.bend  ; wordTablePtrV1

; Index into a long table.
; Inputs: A0 = table base; D5.W = index.
; Outputs: A0 = indexed field pointer.
; Clobbers: D2/A0/CCR.
; CCR: reflects D2.L after index scaling.
longTablePtrV1	.block
	moveq #0, d2
	move.w d5, d2
	lsl.l #2, d2
	lea 0(a0, d2.w), a0
	rts
	.bend  ; longTablePtrV1

; Compare two bounded layout names case-insensitively.
; Inputs: A0/D0 = left pointer/length; A1/D1 = right pointer/length.
; Outputs: D0.L = 0 on match, 1 on mismatch.
; Clobbers: D0-D4/A0-A1/CCR.
; CCR: reflects D0.L on return.
namesMatchV1	.block
	movem.l d1-d4/a0-a1, -(sp)
	cmp.l d1, d0
	bne.w fail
	move.l d0, d2
	beq.w fail
loop
	tst.l d2
	beq.w ok
	move.b (a0)+, d3
	move.b (a1)+, d4
	bsr.w lowerD3
	exg d3, d4
	bsr.w lowerD3
	exg d3, d4
	cmp.b d4, d3
	bne.w fail
	subq.l #1, d2
	bra.s loop
ok
	movem.l (sp)+, d1-d4/a0-a1
	moveq #0, d0
	rts
fail
	movem.l (sp)+, d1-d4/a0-a1
	moveq #1, d0
	rts
	.bend  ; namesMatchV1

; Lowercase an ASCII byte in D3.B.
; Inputs: D3.B = byte.
; Outputs: D3.B = lowercase ASCII when applicable.
; Clobbers: D3/CCR.
; CCR: unspecified on return.
lowerD3	.block
	cmpi.b #'A', d3
	bcs.s done
	cmpi.b #'Z', d3
	bhi.s done
	addi.b #32, d3
done
	rts
	.bend  ; lowerD3

	.endsection

	.section bss, kind=bss

OpasmLayoutRegionCount
	.res word, 1

OpasmLayoutSectionCount
	.res word, 1

OpasmLayoutSectionActive
	.res word, 1

OpasmLayoutActiveSectionIndex
	.res word, 1

OpasmLayoutPlaceSectionIndex
	.res word, 1

OpasmLayoutPlaceRegionIndex
	.res word, 1

OpasmLayoutScratchStart
	.res long, 1

OpasmLayoutScratchEnd
	.res long, 1

OpasmLayoutScratchNameLen
	.res word, 1

OpasmLayoutScratchName
	.res byte, OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutRegionNameLens
	.res word, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionNames
	.res byte, OPASM_LAYOUT_REGION_CAPACITY * OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutRegionStarts
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionEnds
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionCursors
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionAligns
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutSectionNameLens
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionNames
	.res byte, OPASM_LAYOUT_SECTION_CAPACITY * OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutSectionPlacedFlags
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionStartPcs
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionSizes
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionBases
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionAligns
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutPlaceSectionNameLen
	.res word, 1

OpasmLayoutPlaceSectionName
	.res byte, OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutPlaceRegionNameLen
	.res word, 1

OpasmLayoutPlaceRegionName
	.res byte, OPASM_LAYOUT_NAME_CAPACITY

	.endsection
	.endmodule
