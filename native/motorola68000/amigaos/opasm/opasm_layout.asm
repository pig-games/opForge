; Native layout arithmetic owner.

	.module opasm.amigaos.layout
	.cpu 68020
	.use opasm.amigaos.engine as eng

	.section code, kind=code
	.pub

OPASM_LAYOUT_NAME_CAPACITY = 32
OPASM_LAYOUT_OWNER_CAPACITY = 64
OPASM_LAYOUT_REGION_CAPACITY = 8
OPASM_LAYOUT_SECTION_CAPACITY = 16
OPASM_LAYOUT_MAP_CAPACITY = 16
OPASM_LAYOUT_STATEMENT_CAPACITY = 512
OPASM_LAYOUT_REACHABLE_CAPACITY = 512
OPASM_LAYOUT_INDEX_NONE = $ffff
OPASM_LAYOUT_SECTION_KIND_CODE = 0
OPASM_LAYOUT_SECTION_KIND_DATA = 1
OPASM_LAYOUT_SECTION_KIND_BSS = 2

; Reset all layout state for a new assembly session.
; Outputs: D0.L = 0.
; Clobbers: D0/CCR; D1/A0-A1 are preserved.
; CCR: reflects D0 on return.
resetStateV1	.block
	movem.l d1/a0-a1, -(sp)
	clr.w OpasmLayoutRegionCount.l
	clr.w OpasmLayoutSectionCount.l
	clr.w OpasmLayoutSectionActive.l
	move.w #OPASM_LAYOUT_INDEX_NONE, OpasmLayoutActiveSectionIndex.l
	clr.w OpasmLayoutScratchNameLen
	clr.b OpasmLayoutScratchName
	clr.w OpasmLayoutScratchSectionOwnerLen
	clr.b OpasmLayoutScratchSectionOwner
	clr.w OpasmLayoutScratchSectionKind
	clr.w OpasmLayoutScratchSectionLogical
	clr.w OpasmLayoutScratchMapLogicalOwnerLen
	clr.b OpasmLayoutScratchMapLogicalOwner
	clr.w OpasmLayoutScratchMapConcreteOwnerLen
	clr.b OpasmLayoutScratchMapConcreteOwner
	clr.w OpasmLayoutMapCount.l
	clr.w OpasmLayoutReachableLabelCount.l
	clr.l OpasmLayoutMappedByteCount.l
	clr.w OpasmLayoutPlaceSectionNameLen
	clr.b OpasmLayoutPlaceSectionName
	clr.w OpasmLayoutPlaceRegionNameLen
	clr.b OpasmLayoutPlaceRegionName
	lea OpasmLayoutStatementSectionIndices.l, a0
	lea OpasmLayoutStatementMappedFlags.l, a1
	move.w #OPASM_LAYOUT_STATEMENT_CAPACITY - 1, d1
statementResetLoop
	move.w #OPASM_LAYOUT_INDEX_NONE, (a0)+
	clr.b (a1)+
	dbf d1, statementResetLoop
	movem.l (sp)+, d1/a0-a1
	moveq #0, d0
	rts
	.bend  ; resetStateV1

; Prepare retained layout state for pass two.
; Outputs: D0.L = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
beginPassTwoV1	.block
	clr.w OpasmLayoutSectionActive.l
	moveq #0, d0
	rts
	.bend  ; beginPassTwoV1

; Close the active section and retain its pass-one byte size.
; Outputs: D0.L = 0 on success, 1 when no section is active.
; Clobbers: D0-D5/A0/CCR.
; CCR: reflects D0 on return.
processEndsectionV1	.block
	movem.l d1-d5/a0, -(sp)
	tst.w OpasmLayoutSectionActive.l
	beq.w fail
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.s clearActive
	jsr eng.opasmEngineGetSessionCurrentPcV1
	move.l d0, d1
	moveq #0, d5
	move.w OpasmLayoutActiveSectionIndex.l, d5
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d5
	beq.w fail
	lea OpasmLayoutSectionStartPcs.l, a0
	bsr.w longTablePtrV1
	sub.l (a0), d1
	lea OpasmLayoutSectionSizes.l, a0
	bsr.w longTablePtrV1
	move.l d1, (a0)

clearActive
	clr.w OpasmLayoutSectionActive.l
	move.w #OPASM_LAYOUT_INDEX_NONE, OpasmLayoutActiveSectionIndex.l
	moveq #0, d0
	bra.w return

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
	move.w OpasmLayoutRegionCount.l, d6
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
	tst.w OpasmLayoutSectionActive.l
	bne.w fail
	moveq #0, d6
	move.w OpasmLayoutSectionCount.l, d6
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
	lea OpasmLayoutScratchSectionOwner.l, a0
	moveq #0, d0
	move.w OpasmLayoutScratchSectionOwnerLen, d0
	move.l d6, d2
	lsl.l #6, d2
	lea OpasmLayoutSectionOwnerNames.l, a1
	adda.l d2, a1
	jsr copyNameBytesV1
	lea OpasmLayoutSectionOwnerNameLens.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w OpasmLayoutScratchSectionOwnerLen, (a0)
	lea OpasmLayoutSectionPlacedFlags.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	clr.w (a0)
	lea OpasmLayoutSectionKinds.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w OpasmLayoutScratchSectionKind, (a0)
	lea OpasmLayoutSectionLogicalFlags.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w OpasmLayoutScratchSectionLogical, (a0)
	lea OpasmLayoutSectionRegionIndices.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w #OPASM_LAYOUT_INDEX_NONE, (a0)
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
	move.w #1, OpasmLayoutSectionActive.l
	move.w d6, OpasmLayoutActiveSectionIndex.l
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
	cmp.w OpasmLayoutSectionCount.l, d5
	bhs.w fail
	move.w #1, OpasmLayoutSectionActive.l
	move.w d5, OpasmLayoutActiveSectionIndex.l
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
	cmp.w OpasmLayoutSectionCount.l, d4
	bhs.w fail
	cmpi.w #OPASM_LAYOUT_REGION_CAPACITY, d6
	bhs.w fail
	cmp.w OpasmLayoutRegionCount.l, d6
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
	lea OpasmLayoutSectionRegionIndices.l, a0
	move.w d4, d5
	jsr wordTablePtrV1
	move.w d6, (a0)
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
	tst.w OpasmLayoutSectionActive.l
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
	cmp.w OpasmLayoutRegionCount.l, d5
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
	cmp.w OpasmLayoutSectionCount.l, d5
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

; Find a section by its structural owner and bounded name.
; Inputs: A0/D0 = section name; A1/D1 = owner name (zero length is valid).
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0 on return.
findSectionByOwnedNameV1	.block
	movem.l d1-d4/d6/a0-a3, -(sp)
	movea.l a0, a2
	move.l d0, d4
	movea.l a1, a3
	move.l d1, d6
	moveq #0, d5

loop
	cmp.w OpasmLayoutSectionCount.l, d5
	bhs.w missing
	lea OpasmLayoutSectionNameLens.l, a0
	jsr wordTablePtrV1
	moveq #0, d0
	move.w (a0), d0
	movea.l a2, a1
	move.l d4, d1
	lea OpasmLayoutSectionNames.l, a0
	moveq #0, d2
	move.w d5, d2
	lsl.l #5, d2
	lea 0(a0, d2.w), a0
	jsr namesMatchV1
	bne.s next
	lea OpasmLayoutSectionOwnerNameLens.l, a0
	jsr wordTablePtrV1
	cmp.w (a0), d6
	bne.s next
	tst.w d6
	beq.s found
	lea OpasmLayoutSectionOwnerNames.l, a0
	moveq #0, d2
	move.w d5, d2
	lsl.l #6, d2
	lea 0(a0, d2.w), a0
	movea.l a3, a1
	moveq #0, d0
	move.w d6, d0
	move.l d6, d1
	jsr namesMatchV1
	beq.s found
next
	addq.w #1, d5
	bra.w loop

found
	movem.l (sp)+, d1-d4/d6/a0-a3
	moveq #0, d0
	rts
missing
	movem.l (sp)+, d1-d4/d6/a0-a3
	moveq #1, d0
	rts
	.bend  ; findSectionByOwnedNameV1

; Return whether the current scratch region overlaps an existing region.
; Outputs: D0.L = 0 when clear, 1 on overlap.
; Clobbers: D0-D6/A0/CCR.
; CCR: reflects D0 on return.
scratchRegionOverlapsV1	.block
	moveq #0, d6

loop
	cmp.w OpasmLayoutRegionCount.l, d6
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

; Retain the structural owner for the current scratch section.
; Inputs: A0/D0 = owner name and length (zero length is valid).
; Outputs: D0.L = 0 on success, 1 on invalid length.
setScratchSectionOwnerV1	.block
	movem.l d1/a0-a1, -(sp)
	cmpi.w #OPASM_LAYOUT_OWNER_CAPACITY - 1, d0
	bhi.s fail
	move.w d0, OpasmLayoutScratchSectionOwnerLen
	lea OpasmLayoutScratchSectionOwner.l, a1
	jsr copyNameBytesV1
	movem.l (sp)+, d1/a0-a1
	moveq #0, d0
	rts
fail
	movem.l (sp)+, d1/a0-a1
	moveq #1, d0
	rts
	.bend  ; setScratchSectionOwnerV1

; Retain the structural owners for the next logical-to-concrete map.
; Inputs: A0/D0 = logical owner; A1/D1 = concrete owner.
; Outputs: D0.L = 0 on success, 1 on invalid length.
setScratchMapOwnersV1	.block
	movem.l d1-d3/a0-a3, -(sp)
	cmpi.w #OPASM_LAYOUT_OWNER_CAPACITY - 1, d0
	bhi.s fail
	cmpi.w #OPASM_LAYOUT_OWNER_CAPACITY - 1, d1
	bhi.s fail
	movea.l a1, a3
	move.w d1, d3
	move.w d0, OpasmLayoutScratchMapLogicalOwnerLen
	lea OpasmLayoutScratchMapLogicalOwner.l, a1
	jsr copyNameBytesV1
	movea.l a3, a0
	move.w d3, d0
	move.w d0, OpasmLayoutScratchMapConcreteOwnerLen
	lea OpasmLayoutScratchMapConcreteOwner.l, a1
	jsr copyNameBytesV1
	movem.l (sp)+, d1-d3/a0-a3
	moveq #0, d0
	rts
fail
	movem.l (sp)+, d1-d3/a0-a3
	moveq #1, d0
	rts
	.bend  ; setScratchMapOwnersV1

; Store the parsed section kind for the next pass-one section append.
; Inputs: D0.W = OPASM_LAYOUT_SECTION_KIND_*.
; Outputs: D0.L = 0 on success, 1 for an invalid kind.
; Clobbers: D0/CCR.
setScratchSectionKindV1	.block
	cmpi.w #OPASM_LAYOUT_SECTION_KIND_BSS, d0
	bhi.s fail
	move.w d0, OpasmLayoutScratchSectionKind
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; setScratchSectionKindV1

; Store whether the next pass-one section is logical rather than directly emitted.
; Inputs: D0.W = 0 concrete, nonzero logical. Outputs: D0.L = 0.
setScratchSectionLogicalV1	.block
	tst.w d0
	beq.s store
	moveq #1, d0
store
	move.w d0, OpasmLayoutScratchSectionLogical
	moveq #0, d0
	rts
	.bend  ; setScratchSectionLogicalV1

; Retain the active section owner for one pass-one statement.
; Inputs: D0.W = statement index. Outputs: D0.L = 0 on success.
recordStatementSectionV1	.block
	cmpi.w #OPASM_LAYOUT_STATEMENT_CAPACITY, d0
	bhs.s fail
	moveq #0, d1
	move.w d0, d1
	add.w d1, d1
	lea OpasmLayoutStatementSectionIndices.l, a0
	adda.l d1, a0
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, (a0)
	bne.s success
	move.w OpasmLayoutActiveSectionIndex.l, (a0)
success
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; recordStatementSectionV1

; Retain one structural logical-to-concrete section map.
; Inputs: A0/D0 = logical name; A1/D1 = concrete name.
; Outputs: D0.L = 0 on success, 1 on invalid length/capacity.
recordSectionMapV1	.block
	movem.l d2-d6/a0-a3, -(sp)
	tst.w d0
	beq.w fail
	tst.w d1
	beq.w fail
	cmpi.w #OPASM_LAYOUT_NAME_CAPACITY - 1, d0
	bhi.w fail
	cmpi.w #OPASM_LAYOUT_NAME_CAPACITY - 1, d1
	bhi.w fail
	moveq #0, d6
	move.w OpasmLayoutMapCount.l, d6
	cmpi.w #OPASM_LAYOUT_MAP_CAPACITY, d6
	bhs.w fail
	movea.l a1, a3
	move.w d1, d4
	move.l d6, d2
	lsl.l #5, d2
	lea OpasmLayoutMapLogicalNames.l, a1
	adda.l d2, a1
	jsr copyNameBytesV1
	lea OpasmLayoutMapLogicalNameLens.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w d0, (a0)
	movea.l a3, a0
	move.w d4, d0
	move.l d6, d2
	lsl.l #5, d2
	lea OpasmLayoutMapConcreteNames.l, a1
	adda.l d2, a1
	jsr copyNameBytesV1
	lea OpasmLayoutMapConcreteNameLens.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w d4, (a0)
	move.l d6, d2
	lsl.l #6, d2
	lea OpasmLayoutScratchMapLogicalOwner.l, a0
	moveq #0, d0
	move.w OpasmLayoutScratchMapLogicalOwnerLen, d0
	lea OpasmLayoutMapLogicalOwnerNames.l, a1
	adda.l d2, a1
	jsr copyNameBytesV1
	lea OpasmLayoutMapLogicalOwnerNameLens.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w OpasmLayoutScratchMapLogicalOwnerLen, (a0)
	move.l d6, d2
	lsl.l #6, d2
	lea OpasmLayoutScratchMapConcreteOwner.l, a0
	moveq #0, d0
	move.w OpasmLayoutScratchMapConcreteOwnerLen, d0
	lea OpasmLayoutMapConcreteOwnerNames.l, a1
	adda.l d2, a1
	jsr copyNameBytesV1
	lea OpasmLayoutMapConcreteOwnerNameLens.l, a0
	move.w d6, d5
	jsr wordTablePtrV1
	move.w OpasmLayoutScratchMapConcreteOwnerLen, (a0)
	lea OpasmLayoutMapCount.l, a0
	addq.w #1, (a0)
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d2-d6/a0-a3
	rts
	.bend  ; recordSectionMapV1

; Retain a label whose selected use makes its logical unit reachable.
; Inputs: D0.W = label index. Outputs: D0.L = 0 on success.
recordReachableLabelV1	.block
	movem.l d1-d3/a0, -(sp)
	move.w d0, d1
	moveq #0, d2
	move.w OpasmLayoutReachableLabelCount.l, d2
	moveq #0, d3
	lea OpasmLayoutReachableLabelIndices.l, a0
duplicateLoop
	cmp.w d2, d3
	bhs.s append
	cmp.w (a0)+, d1
	beq.s success
	addq.w #1, d3
	bra.s duplicateLoop
append
	cmpi.w #OPASM_LAYOUT_REACHABLE_CAPACITY, d2
	bhs.s fail
	add.w d2, d2
	lea OpasmLayoutReachableLabelIndices.l, a0
	adda.l d2, a0
	move.w d1, (a0)
	lea OpasmLayoutReachableLabelCount.l, a0
	addq.w #1, (a0)
success
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d3/a0
	rts
	.bend  ; recordReachableLabelV1

; Return the retained region count.
; Outputs: D0.L = count.
; Clobbers: D0/CCR.
getRegionCountV1	.block
	moveq #0, d0
	move.w OpasmLayoutRegionCount.l, d0
	rts
	.bend  ; getRegionCountV1

; Return one retained region name.
; Inputs: D5.W = region index.
; Outputs: A0 = name; D0.L = length, or zero for invalid index.
; Clobbers: D0/D2/A0/CCR.
getRegionNameV1	.block
	cmp.w OpasmLayoutRegionCount.l, d5
	bhs.s invalid
	move.l d5, d2
	lsl.l #5, d2
	lea OpasmLayoutRegionNames.l, a0
	adda.l d2, a0
	lea OpasmLayoutRegionNameLens.l, a0
	jsr wordTablePtrV1
	moveq #0, d0
	move.w (a0), d0
	move.l d5, d2
	lsl.l #5, d2
	lea OpasmLayoutRegionNames.l, a0
	adda.l d2, a0
	rts
invalid
	suba.l a0, a0
	moveq #0, d0
	rts
	.bend  ; getRegionNameV1

; Return one retained region's numeric state.
; Inputs: D5.W = region index.
; Outputs: D0=start, D1=end, D2=cursor, D3=alignment.
; Clobbers: D0-D3/D5/A0/CCR.
getRegionInfoV1	.block
	lea OpasmLayoutRegionStarts.l, a0
	jsr longTablePtrV1
	move.l (a0), -(sp)
	lea OpasmLayoutRegionEnds.l, a0
	jsr longTablePtrV1
	move.l (a0), -(sp)
	lea OpasmLayoutRegionCursors.l, a0
	jsr longTablePtrV1
	move.l (a0), -(sp)
	lea OpasmLayoutRegionAligns.l, a0
	jsr longTablePtrV1
	move.l (a0), d3
	move.l (sp)+, d2
	move.l (sp)+, d1
	move.l (sp)+, d0
	rts
	.bend  ; getRegionInfoV1

; Return the retained section count.
; Outputs: D0.L = count.
; Clobbers: D0/CCR.
getSectionCountV1	.block
	moveq #0, d0
	move.w OpasmLayoutSectionCount.l, d0
	rts
	.bend  ; getSectionCountV1

; Return one retained section name.
; Inputs: D5.W = section index.
; Outputs: A0 = name; D0.L = length, or zero for invalid index.
; Clobbers: D0/D2/A0/CCR.
getSectionNameV1	.block
	cmp.w OpasmLayoutSectionCount.l, d5
	bhs.s invalid
	lea OpasmLayoutSectionNameLens.l, a0
	jsr wordTablePtrV1
	moveq #0, d0
	move.w (a0), d0
	move.l d5, d2
	lsl.l #5, d2
	lea OpasmLayoutSectionNames.l, a0
	adda.l d2, a0
	rts
invalid
	suba.l a0, a0
	moveq #0, d0
	rts
	.bend  ; getSectionNameV1

; Return one retained section's numeric state.
; Inputs: D5.W = section index.
; Outputs: D0=base, D1=size, D2=kind, D3=region index.
; Clobbers: D0-D3/D5/A0/CCR.
getSectionInfoV1	.block
	lea OpasmLayoutSectionBases.l, a0
	jsr longTablePtrV1
	move.l (a0), -(sp)
	lea OpasmLayoutSectionSizes.l, a0
	jsr longTablePtrV1
	move.l (a0), -(sp)
	lea OpasmLayoutSectionKinds.l, a0
	jsr wordTablePtrV1
	moveq #0, d0
	move.w (a0), d0
	move.l d0, -(sp)
	lea OpasmLayoutSectionRegionIndices.l, a0
	jsr wordTablePtrV1
	moveq #0, d3
	move.w (a0), d3
	move.l (sp)+, d2
	move.l (sp)+, d1
	move.l (sp)+, d0
	rts
	.bend  ; getSectionInfoV1

; Translate a pass-one PC through the final placed section layout.
; Inputs: D0 = pass-one PC. Outputs: D0 = placed PC and D1 = 1 when the PC
; belongs to a placed section; otherwise D0 is unchanged and D1 = 0.
translatePassOneAddressV1	.block
	movem.l d2-d7/a0-a2, -(sp)
	move.l d0, d7
	moveq #0, d6
	move.w OpasmLayoutSectionCount.l, d6
	subq.w #1, d6
sectionLoop
	tst.w d6
	bmi.s notTranslated
	move.w d6, d5
	lea OpasmLayoutSectionStartPcs.l, a0
	jsr longTablePtrV1
	move.l (a0), d3
	cmp.l d3, d7
	blo.s next
	lea OpasmLayoutSectionSizes.l, a0
	jsr longTablePtrV1
	move.l (a0), d4
	beq.s next
	move.l d3, d2
	add.l d4, d2
	bcs.s next
	cmp.l d2, d7
	bhs.s next
	lea OpasmLayoutSectionPlacedFlags.l, a0
	jsr wordTablePtrV1
	tst.w (a0)
	beq.s notTranslated
	lea OpasmLayoutSectionBases.l, a0
	jsr longTablePtrV1
	move.l d7, d0
	sub.l d3, d0
	add.l (a0), d0
	moveq #1, d1
	bra.s return
next
	subq.w #1, d6
	bra.s sectionLoop
notTranslated
	move.l d7, d0
	clr.l d1
return
	movem.l (sp)+, d2-d7/a0-a2
	rts
	.bend  ; translatePassOneAddressV1

; Resolve retained section maps and rebase each selected logical unit after
; concrete placement has finalized but before pass-two expression evaluation.
; Outputs: D0.L = 0 on success, 1 for invalid map or reachability state.
finalizeReachableSectionMapsV1	.block
	movem.l d1-d7/a0-a2, -(sp)
	clr.l OpasmLayoutMappedByteCount.l
	moveq #0, d7
resolveMapLoop
	cmp.w OpasmLayoutMapCount.l, d7
	bhs.w reachableBegin
	move.l d7, d2
	lsl.l #5, d2
	lea OpasmLayoutMapLogicalNames.l, a0
	adda.l d2, a0
	lea OpasmLayoutMapLogicalNameLens.l, a1
	moveq #0, d6
	move.w d7, d6
	add.w d6, d6
	moveq #0, d0
	move.w 0(a1, d6.w), d0
	move.l d7, d2
	lsl.l #6, d2
	lea OpasmLayoutMapLogicalOwnerNames.l, a1
	adda.l d2, a1
	lea OpasmLayoutMapLogicalOwnerNameLens.l, a2
	moveq #0, d1
	move.w 0(a2, d6.w), d1
	jsr findSectionByOwnedNameV1
	bne.w logicalMapMissing
	lea OpasmLayoutMapSourceSectionIndices.l, a0
	move.w d5, 0(a0, d6.w)
	move.l d7, d2
	lsl.l #5, d2
	lea OpasmLayoutMapConcreteNames.l, a0
	adda.l d2, a0
	lea OpasmLayoutMapConcreteNameLens.l, a1
	moveq #0, d0
	move.w 0(a1, d6.w), d0
	move.l d7, d2
	lsl.l #6, d2
	lea OpasmLayoutMapConcreteOwnerNames.l, a1
	adda.l d2, a1
	lea OpasmLayoutMapConcreteOwnerNameLens.l, a2
	moveq #0, d1
	move.w 0(a2, d6.w), d1
	jsr findSectionByOwnedNameV1
	bne.w concreteMapMissing
	lea OpasmLayoutMapTargetSectionIndices.l, a0
	move.w d5, 0(a0, d6.w)
	addq.w #1, d7
	bra.w resolveMapLoop

reachableBegin
	moveq #0, d7
reachableLoop
	cmp.w OpasmLayoutReachableLabelCount.l, d7
	bhs.w success
	lea OpasmLayoutReachableLabelIndices.l, a0
	move.w d7, d1
	add.w d1, d1
	moveq #0, d6
	move.w 0(a0, d1.w), d6
	move.w d6, OpasmLayoutCurrentLabelIndex.l
	move.l d6, d0
	jsr eng.opasmEngineGetLabelStatementIndexV1
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d0
	beq.w labelStatementMissing
	move.w d0, OpasmLayoutCurrentStatementIndex.l
	move.w d0, d1
	add.w d1, d1
	lea OpasmLayoutStatementSectionIndices.l, a0
	move.w 0(a0, d1.w), d5
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d5
	beq.w unsectionedReachable
	move.w d5, OpasmLayoutCurrentSourceSection.l
	lea OpasmLayoutSectionLogicalFlags.l, a0
	move.w d5, d1
	add.w d1, d1
	move.w 0(a0, d1.w), d0
	bne.s logicalReachable
	addq.w #1, d7
	bra.w reachableLoop
logicalReachable
	tst.w OpasmLayoutMapCount.l
	beq.w noStructuralMaps
	moveq #0, d4
findTargetLoop
	cmp.w OpasmLayoutMapCount.l, d4
	bhs.w findTargetByAddress
	lea OpasmLayoutMapSourceSectionIndices.l, a0
	move.w d4, d2
	add.w d2, d2
	move.w 0(a0, d2.w), d3
	cmp.w d3, d5
	bne.s nextTargetMap
	bra.w targetFound
nextTargetMap
	addq.w #1, d4
	bra.w findTargetLoop
findTargetByAddress
	move.l d6, d0
	jsr eng.opasmEngineGetLabelValueV1
	move.l d0, d1
	moveq #0, d4
findAddressLoop
	cmp.w OpasmLayoutMapCount.l, d4
	bhs.w labelMapMissing
	move.w d4, d2
	add.w d2, d2
	lea OpasmLayoutMapSourceSectionIndices.l, a0
	moveq #0, d5
	move.w 0(a0, d2.w), d5
	lea OpasmLayoutSectionStartPcs.l, a0
	jsr longTablePtrV1
	cmp.l (a0), d1
	blo.s nextAddressMap
	move.l (a0), d3
	lea OpasmLayoutSectionSizes.l, a0
	jsr longTablePtrV1
	add.l (a0), d3
	cmp.l d3, d1
	blo.s addressTargetFound
nextAddressMap
	addq.w #1, d4
	bra.w findAddressLoop
addressTargetFound
	move.w d4, d2
	add.w d2, d2
targetFound
	lea OpasmLayoutMapTargetSectionIndices.l, a0
	move.w 0(a0, d2.w), d3
	move.w d3, OpasmLayoutCurrentTargetSection.l
	move.w d3, d5
	lea OpasmLayoutSectionBases.l, a0
	jsr longTablePtrV1
	move.l (a0), d1
	lea OpasmLayoutSectionSizes.l, a0
	jsr longTablePtrV1
	add.l (a0), d1
	bcs.w fail
	add.l OpasmLayoutMappedByteCount.l, d1
	bcs.w fail
	move.w OpasmLayoutCurrentTargetSection.l, d5
	lea OpasmLayoutSectionRegionIndices.l, a0
	jsr wordTablePtrV1
	move.w (a0), d5
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d5
	beq.w targetRegionMissing
	lea OpasmLayoutRegionEnds.l, a0
	jsr longTablePtrV1
	cmp.l (a0), d1
	bhi.w targetRegionOverflow
	move.l d1, OpasmLayoutCurrentMappedAddress.l
	move.l d6, d0
	jsr eng.opasmEngineGetLabelValueV1
	move.l d0, OpasmLayoutCurrentOriginalAddress.l
	move.w OpasmLayoutCurrentSourceSection.l, d5
	lea OpasmLayoutSectionStartPcs.l, a0
	jsr longTablePtrV1
	move.l (a0), d3
	lea OpasmLayoutSectionSizes.l, a0
	jsr longTablePtrV1
	add.l (a0), d3
	move.l d3, OpasmLayoutCurrentUnitEndAddress.l
	move.w #OPASM_LAYOUT_INDEX_NONE, OpasmLayoutCurrentUnitEndStatement.l
	jsr eng.opasmEngineGetLabelCountV1
	move.w d0, d6
	moveq #0, d4
nextLabelLoop
	cmp.w d6, d4
	bhs.w nextLabelDone
	move.l d4, d0
	jsr eng.opasmEngineGetLabelStatementIndexV1
	move.w d0, d2
	cmp.w OpasmLayoutCurrentStatementIndex.l, d2
	bls.w nextLabel
	move.w OpasmLayoutCurrentUnitEndStatement.l, d3
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d3
	beq.w checkNextOwner
	cmp.w d3, d2
	bhs.w nextLabel
checkNextOwner
	move.w d2, d3
	add.w d3, d3
	lea OpasmLayoutStatementSectionIndices.l, a0
	move.w 0(a0, d3.w), d3
	cmp.w OpasmLayoutCurrentSourceSection.l, d3
	bne.w nextLabel
	move.w d2, OpasmLayoutCurrentUnitEndStatement.l
	move.l d4, d0
	jsr eng.opasmEngineGetLabelValueV1
	move.l d0, OpasmLayoutCurrentUnitEndAddress.l
nextLabel
	addq.w #1, d4
	bra.w nextLabelLoop
nextLabelDone
	moveq #0, d4
	move.w OpasmLayoutCurrentStatementIndex.l, d4
	moveq #0, d6
	move.w OpasmLayoutCurrentUnitEndStatement.l, d6
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d6
	bne.w markLoop
	move.w #OPASM_LAYOUT_STATEMENT_CAPACITY, d6
markLoop
	cmp.w d6, d4
	bhs.w marked
	lea OpasmLayoutStatementMappedFlags.l, a0
	move.b #1, 0(a0, d4.w)
	addq.w #1, d4
	bra.w markLoop
marked
	move.l OpasmLayoutCurrentUnitEndAddress.l, d0
	sub.l OpasmLayoutCurrentOriginalAddress.l, d0
	bcs.w mappedRangeInvalid
	add.l d0, OpasmLayoutMappedByteCount.l
	; Rust retains the logical pass-one label value for encoded references. The
	; map controls reachability and output routing; it does not relocate the
	; expression-visible symbol into the concrete section's placed address.
	addq.w #1, d7
	bra.w reachableLoop
unsectionedReachable
	; Ordinary selected references participate in expression resolution but do
	; not require a logical-section map.
	addq.w #1, d7
	bra.w reachableLoop
success
	moveq #0, d0
	bra.w return
logicalMapMissing
	moveq #11, d0
	bra.s return
concreteMapMissing
	moveq #12, d0
	bra.s return
labelStatementMissing
	moveq #40, d0
	add.w d6, d0
	bra.s return
labelMapMissing
	moveq #0, d0
	lea OpasmLayoutMapSourceSectionIndices.l, a0
	move.w (a0), d0
	add.w d5, d5
	add.w d5, d0
	addi.w #32, d0
	bra.s return
noStructuralMaps
	moveq #31, d0
	bra.s return
targetRegionMissing
	moveq #24, d0
	bra.s return
targetRegionOverflow
	moveq #25, d0
	bra.s return
mappedRangeInvalid
	moveq #26, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; finalizeReachableSectionMapsV1

; Return whether a label's defining statement is owned by a finalized mapped
; logical unit. Inputs: D0.W = label index. Outputs: D0.L = boolean.
labelIsMappedV1	.block
	jsr eng.opasmEngineGetLabelStatementIndexV1
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d0
	beq.s notMapped
	cmpi.w #OPASM_LAYOUT_STATEMENT_CAPACITY, d0
	bhs.s notMapped
	move.w d0, d1
	lea OpasmLayoutStatementMappedFlags.l, a0
	moveq #0, d0
	move.b 0(a0, d1.w), d0
	rts
notMapped
	moveq #0, d0
	rts
	.bend  ; labelIsMappedV1

; Select pass-two image routing for one statement.
; Inputs: D0.W = statement index. Outputs: D0.L = 0 main, 1 discard, 2 mapped.
statementImageRouteV1	.block
	cmpi.w #OPASM_LAYOUT_STATEMENT_CAPACITY, d0
	bhs.s main
	move.w d0, d1
	lea OpasmLayoutStatementMappedFlags.l, a0
	tst.b 0(a0, d1.w)
	bne.s mapped
	add.w d1, d1
	lea OpasmLayoutStatementSectionIndices.l, a0
	move.w 0(a0, d1.w), d5
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d5
	beq.s main
	lea OpasmLayoutSectionLogicalFlags.l, a0
	add.w d5, d5
	tst.w 0(a0, d5.w)
	beq.s main
	moveq #1, d0
	rts
mapped
	moveq #2, d0
	rts
main
	moveq #0, d0
	rts
	.bend  ; statementImageRouteV1

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

; Find the current scratch section by structural owner and name.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
findScratchOwnedSectionV1	.block
	lea OpasmLayoutScratchName.l, a0
	moveq #0, d0
	move.w OpasmLayoutScratchNameLen, d0
	lea OpasmLayoutScratchSectionOwner.l, a1
	moveq #0, d1
	move.w OpasmLayoutScratchSectionOwnerLen, d1
	jsr findSectionByOwnedNameV1
	rts
	.bend  ; findScratchOwnedSectionV1

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

OpasmLayoutScratchSectionKind
	.res word, 1

OpasmLayoutScratchSectionLogical
	.res word, 1

OpasmLayoutScratchSectionOwnerLen
	.res word, 1

OpasmLayoutScratchSectionOwner
	.res byte, OPASM_LAYOUT_OWNER_CAPACITY

OpasmLayoutScratchMapLogicalOwnerLen
	.res word, 1

OpasmLayoutScratchMapLogicalOwner
	.res byte, OPASM_LAYOUT_OWNER_CAPACITY

OpasmLayoutScratchMapConcreteOwnerLen
	.res word, 1

OpasmLayoutScratchMapConcreteOwner
	.res byte, OPASM_LAYOUT_OWNER_CAPACITY

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

OpasmLayoutSectionOwnerNameLens
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionOwnerNames
	.res byte, OPASM_LAYOUT_SECTION_CAPACITY * OPASM_LAYOUT_OWNER_CAPACITY

OpasmLayoutSectionPlacedFlags
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionKinds
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionLogicalFlags
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionRegionIndices
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

OpasmLayoutMapCount
	.res word, 1

OpasmLayoutMapLogicalNameLens
	.res word, OPASM_LAYOUT_MAP_CAPACITY

OpasmLayoutMapLogicalNames
	.res byte, OPASM_LAYOUT_MAP_CAPACITY * OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutMapLogicalOwnerNameLens
	.res word, OPASM_LAYOUT_MAP_CAPACITY

OpasmLayoutMapLogicalOwnerNames
	.res byte, OPASM_LAYOUT_MAP_CAPACITY * OPASM_LAYOUT_OWNER_CAPACITY

OpasmLayoutMapConcreteNameLens
	.res word, OPASM_LAYOUT_MAP_CAPACITY

OpasmLayoutMapConcreteNames
	.res byte, OPASM_LAYOUT_MAP_CAPACITY * OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutMapConcreteOwnerNameLens
	.res word, OPASM_LAYOUT_MAP_CAPACITY

OpasmLayoutMapConcreteOwnerNames
	.res byte, OPASM_LAYOUT_MAP_CAPACITY * OPASM_LAYOUT_OWNER_CAPACITY

OpasmLayoutMapSourceSectionIndices
	.res word, OPASM_LAYOUT_MAP_CAPACITY

OpasmLayoutMapTargetSectionIndices
	.res word, OPASM_LAYOUT_MAP_CAPACITY

OpasmLayoutStatementSectionIndices
	.res word, OPASM_LAYOUT_STATEMENT_CAPACITY

OpasmLayoutStatementMappedFlags
	.res byte, OPASM_LAYOUT_STATEMENT_CAPACITY

OpasmLayoutReachableLabelCount
	.res word, 1

OpasmLayoutReachableLabelIndices
	.res word, OPASM_LAYOUT_REACHABLE_CAPACITY

OpasmLayoutMappedByteCount
	.res long, 1

OpasmLayoutCurrentLabelIndex
	.res word, 1

OpasmLayoutCurrentStatementIndex
	.res word, 1

OpasmLayoutCurrentSourceSection
	.res word, 1

OpasmLayoutCurrentTargetSection
	.res word, 1

OpasmLayoutCurrentUnitEndStatement
	.res word, 1

OpasmLayoutCurrentMappedAddress
	.res long, 1

OpasmLayoutCurrentOriginalAddress
	.res long, 1

OpasmLayoutCurrentUnitEndAddress
	.res long, 1

	.endsection
	.endmodule
