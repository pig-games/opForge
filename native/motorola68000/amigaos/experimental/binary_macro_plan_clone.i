	.pub
; A0=source Block,D1=handle,A1=disjoint destination Block. D0/CCR=status;
; D1=new handle or zero on failure; other registers preserved. Copy one complete
; current plan, recipes and spelling without retaining pointers or rewriting IDs.
; Both blocks are valid owners. Destination capacity may grow on failure, but
; Used changes only after validation, allocation, copying and offset rebasing.
clone	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	bsr.w cloneOwners
	bne.w bad
	move.l d1, d3
	beq.w bad
	subq.l #1, d3
	btst #0, d3
	bne.w bad
	movea.l a6, a0
	bsr.w resolve
	bne.w bad
	movea.l a1, a4
	move.l Plan.Bytes(a4), d6
	btst #0, d6
	bne.w bad
	move.l d6, d5
	sub.l d3, d5
	bsr.w cloneGeometry
	bne.w bad
	move.l memory.Block.Used(a5), d4
	btst #0, d4
	bne.w bad
	move.l d4, d7
	add.l d5, d7
	bcs.w bad
	cmpi.l #ARENA_LIMIT, d7
	bhi.w bad
	movea.l a5, a0
	move.l d7, d0
	move.l #ARENA_LIMIT, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l a4, a0
	movea.l memory.Block.Pointer(a5), a1
	adda.l d4, a1
	move.l d5, d0
	lsr.l #2, d0
	beq.w tail
copyLong
	move.l (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyLong
tail
	move.l d5, d0
	andi.l #3, d0
	beq.w copied
copyByte
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyByte
copied
	movea.l memory.Block.Pointer(a5), a2
	lea 0(a2, d4.l), a0
	move.l d7, Plan.Bytes(a0)
	move.l Plan.Count(a0), d2
	lea HEADER_BYTES(a0), a3
rows
	bsr.w cloneSpelling
	adda.w #ROW_BYTES, a3
	subq.l #1, d2
	bne.w rows
	move.l Plan.Recipes(a0), d0
	beq.w publish
	sub.l d3, d0
	add.l d4, d0
	move.l d0, Plan.Recipes(a0)
	lea 0(a2, d0.l), a3
	move.l RecipeSet.Count(a3), d2
	adda.w #RecipeSet.Rows, a3
recipes
	tst.l d2
	beq.w publish
	bsr.w cloneSpelling
	cmpi.w #abi.PRVM_RESULT_MACRO_NAMED, Row.Kind(a3)
	bne.w nextRecipe
	move.l Row.Aux0(a3), d0
	sub.l d3, d0
	add.l d4, d0
	move.l d0, Row.Aux0(a3)
	move.l Row.Aux1(a3), d0
	sub.l d3, d0
	add.l d4, d0
	move.l d0, Row.Aux1(a3)
nextRecipe
	adda.w #ROW_BYTES, a3
	subq.l #1, d2
	bra.w recipes
publish
	move.l d7, memory.Block.Used(a5)
	move.l d4, d1
	addq.l #1, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; clone
	.priv
; A6/A5=source/destination owners. D0/CCR=status; D2/A0-A1 clobbered.
; Reject overlapping allocation extents before destination growth can free one.
cloneOwners	.block
	move.l a6, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	cmpa.l a5, a6
	beq.w bad
	move.l a5, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l memory.Block.Used(a6), d0
	cmp.l memory.Block.Capacity(a6), d0
	bhi.w bad
	move.l memory.Block.Used(a5), d0
	cmp.l memory.Block.Capacity(a5), d0
	bhi.w bad
	move.l memory.Block.Pointer(a6), d0
	beq.w bad
	btst #0, d0
	bne.w bad
	movea.l d0, a0
	add.l memory.Block.Capacity(a6), d0
	bcs.w bad
	movea.l d0, a1
	move.l memory.Block.Pointer(a5), d0
	bne.w allocated
	tst.l memory.Block.Capacity(a5)
	bne.w bad
	bra.w good
allocated
	btst #0, d0
	bne.w bad
	move.l d0, d2
	add.l memory.Block.Capacity(a5), d2
	bcs.w bad
	cmp.l a1, d0
	bhs.w good
	cmp.l a0, d2
	bls.w good
bad
	moveq #1, d0
	rts
good
	moveq #0, d0
	rts
	.bend  ; cloneOwners

; A4=resolved source plan,A6=source owner,D3=start,D6=end. D0/CCR=status;
; other registers preserved. Current constructors place rows, optional recipes,
; then a single spelling pool and at most one alignment byte in this order.
cloneGeometry	.block
	movem.l d1-d2/d5/d7/a0-a3, -(sp)
	move.l Plan.Count(a4), d1
	move.l d1, d2
	lsl.l #5, d2
	addi.l #HEADER_BYTES, d2
	add.l d3, d2
	bcs.w bad
	lea HEADER_BYTES(a4), a3
	move.l Row.SpellingStart(a3), d5
	move.l Row.SpellingEnd(a3), d7
	cmp.l d5, d7
	blo.w bad
	cmp.l d6, d7
	bhi.w bad
	move.l d6, d0
	sub.l d7, d0
	cmpi.l #1, d0
	bhi.w bad
	move.l Plan.Recipes(a4), d0
	beq.w noRecipes
	cmp.l d2, d0
	bne.w bad
	addq.l #RecipeSet.Rows, d2
	bcs.w bad
	cmp.l d5, d2
	bhi.w bad
	movea.l memory.Block.Pointer(a6), a0
	adda.l d0, a0
	move.l RecipeSet.Count(a0), d0
	cmpi.l #MAX_ROWS, d0
	bhi.w bad
	lsl.l #5, d0
	add.l d0, d2
	bcs.w bad
noRecipes
	cmp.l d5, d2
	bne.w bad
row
	cmpi.w #abi.PRVM_RESULT_MACRO_LINE, Row.Kind(a3)
	blo.w bad
	cmpi.w #abi.PRVM_RESULT_MACRO_LABEL, Row.Kind(a3)
	bhi.w bad
	move.l Row.PackedStart(a3), d0
	cmpi.l #256, d0
	bhi.w bad
	move.l Row.PackedEnd(a3), d2
	cmp.l d0, d2
	blo.w bad
	cmpi.l #256, d2
	bhi.w bad
	move.l Row.SpellingStart(a3), d0
	cmp.l d5, d0
	blo.w bad
	move.l Row.SpellingEnd(a3), d2
	cmp.l d0, d2
	blo.w bad
	cmp.l d7, d2
	bhi.w bad
	cmpi.w #abi.PRVM_RESULT_MACRO_FORMAL, Row.Kind(a3)
	beq.w formal
	cmpi.w #abi.PRVM_RESULT_MACRO_LINE, Row.Kind(a3)
	bne.w nextRow
	move.l Row.Aux2(a3), d0
	bra.w packedAuxiliary
formal
	move.l Row.Aux0(a3), d0
packedAuxiliary
	cmpi.l #-1, d0
	beq.w nextRow
	cmpi.l #256, d0
	bhi.w bad
nextRow
	adda.w #ROW_BYTES, a3
	subq.l #1, d1
	bne.w row
	move.l Plan.Recipes(a4), d0
	beq.w good
	movea.l memory.Block.Pointer(a6), a3
	adda.l d0, a3
	move.l RecipeSet.Count(a3), d1
	adda.w #RecipeSet.Rows, a3
	move.l d5, d2
recipe
	tst.l d1
	beq.w complete
	cmp.l Row.SpellingStart(a3), d2
	bne.w bad
	move.l Row.SpellingEnd(a3), d2
	cmp.l Row.SpellingStart(a3), d2
	bls.w bad
	cmp.l d7, d2
	bhi.w bad
	cmpi.w #abi.PRVM_RESULT_MACRO_LITERAL, Row.Kind(a3)
	blo.w bad
	cmpi.w #abi.PRVM_RESULT_MACRO_SUPPLIED_LIST, Row.Kind(a3)
	bhi.w bad
	cmpi.w #abi.PRVM_RESULT_MACRO_NAMED, Row.Kind(a3)
	bne.w nextRecipe
	move.l Row.Aux0(a3), d0
	cmp.l Row.SpellingStart(a3), d0
	blo.w bad
	move.l Row.Aux1(a3), d5
	cmp.l d0, d5
	bls.w bad
	cmp.l d2, d5
	bhi.w bad
nextRecipe
	adda.w #ROW_BYTES, a3
	subq.l #1, d1
	bra.w recipe
complete
	cmp.l d7, d2
	bne.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/d5/d7/a0-a3
	tst.l d0
	rts
	.bend  ; cloneGeometry

; A3=row,D3=old plan start,D4=new start. D0 clobbered; other registers kept.
; Geometry preflight proves both rebased offsets lie inside the reserved plan.
cloneSpelling	.block
	move.l Row.SpellingStart(a3), d0
	sub.l d3, d0
	add.l d4, d0
	move.l d0, Row.SpellingStart(a3)
	move.l Row.SpellingEnd(a3), d0
	sub.l d3, d0
	add.l d4, d0
	move.l d0, Row.SpellingEnd(a3)
	rts
	.bend  ; cloneSpelling
