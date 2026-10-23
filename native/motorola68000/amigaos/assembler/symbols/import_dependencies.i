; Preserve dependency ordering after a module's source imports have retired.
; This adds no spelling exposure: references in its compiled image are already
; canonical numeric identities. Ordinary imports still own their own aliases.
	.pub
; A0=scope,D0=owning module lexical index+1,D1=target lexical index0.
; D0/CCR=status; all other registers preserved. Duplicate targets are idempotent.
; A malformed link or exhausted table rejects before publishing a new row.
seedDependency	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d6
	beq.w bad
	moveq #0, d2
	move.w layout.State.Count(a6), d2
	cmp.l d2, d6
	bhi.w bad
	cmp.l d2, d1
	bhs.w bad
	move.l d1, d5
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a6), a0
	cmpa.w #0, a0
	beq.w bad
	move.l d6, d0
	subq.l #1, d0
	add.l d0, d0
	move.w 0(a0, d0.l), d2
	andi.w #modules.MODULE_ID, d2
	beq.w bad
	move.l d5, d0
	add.l d0, d0
	move.w 0(a0, d0.l), d2
	andi.w #modules.MODULE_ID, d2
	beq.w bad
	lea layout.IMPORT_STATE(a6), a4
	move.l d6, d0
	subq.l #1, d0
	add.l d0, d0
	movea.l HEADS_POINTER(a4), a3
	adda.l d0, a3
	moveq #0, d3
	move.w (a3), d3
	moveq #0, d7
	moveq #0, d2
	move.w COUNT(a4), d2
next
	tst.l d3
	beq.w append
	cmp.l d2, d3
	bhi.w bad
	addq.l #1, d7
	cmp.l d2, d7
	bhi.w bad
	move.l d3, d0
	subq.l #1, d0
	mulu.w #ITEM_BYTES, d0
	movea.l ITEMS_POINTER(a4), a0
	adda.l d0, a0
	cmp.w Item.Target(a0), d5
	beq.w good
	moveq #0, d3
	move.w Item.Next(a0), d3
	bra.w next
append
	moveq #0, d7
	move.w COUNT(a4), d7
	cmpi.l #RECORD_LIMIT, d7
	bhs.w bad
	move.l d7, d0
	addq.l #1, d0
	movea.l a4, a0
	bsr.w reserveItems
	bne.w bad
	move.l d7, d0
	mulu.w #ITEM_BYTES, d0
	movea.l ITEMS_POINTER(a4), a0
	adda.l d0, a0
	move.w d5, Item.Target(a0)
	move.w d5, Item.Qualifier(a0)
	clr.w Item.Selected(a0)
	clr.w Item.Unqualified(a0)
	move.w (a3), Item.Next(a0)
	clr.w Item.Flags(a0)
	addi.l #ITEM_BYTES, d0
	move.l d0, ITEMS+memory.Block.Used(a4)
	addq.l #1, d7
	move.w d7, (a3)
	move.w d7, COUNT(a4)
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seedDependency
