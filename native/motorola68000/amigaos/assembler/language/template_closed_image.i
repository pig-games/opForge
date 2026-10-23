; Canonical layout and bounds for one closed template capsule.
	.priv
closedBuild	.block
	movem.l d1-d7/a0-a5, -(sp)
	move.l #CLOSED_HEADER_BYTES+DEF_BYTES, d6
	lea ClosedWork.Body(a6), a2
	moveq #4-1, d7
measure
	addq.l #3, d6
	bcs.w bad
	andi.l #$fffffffc, d6
	add.l memory.Block.Used(a2), d6
	bcs.w bad
	adda.w #BLOCK_BYTES, a2
	dbra d7, measure
	addq.l #3, d6
	bcs.w bad
	andi.l #$fffffffc, d6
	move.l ClosedWork.Map+memory.Block.Used(a6), d0
	lsr.l #1, d0  ; two-long map rows publish one-long root handles
	add.l d0, d6
	bcs.w bad
	cmpi.l #memory.LIMIT, d6
	bhi.w bad
	lea ClosedWork.Image(a6), a0
	move.l d6, d0
	jsr memory.reserveExact
	bne.w bad
	move.l d6, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a5
	movea.l a5, a0
	move.l d6, d0
clear
	clr.b (a0)+
	subq.l #1, d0
	bne.w clear
	move.l #CLOSED_MAGIC, ClosedHeader.Magic(a5)
	move.l d6, ClosedHeader.Bytes(a5)
	move.l #CLOSED_HEADER_BYTES, ClosedHeader.Definition(a5)
	lea ClosedWork.Definition(a6), a0
	lea CLOSED_HEADER_BYTES(a5), a1
	move.l #DEF_BYTES, d0
	bsr.w closedCopy
	move.l #CLOSED_HEADER_BYTES+DEF_BYTES, d5
	lea ClosedWork.Body(a6), a2
	lea ClosedHeader.Body(a5), a3
	moveq #4-1, d7
section
	addq.l #3, d5
	andi.l #$fffffffc, d5
	move.l d5, (a3)+
	move.l memory.Block.Used(a2), d0
	move.l d0, (a3)+
	movea.l memory.Block.Pointer(a2), a0
	movea.l a5, a1
	adda.l d5, a1
	bsr.w closedCopy
	add.l d0, d5
	adda.w #BLOCK_BYTES, a2
	dbra d7, section
	addq.l #3, d5
	andi.l #$fffffffc, d5
	move.l d5, ClosedHeader.Roots(a5)
	move.l ClosedWork.Map+memory.Block.Used(a6), d7
	lsr.l #3, d7
	move.l d7, ClosedHeader.RootCount(a5)
	movea.l ClosedWork.Map+memory.Block.Pointer(a6), a0
	movea.l a5, a1
	adda.l d5, a1
root
	tst.l d7
	beq.w validate
	move.l ClosedMap.Target(a0), (a1)+
	adda.w #CLOSED_MAP_BYTES, a0
	subq.l #1, d7
	bra.w root
validate
	movea.l a5, a0
	move.l d6, d0
	bsr.w validateClosed
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; closedBuild

	.pub
; A0=capsule,D0=readable bytes. D0/CCR=status; others preserved.
; Validate canonical pool extents, descriptor spans and complete body records.
; Plan cloning validates the plan's spelling/recipe internals before use.
validateClosed	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #BLOCK_BYTES, sp
	movea.l a0, a6
	move.l d0, d6
	cmpi.l #CLOSED_HEADER_BYTES+DEF_BYTES, d6
	blo.w bad
	cmpi.l #memory.LIMIT, d6
	bhi.w bad
	move.l a6, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	add.l d6, d0
	bcs.w bad
	cmpi.l #CLOSED_MAGIC, ClosedHeader.Magic(a6)
	bne.w bad
	cmp.l ClosedHeader.Bytes(a6), d6
	bne.w bad
	cmpi.l #CLOSED_HEADER_BYTES, ClosedHeader.Definition(a6)
	bne.w bad
	lea CLOSED_HEADER_BYTES(a6), a5
	cmpi.w #KIND_EXTERNAL, Def.Kind(a5)
	bhi.w bad
	cmpi.w #PARAM_LIMIT, Def.ParamCount(a5)
	bhi.w bad
	tst.l Def.Flags(a5)
	bne.w bad
	tst.l Def.First(a5)
	bne.w bad
	move.l Def.Last(a5), d0
	cmp.l ClosedHeader.BodyBytes(a6), d0
	bne.w bad
	move.l #CLOSED_HEADER_BYTES+DEF_BYTES, d5
	lea ClosedHeader.Body(a6), a2
	moveq #4-1, d7
section
	addq.l #3, d5
	bcs.w bad
	andi.l #$fffffffc, d5
	cmp.l (a2)+, d5
	bne.w bad
	add.l (a2)+, d5
	bcs.w bad
	cmp.l d6, d5
	bhi.w bad
	dbra d7, section
	addq.l #3, d5
	bcs.w bad
	andi.l #$fffffffc, d5
	cmp.l ClosedHeader.Roots(a6), d5
	bne.w bad
	move.l ClosedHeader.RootCount(a6), d0
	cmpi.l #memory.LIMIT/4, d0
	bhi.w bad
	lsl.l #2, d0
	add.l d5, d0
	bcs.w bad
	cmp.l d6, d0
	bne.w bad
	moveq #0, d7
parameter
	cmp.w Def.ParamCount(a5), d7
	bhs.w plans
	move.l d7, d0
	lsl.w #2, d0
	move.l Def.DefaultStart0(a5, d0.w), d1
	move.l Def.DefaultEnd0(a5, d0.w), d2
	sub.l d1, d2
	bcs.w bad
	cmpi.l #ARG_LIMIT, d2
	bhi.w bad
	move.l Def.DefaultEnd0(a5, d0.w), d1
	cmp.l ClosedHeader.DefaultBytes(a6), d1
	bhi.w bad
	move.l Def.TextStart0(a5, d0.w), d1
	lea Def.TextEnd0(a5), a0
	move.l 0(a0, d0.w), d2
	sub.l d1, d2
	bcs.w bad
	cmpi.l #TEXT_LIMIT, d2
	bhi.w bad
	move.l 0(a0, d0.w), d1
	cmp.l ClosedHeader.TextBytes(a6), d1
	bhi.w bad
	addq.w #1, d7
	bra.w parameter
plans
	movea.l a6, a0
	adda.l ClosedHeader.Plans(a6), a0
	move.l a0, memory.Block.Pointer(sp)
	clr.l memory.Block.Capacity(sp)
	move.l ClosedHeader.PlanBytes(a6), memory.Block.Used(sp)
	move.l Def.HeaderPlan(a5), d1
	beq.w roots
	movea.l sp, a0
	jsr plans.resolve
	bne.w bad
roots
	movea.l a6, a4
	adda.l ClosedHeader.Roots(a6), a4
	move.l ClosedHeader.RootCount(a6), d7
	moveq #0, d5
root
	tst.l d7
	beq.w body
	move.l (a4)+, d1
	cmp.l d5, d1
	bls.w bad
	move.l d1, d5
	movea.l sp, a0
	jsr plans.resolve
	bne.w bad
	subq.l #1, d7
	bra.w root
body
	movea.l a6, a4
	adda.l ClosedHeader.Body(a6), a4
	movea.l a4, a5
	adda.l ClosedHeader.BodyBytes(a6), a5
record
	cmpa.l a5, a4
	beq.w good
	bhi.w bad
	move.l a5, d0
	sub.l a4, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w bad
	moveq #0, d7
	move.b source.RecordHeader.Length(a4), d7
	addq.l #1, d7
	cmp.l d0, d7
	bhi.w bad
	movea.l a4, a0
	move.l d7, d0
	bsr.w validateBodyPlan
	bne.w bad
	moveq #0, d0
	move.b source.RecordHeader.Flags(a4), d0
	andi.w #source.FLAG_PLAN+LINE_PLAN_FLAG, d0
	beq.w advance
	movea.l a4, a3
	adda.l d7, a3
	suba.w #source.PLAN_TOKEN_BYTES, a3
	move.l source.PlanToken.Handle(a3), d1
	movea.l sp, a0
	jsr plans.resolve
	bne.w bad
advance
	adda.l d7, a4
	bra.w record
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	adda.w #BLOCK_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validateClosed
	.priv
