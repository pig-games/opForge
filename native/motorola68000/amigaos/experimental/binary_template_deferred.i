; Binding recipes is a preparation responsibility, completed before invocation.
.if DEFER_MACRO_PLANS
	.priv
; A6=templates,A0=Def. D0/CCR=status; all other registers preserved.
; Numeric body tokens already capture declaration scope. Resolve every retained
; plan once, then publish BodyReady. No caller scope or original text is read.
bindBody .block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a0, a4
	cmpi.w #KIND_MACRO, Def.Kind(a4)
	bne.w ready
	tst.l Def.BodyReady(a4)
	bne.w ready
	move.l Def.First(a4), d6
next
	cmp.l Def.Last(a4), d6
	beq.w complete
	bhi.w bad
	movea.l BODY+memory.Block.Pointer(a6), a2
	adda.l d6, a2
	moveq #0, d5
	move.b source.RecordHeader.Length(a2), d5
	addq.l #1, d5
	move.l d6, d7
	add.l d5, d7
	bcs.w bad
	cmp.l Def.Last(a4), d7
	bhi.w bad
	movea.l a2, a0
	move.l d5, d0
	bsr.w validateBodyPlan
	bne.w bad
	moveq #0, d0
	move.b source.RecordHeader.Flags(a2), d0
	andi.w #source.FLAG_PLAN+LINE_PLAN_FLAG, d0
	beq.w advance
	movea.l a2, a3
	adda.l d5, a3
	suba.w #source.PLAN_TOKEN_BYTES, a3
	move.l source.PlanToken.Handle(a3), d1
	btst #31, d1
	beq.w advance
	move.l State.BindPlan(a6), d0
	beq.w bad
	movea.l d0, a1
	movea.l State.ParserContext(a6), a0
	jsr (a1)
	tst.l d0
	bne.w bad
	move.l d1, source.PlanToken.Handle(a3)
advance
	move.l d7, d6
	bra.w next
complete
	move.l #1, Def.BodyReady(a4)
ready
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend ; bindBody
.endif
