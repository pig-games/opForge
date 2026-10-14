; Owner-side scalar snapshots. No original strings or borrowed pointers persist.
; Invocations are counted at queueCall, never lookup, preview or next-record reads.
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	.priv
; A0=template state,A1=published Def,D0=pool,D1=header line. Passive.
auditDefine .block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a4, -(sp)
	suba.w #lazy_work.TEMPLATE_DEFINE_PACKET_BYTES, sp
	movea.l sp, a4
	movea.l a0, a3
	movea.l a1, a2
	move.l State.AuditSession(a3), lazy_work.TemplateDefinePacket.Session(a4)
	move.l d0, lazy_work.TemplateDefinePacket.Pool(a4)
	move.l d1, lazy_work.TemplateDefinePacket.Line(a4)
	moveq #0, d1
	tst.l d0
	bne.w external
	move.w State.Open(a3), d1
	bra.w selected
external
	move.w State.ExternalOpen(a3), d1
selected
	move.l d1, lazy_work.TemplateDefinePacket.Handle(a4)
	moveq #0, d1
	move.w Def.Kind(a2), d1
	move.l d1, lazy_work.TemplateDefinePacket.Kind(a4)
	moveq #0, d1
	move.w Def.Name(a2), d1
	move.l d1, lazy_work.TemplateDefinePacket.Name(a4)
	move.l Def.Origin(a2), lazy_work.TemplateDefinePacket.Origin(a4)
	move.l Def.HeaderPlan(a2), lazy_work.TemplateDefinePacket.HeaderPlan(a4)
	move.l Def.First(a2), lazy_work.TemplateDefinePacket.First(a4)
	move.l #DEF_BYTES, lazy_work.TemplateDefinePacket.DefBytes(a4)
	moveq #0, d6
	moveq #0, d7
	moveq #0, d2
	moveq #PARAM_LIMIT-1, d3
defaults
	move.l Def.DefaultEnd0(a2, d2.w), d4
	sub.l Def.DefaultStart0(a2, d2.w), d4
	add.l d4, d6
	lea Def.TextEnd0(a2), a0
	move.l 0(a0, d2.w), d4
	sub.l Def.TextStart0(a2, d2.w), d4
	add.l d4, d7
	addq.w #4, d2
	dbra d3, defaults
	move.l d6, lazy_work.TemplateDefinePacket.DefaultBytes(a4)
	move.l d7, lazy_work.TemplateDefinePacket.TextBytes(a4)
	movea.l a4, a0
	jsr lazy_work.templateDefine
	adda.w #lazy_work.TEMPLATE_DEFINE_PACKET_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a4
	move.w (sp)+, ccr
	rts
	.bend ; auditDefine

; A0=template state,A1=accepted new frame. Passive, after Depth publication.
; A suspended parent at Floor is not the child include's consumer.
auditCall .block
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a3, -(sp)
	suba.w #lazy_work.TEMPLATE_CALL_PACKET_BYTES, sp
	movea.l sp, a3
	movea.l a0, a2
	move.l State.AuditSession(a2), lazy_work.TemplateCallPacket.Session(a3)
	moveq #0, d0
	move.w CallFrame.Definition(a1), d0
	addq.l #1, d0
	move.l d0, lazy_work.TemplateCallPacket.Handle(a3)
	moveq #0, d0
	move.w CallFrame.External(a1), d0
	move.l d0, lazy_work.TemplateCallPacket.Pool(a3)
	move.l State.Origin(a2), lazy_work.TemplateCallPacket.Origin(a3)
	moveq #0, d0
	move.w CallFrame.CallLine(a1), d0
	move.l d0, lazy_work.TemplateCallPacket.Line(a3)
	moveq #0, d0
	move.w CallFrame.DefaultMask(a1), d0
	move.l d0, lazy_work.TemplateCallPacket.DefaultMask(a3)
	clr.l lazy_work.TemplateCallPacket.ParentHandle(a3)
	clr.l lazy_work.TemplateCallPacket.ParentPool(a3)
	moveq #0, d0
	move.w State.Depth(a2), d0
	subq.w #1, d0 ; number of previously queued frames
	cmp.l State.AuditFloor(a2), d0
	bls.w parentDone
	subq.w #1, d0
	mulu.w #FRAME_BYTES, d0
	lea FRAMES(a2), a0
	adda.l d0, a0
	moveq #0, d0
	move.w CallFrame.Definition(a0), d0
	addq.l #1, d0
	move.l d0, lazy_work.TemplateCallPacket.ParentHandle(a3)
	moveq #0, d0
	move.w CallFrame.External(a0), d0
	move.l d0, lazy_work.TemplateCallPacket.ParentPool(a3)
parentDone
	movea.l a3, a0
	jsr lazy_work.templateCall
	adda.w #lazy_work.TEMPLATE_CALL_PACKET_BYTES, sp
	movem.l (sp)+, d0-d3/a0-a3
	move.w (sp)+, ccr
	rts
	.bend ; auditCall

; A0=live template state, before finish releases any pool. Passive.
; Snapshot Used, not reserved capacity; plan arena ownership stays unattributed.
auditPools .block
	move.w ccr, -(sp)
	movem.l d0/a0-a2, -(sp)
	suba.w #lazy_work.TEMPLATE_POOLS_PACKET_BYTES, sp
	movea.l sp, a2
	movea.l a0, a1
	move.l State.AuditSession(a1), lazy_work.TemplatePoolsPacket.Session(a2)
	move.l DEFS+memory.Block.Used(a1), lazy_work.TemplatePoolsPacket.InternalBytes(a2)
	move.l EXTERNALS+memory.Block.Used(a1), lazy_work.TemplatePoolsPacket.ExternalBytes(a2)
	move.l DEFAULT_USED(a1), lazy_work.TemplatePoolsPacket.DefaultBytes(a2)
	move.l DEFAULT_TEXT_USED(a1), lazy_work.TemplatePoolsPacket.TextBytes(a2)
	move.l BODY+memory.Block.Used(a1), lazy_work.TemplatePoolsPacket.BodyBytes(a2)
	clr.l lazy_work.TemplatePoolsPacket.PlanBytes(a2)
	move.l State.Plans(a1), d0
	beq.w noPlans
	movea.l d0, a0
	move.l memory.Block.Used(a0), lazy_work.TemplatePoolsPacket.PlanBytes(a2)
noPlans
	movea.l a2, a0
	jsr lazy_work.templatePools
	adda.w #lazy_work.TEMPLATE_POOLS_PACKET_BYTES, sp
	movem.l (sp)+, d0/a0-a2
	move.w (sp)+, ccr
	rts
	.bend ; auditPools
.endif
.endif
