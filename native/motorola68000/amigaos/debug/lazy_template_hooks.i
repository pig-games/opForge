; Passive template-owner adapters. Both audit flags gate code and imports.
; State/descriptor/frame operands are address registers, never SP-relative.
WORK_TEMPLATE_BEGIN .macro state
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .state, a0
	clr.l State.AuditFloor(a0)
	adda.w #State.AuditSession, a0
	jsr lazy_work.templateBegin
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_TEMPLATE_DEFINE .macro state, descriptor, pool, line
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0-d1/a0-a1, -(sp)
	move.l .state, -(sp)
	move.l .descriptor, -(sp)
	move.l .pool, d0
	moveq #0, d1
	move.w .line, d1
	movea.l (sp)+, a1
	movea.l (sp)+, a0
	bsr.w auditDefine
	movem.l (sp)+, d0-d1/a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_TEMPLATE_CLOSE .macro state, descriptor, pool
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0-d3/a0-a1, -(sp)
	move.l .state, -(sp)
	move.l .descriptor, -(sp)
	move.l .pool, d1
	movea.l (sp)+, a1
	movea.l (sp)+, a0
	moveq #0, d0
	tst.l d1
	bne.w external
	move.w State.Open(a0), d0
	bra.w selected
external
	move.w State.ExternalOpen(a0), d0
selected
	move.l Def.Last(a1), d2
	move.l State.AuditSession(a0), d3
	jsr lazy_work.templateClose
	movem.l (sp)+, d0-d3/a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_TEMPLATE_CALL .macro state, frame
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l a0-a1, -(sp)
	move.l .state, -(sp)
	move.l .frame, -(sp)
	movea.l (sp)+, a1
	movea.l (sp)+, a0
	bsr.w auditCall
	movem.l (sp)+, a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_TEMPLATE_POOLS .macro state
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .state, a0
	bsr.w auditPools
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
; Statement owner supplies its header line after external publication.
WORK_TEMPLATE_LINE .macro state, handle, pool, line
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	moveq #0, d0
	move.w .handle, d0
	move.l .pool, d1
	moveq #0, d2
	move.w .line, d2
	movea.l .state, a0
	move.l templates.State.AuditSession(a0), d3
	jsr lazy_work.templateLine
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
