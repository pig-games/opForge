; Passive acquisition observer. All code/imports disappear without all gates.
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_LAZY_CANDIDATES
	.use opforge.debug.lazy_work as candidate_audit
.endif
.endif
.endif
CANDIDATE_BEGIN	.macro
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_LAZY_CANDIDATES
	jsr candidate_audit.candidateBegin
.endif
.endif
.endif
.endmacro
CANDIDATE_END	.macro request
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_LAZY_CANDIDATES
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .request, a0
	jsr candidate_audit.candidateEnd
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
CANDIDATE_CREATED	.macro request
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_LAZY_CANDIDATES
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .request, a0
	jsr candidate_audit.candidateCreated
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
; Observe the definition before replay changes its open state. A4 is the fresh
; capture record, A6 frontend scratch. No source text or retained pointer.
CANDIDATE_OWNER	.macro
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_LAZY_CANDIDATES
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	moveq #0, d0
	move.w TEMPLATE_STATE+templates.State.Open(a6), d0
	moveq #0, d1
	move.l TEMPLATE_STATE+templates.State.AuditSession(a6), d3
	movea.l a4, a0
	jsr candidate_audit.candidateOwner
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
