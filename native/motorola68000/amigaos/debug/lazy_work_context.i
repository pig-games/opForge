; Frontend-only observer adapter for the existing template state.
; Caller provides its scratch base and named template-state offset.
; Observe actual definition/expansion flags; never infer reachability.
WORK_TEMPLATE_CONTEXT .macro scratch, offset
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0-d2/a0, -(sp)
	movea.l .scratch, a0
	adda.l #.offset, a0
	moveq #lazy_work.CONTEXT_DEFINITION, d0
	tst.w templates.State.Open(a0)
	bne.w selected
	tst.w templates.State.ExternalOpen(a0)
	bne.w selected
	moveq #lazy_work.CONTEXT_EXPANSION, d0
	tst.w templates.State.Depth(a0)
	bne.w selected
	moveq #lazy_work.CONTEXT_ORDINARY, d0
selected
	jsr lazy_work.contextSet
	moveq #0, d0
	moveq #0, d1
	move.w templates.State.Open(a0), d0
	bne.w owner
	move.w templates.State.ExternalOpen(a0), d0
	moveq #1, d1
owner
	move.l templates.State.AuditSession(a0), d2
	jsr lazy_work.templateOwner
	movem.l (sp)+, d0-d2/a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Frontend owns source suspension. The template cursor's Floor may be stale
; while a physical child source is being prepared.
WORK_TEMPLATE_FLOOR .macro scratch, offset, floor
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	moveq #0, d0
	move.w .floor, d0
	movea.l .scratch, a0
	adda.l #.offset, a0
	move.l d0, templates.State.AuditFloor(a0)
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

WORK_ORIGIN_PATHS .macro ; app-owned paths, reported before owner release
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	bsr.w workOriginPaths
.endif
.endif
.endif
.endif
.endif
.endmacro
