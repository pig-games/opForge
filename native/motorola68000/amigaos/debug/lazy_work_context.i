; Frontend-only observer adapter for the existing template state.
; Caller provides its scratch base and named template-state offset.
; Observe actual definition/expansion flags; never infer reachability.
WORK_TEMPLATE_CONTEXT .macro scratch, offset
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
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
