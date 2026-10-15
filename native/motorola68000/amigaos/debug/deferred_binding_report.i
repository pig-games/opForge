; Existing bounded progress ABI; no report code/import exists in release mode.
DEFERRED_BINDING_REPORT	.macro dosbase, session
.if DEFER_MACRO_PLANS
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d5/a0, -(sp)
	lea .session, a0
	jsr frontend.deferredStats
	.MEMORY_PROGRESS .dosbase, #3300, d1, d2, d3
	.MEMORY_PROGRESS .dosbase, #3301, d4, d5, #0
.ifdef OPFORGE_DEFERRED_MACRO_CANDIDATES
	lea .session, a0
	jsr frontend.acquisitionStats
	.MEMORY_PROGRESS .dosbase, #3302, d1, d2, #0
.endif
	movem.l (sp)+, d0-d5/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro
