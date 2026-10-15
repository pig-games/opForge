; Existing bounded progress ABI; no report code/import exists in release mode.
; Keep summaries outside the per-definition inventory phase range.
DEFERRED_REPORT_BINDING = 450000
DEFERRED_REPORT_STORAGE = 450001
DEFERRED_REPORT_ACQUISITION = 450002
DEFERRED_REPORT_LINES = 450003
DEFERRED_BINDING_REPORT	.macro dosbase, session
.if DEFER_MACRO_PLANS
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d5/a0, -(sp)
	lea .session, a0
	jsr frontend.deferredStats
	.MEMORY_PROGRESS .dosbase, #DEFERRED_REPORT_BINDING, d1, d2, d3
	.MEMORY_PROGRESS .dosbase, #DEFERRED_REPORT_STORAGE, d4, d5, #0
.ifdef OPFORGE_DEFERRED_MACRO_CANDIDATES
	lea .session, a0
	jsr frontend.acquisitionStats
	.MEMORY_PROGRESS .dosbase, #DEFERRED_REPORT_ACQUISITION, d1, d2, #0
.endif
.ifdef OPFORGE_DEFERRED_MACRO_LINES
	lea .session, a0
	jsr frontend.lineStats
	.MEMORY_PROGRESS .dosbase, #DEFERRED_REPORT_LINES, d1, d2, #0
.endif
	movem.l (sp)+, d0-d5/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro
