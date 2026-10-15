; Optional feature reports share preservation and preprocessing gates. Feature
; selection stays in the caller so source configuration controls emitted calls.
DEFERRED_REPORT_PAIR	.macro dosbase, session, routine, phase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d5/a0, -(sp)
	lea .session, a0
	jsr .routine
	.MEMORY_PROGRESS .dosbase, #.phase, d1, d2, #0
	movem.l (sp)+, d0-d5/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
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
	movem.l (sp)+, d0-d5/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.if ACQUIRE_MACRO_CANDIDATES
	.DEFERRED_REPORT_PAIR .dosbase, .session, frontend.acquisitionStats, DEFERRED_REPORT_ACQUISITION
.endif
.if DEFER_MACRO_LINES
	.DEFERRED_REPORT_PAIR .dosbase, .session, frontend.lineStats, DEFERRED_REPORT_LINES
.endif
.endif
.endmacro
