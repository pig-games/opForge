; Additional opt-in passive work observer. Both flags are required.
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	.use debug.amigaos.lazy_work as lazy_work
.endif
.endif
WORK_COUNT .macro field, amount
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .amount, d1
	move.l #.field, d0
	jsr lazy_work.count
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_CALL .macro routine
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	jsr .routine
.endif
.endif
.endmacro
WORK_ACQUIRE .macro index, request
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	moveq #.index, d0
	lea capture.Frame.WorkCounts(.request), a0
	jsr lazy_work.acquire
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_CAPTURE_RESET .macro request
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	clr.l capture.Frame.WorkCounts(.request)
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_BOUND_BEGIN .macro amount
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	move.l d1, -(sp)
	move.l .amount, d1
	jsr lazy_work.boundBegin
	move.l (sp)+, d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_BOUND_END .macro amount
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	move.l d1, -(sp)
	move.l .amount, d1
	jsr lazy_work.boundEnd
	move.l (sp)+, d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_PUBLICATION .macro state
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	move.l a1, -(sp)
	movea.l .state, a1
	jsr lazy_work.publication
	movea.l (sp)+, a1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_CAPTURE_REGION .macro descriptor, globalEnd
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0/a0-a1, -(sp)
	movea.l .descriptor, a1
	move.l .globalEnd, d0
	jsr lazy_units.getState
	jsr lazy_work.captureRegion
	movem.l (sp)+, d0/a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_SELECTED .macro table, count
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0/a0-a1, -(sp)
	movea.l .table, a1
	move.w .count, d0
	andi.l #$ffff, d0
	jsr lazy_units.getState
	jsr lazy_work.selected
	movem.l (sp)+, d0/a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_REPORT .macro dosbase
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr lazy_work.report
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endif
.endmacro
WORK_EFFECT .macro record
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .record, a0
	jsr lazy_work.effect
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
; Operands may alias D1/D2; SP-relative operands are outside this API.
WORK_TEMPLATE_DELTA .macro previous, current
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	move.l .previous, -(sp)
	move.l .current, -(sp)
	move.l (sp)+, d1
	move.l (sp)+, d2
	jsr lazy_work.templateDelta
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
.endif
.endif
.endmacro

WORK_SAMPLE_BEGIN .macro origin
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d1/a0, -(sp)
	move.l .origin, d1
	jsr lazy_units.getState
	jsr lazy_work.sampleBegin
	movem.l (sp)+, d1/a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

WORK_PLAN_CONTEXT .macro category
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .category, d0
	jsr lazy_work.contextSet
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
WORK_CONTEXT .macro metric, amount
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
	move.w ccr, -(sp)
	movem.l d1-d2, -(sp)
	move.l .amount, d2
	move.l #.metric, d1
	jsr lazy_work.context
	movem.l (sp)+, d1-d2
	move.w (sp)+, ccr
.endif
.endif
.endmacro
