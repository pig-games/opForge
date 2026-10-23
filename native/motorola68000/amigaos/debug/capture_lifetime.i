; Missing the experiment gate emits no code/import/storage. Operands must not
; depend on SP; macros snapshot register operands before argument assignment.
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	.use opforge.debug.capture_lifetime as capture_life
.endif
CAPTURE_LIFE_CLEAR	.macro
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	jsr capture_life.clear
.endif
.endmacro
CAPTURE_LIFE_ALLOC	.macro amount
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr capture_life.allocate
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endmacro
CAPTURE_LIFE_FREE	.macro amount
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr capture_life.release
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endmacro
CAPTURE_LIFE_RETAIN_BEGIN	.macro
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	jsr capture_life.retainedBegin
.endif
.endmacro
CAPTURE_LIFE_RETAIN_END	.macro
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	jsr capture_life.retainedEnd
.endif
.endmacro
CAPTURE_LIFE_PHASE	.macro value
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .value, d0
	jsr capture_life.phase
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endmacro
CAPTURE_LIFE_REGION	.macro index, file
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .index, -(sp)
	move.l .file, -(sp)
	move.l (sp)+, d1
	move.l (sp)+, d0
	jsr capture_life.region
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endmacro
; block=record pointer, layout=packet pointer; no pointer survives inspect.
CAPTURE_LIFE_RECORD	.macro block, layout, index, capacity, bytes
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d2/a0-a1, -(sp)
	move.l .block, -(sp)
	move.l .layout, -(sp)
	move.l .index, -(sp)
	move.l .capacity, -(sp)
	move.l .bytes, -(sp)
	move.l (sp)+, d0
	move.l (sp)+, d2
	move.l (sp)+, d1
	movea.l (sp)+, a1
	movea.l (sp)+, a0
	jsr capture_life.inspect
	movem.l (sp)+, d0-d2/a0-a1
	move.w (sp)+, ccr
.endif
.endmacro
CAPTURE_LIFE_CURVE	.macro span, eligible, modelCapacity, pins, regions
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d4, -(sp)
	move.l .span, -(sp)
	move.l .eligible, -(sp)
	move.l .modelCapacity, -(sp)
	move.l .pins, -(sp)
	move.l .regions, -(sp)
	move.l (sp)+, d4
	move.l (sp)+, d3
	move.l (sp)+, d2
	move.l (sp)+, d1
	move.l (sp)+, d0
	jsr capture_life.curve
	movem.l (sp)+, d0-d4
	move.w (sp)+, ccr
.endif
.endmacro
CAPTURE_LIFE_REPORT	.macro dosbase
.ifdef OPFORGE_CAPTURE_LIFETIME_AUDIT
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr capture_life.report
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro
