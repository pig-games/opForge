; Completion failure observation is independently owned and imported only when
; all progress gates are present. No release code, data or import is emitted.
; Select definitions before macro capture; every missing gate keeps only no-ops.
.ifndef OPFORGE_DEBUG_CONTRACTS
.include "binding_telemetry_noop.i"
.else
.ifndef OPFORGE_MEMORY_TELEMETRY
.include "binding_telemetry_noop.i"
.else
.ifndef OPFORGE_PREPARATION_PROGRESS
.include "binding_telemetry_noop.i"
.else
	.use opforge.debug.binding_diagnostic as binding_diagnostic
BINDING_DIAGNOSTIC_CLEAR	.macro
	jsr binding_diagnostic.clear
.endmacro
; Mark before an existing call or flag setter. Existing branch instructions
; remain unchanged; the shared failure/done boundary commits only nonzero status.
BINDING_ATTEMPT	.macro stage, entry, related
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	move.l .stage, -(sp)
	move.l .entry, -(sp)
	move.l .related, -(sp)
	move.l (sp)+, d2
	move.l (sp)+, d1
	move.l (sp)+, d0
	jsr binding_diagnostic.attempt
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
.endmacro
BINDING_ATTEMPT_WORD	.macro stage, entryWord, related
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .related, d1
	moveq #0, d0
	move.w .entryWord, d0
	.BINDING_ATTEMPT .stage, d0, d1
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endmacro
; A successful binder returns a source ID. Record its zero-based target index
; and retain the pending origin proxy as Related; no lookup or semantic change.
BINDING_CANONICAL_TARGET	.macro stage, target, baseWord
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	move.l .target, d1
	moveq #0, d2
	move.w .baseWord, d2
	move.l .stage, d0
	jsr binding_diagnostic.attemptTarget
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
.endmacro
BINDING_DIAGNOSTIC_COMMIT	.macro status, state, view
	move.w ccr, -(sp)
	movem.l d0/a0-a1, -(sp)
	move.l .status, d0
	movea.l .state, a0
	lea .view, a1
	jsr binding_diagnostic.commit
	movem.l (sp)+, d0/a0-a1
	move.w (sp)+, ccr
.endmacro
BINDING_DIAGNOSTIC_REPORT	.macro dosbase
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr binding_diagnostic.report
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endmacro

; Capture a packed-record consumer failure from its live bounded range. The
; save-frame variant obtains the original base from the consumer's saved A0.
; Observations allocate nothing, change no records and emit nothing in release.
BINDING_RECORD_FAILURE	.macro stage, cursor, limit, savedBaseOffset
	move.w ccr, -(sp)
	movem.l d0/a0-a2, -(sp)
	movea.l .cursor, a0
	movea.l .limit, a2
	movea.l .savedBaseOffset+4*4+2(sp), a1
	move.l .stage, d0
	jsr binding_diagnostic.recordFailure
	movem.l (sp)+, d0/a0-a2
	move.w (sp)+, ccr
.endmacro
BINDING_RECORD_VIEW_FAILURE	.macro stage, cursor, limit, base
	move.w ccr, -(sp)
	movem.l d0/a0-a2, -(sp)
	movea.l .cursor, a0
	movea.l .limit, a2
	movea.l .base, a1
	move.l .stage, d0
	jsr binding_diagnostic.recordFailure
	movem.l (sp)+, d0/a0-a2
	move.w (sp)+, ccr
.endmacro
BINDING_RECORD_OFFSET_FAILURE	.macro stage, cursor, limit, record, offset
	move.w ccr, -(sp)
	movem.l d0-d1/a0-a2, -(sp)
	movea.l .cursor, a0
	movea.l .limit, a2
	movea.l .record, a1
	move.l .offset, d1
	move.l .stage, d0
	jsr binding_diagnostic.recordViewFailure
	movem.l (sp)+, d0-d1/a0-a2
	move.w (sp)+, ccr
.endmacro

.endif
.endif
.endif

; Keep the remap convenience wrapper callable in every build mode.
BINDING_REMAP_FAILURE	.macro cursor, limit, savedBaseOffset
	.BINDING_RECORD_FAILURE #binding_diagnostic.REMAP, .cursor, .limit, .savedBaseOffset
.endmacro
