; Completion failure observation is independently owned and imported only when
; all progress gates are present. No release code, data or import is emitted.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.use debug.amigaos.binding_diagnostic as binding_diagnostic
.endif
.endif
.endif
BINDING_DIAGNOSTIC_CLEAR	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	jsr binding_diagnostic.clear
.endif
.endif
.endif
.endmacro
; Mark before an existing call or flag setter. Existing branch instructions
; remain unchanged; the shared failure/done boundary commits only nonzero status.
BINDING_ATTEMPT	.macro stage, entry, related
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
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
.endif
.endif
.endif
.endmacro
BINDING_ATTEMPT_WORD	.macro stage, entryWord, related
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .related, d1
	moveq #0, d0
	move.w .entryWord, d0
	.BINDING_ATTEMPT .stage, d0, d1
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
; A successful binder returns a source ID. Record its zero-based target index
; and retain the pending origin proxy as Related; no lookup or semantic change.
BINDING_CANONICAL_TARGET	.macro stage, target, baseWord
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	move.l .target, d1
	moveq #0, d2
	move.w .baseWord, d2
	move.l .stage, d0
	jsr binding_diagnostic.attemptTarget
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
BINDING_DIAGNOSTIC_COMMIT	.macro status, state, view
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0/a0-a1, -(sp)
	move.l .status, d0
	movea.l .state, a0
	lea .view, a1
	jsr binding_diagnostic.commit
	movem.l (sp)+, d0/a0-a1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
BINDING_DIAGNOSTIC_REPORT	.macro dosbase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr binding_diagnostic.report
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
