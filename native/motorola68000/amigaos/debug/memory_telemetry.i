; Reusable memory accounting. Both gates are required; release emits nothing.
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.use debug.amigaos.memory_profile as memory_profile
.endif
.endif
MEMORY_ALLOC	.macro amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr memory_profile.allocate
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Bounded work and phase-clock accounting uses the same optional record.
MEMORY_WORK	.macro index, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .amount, d1
	move.l .index, d0
	jsr memory_profile.work
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro

MEMORY_CLOCK	.macro dosbase, index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	move.l .index, d0
	movea.l .dosbase, a0
	jsr memory_profile.clock
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
MEMORY_FREE	.macro amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr memory_profile.release
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Mark a bounded reserve failure (64) or Exec allocation failure (128).
; The call is compiled out of ordinary builds and preserves the failed status.
MEMORY_FAILURE	.macro kind, request, capacity, used
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d3, -(sp)
	move.l .request, d1
	move.l .capacity, d2
	move.l .used, d3
	move.l .kind, d0
	jsr memory_profile.failure
	movem.l (sp)+, d0-d3
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Opt-in bounded source/preparation progress. The observer preserves the entire
; caller frame and writes a short structured line to the guest's captured stdout.
MEMORY_PROGRESS	.macro dosbase, phase, source, line, records
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l .phase, d0
	move.l .source, d1
	move.l .line, d2
	move.l .records, d3
	movea.l .dosbase, a0
	jsr memory_profile.progress
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
MEMORY_PHASE	.macro value
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .value, d0
	jsr memory_profile.phase
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
MEMORY_SAVE	.macro dosbase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr memory_profile.save
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

MEMORY_LAYOUT	.macro runtime_bytes, record_bytes, source_bytes
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	move.l .runtime_bytes, d0
	move.l .record_bytes, d1
	move.l .source_bytes, d2
	jsr memory_profile.layout
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Exclusive preparation stage transition, D0/CCR and all other registers preserved.
MEMORY_STAGE	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.stage
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TOKEN_BEGIN	.macro amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	jsr memory_profile.tokenBegin
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TOKEN_OPCODE	.macro opcode
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .opcode, d0
	jsr memory_profile.tokenOpcode
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TOKEN_SCOPE_BEGIN	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.tokenScopeBegin
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TOKEN_SCOPE_END	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.tokenScopeEnd
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TOKEN_WORK	.macro index, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .amount, d1
	move.l .index, d0
	jsr memory_profile.tokenWork
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro

; Close a scope if active, for shared success/failure return boundaries.
TOKEN_SCOPE_CLOSE	.macro index
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .index, d0
	jsr memory_profile.tokenScopeClose
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
