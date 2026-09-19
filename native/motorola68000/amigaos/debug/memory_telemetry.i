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
