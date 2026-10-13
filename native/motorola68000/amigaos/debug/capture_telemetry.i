; Fixed-size capture accounting. Missing any gate emits no import or calls.
; Call-site identities remain available when the observer alias is gated out.
CAPTURE_ACQUISITION = 0
CAPTURE_REPLAY = 1
CAPTURE_RELEASE = 2
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	.use debug.amigaos.capture_profile as capture_profile
.endif
.endif
.endif

CAPTURE_CLEAR	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	jsr capture_profile.clear
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_GROW_BEGIN	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	jsr capture_profile.growBegin
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_GROW_END	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	jsr capture_profile.growEnd
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_REPLAY_BEGIN	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	jsr capture_profile.replayBegin
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_CONSTRUCTION	.macro recordBytes, count, lexemeBytes, planBytes
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d3, -(sp)
	move.l .recordBytes, d0
	move.l .count, d1
	move.l .lexemeBytes, d2
	move.l .planBytes, d3
	jsr capture_profile.construction
	movem.l (sp)+, d0-d3
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_APPEND	.macro arena, headerBytes, countOffset, lexemeOffset, planOffset, sourceOffset, bytes
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	movem.l d1-d6/a0, -(sp)
	move.l .bytes, d5
	lea .arena, a0
	movea.l capture_profile.BlockView.Pointer(a0), a0
	move.l .countOffset(a0), d1
	move.l .lexemeOffset(a0), d2
	move.l .planOffset(a0), d3
	move.l .sourceOffset(a0), d4
	move.l #.headerBytes, d6
	jsr capture_profile.append
	movem.l (sp)+, d1-d6/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_COPY	.macro amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0, -(sp)
	move.l .amount, d0
	jsr capture_profile.materialize
	movem.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_GROW	.macro used, oldCapacity, newCapacity
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	movem.l d1-d3, -(sp)
	move.l .used, d1
	move.l .oldCapacity, d2
	move.l .newCapacity, d3
	jsr capture_profile.grow
	movem.l (sp)+, d1-d3
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_RETAINED_RELEASE	.macro capacity
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0, -(sp)
	move.l .capacity, d0
	jsr capture_profile.retainedRelease
	movem.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_SNAPSHOT	.macro boundary, table, count, temp, order, stride, arenaOffset, orderCount, orderStride
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
	move.w ccr, -(sp)
	movem.l d0-d5/a0-a2, -(sp)
	move.l .count, d1
	move.l .orderCount, d4
	move.l #.orderStride, d5
	move.l #.boundary, d0
	move.l #.stride, d2
	move.l #.arenaOffset, d3
	lea .table, a0
	lea .temp, a1
	lea .order, a2
	jsr capture_profile.snapshot
	movem.l (sp)+, d0-d5/a0-a2
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

CAPTURE_REPORT	.macro dosbase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_CAPTURE_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	movem.l a0, -(sp)
	movea.l .dosbase, a0
	jsr capture_profile.report
	movem.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro
