; Fixed-size sampled routine calls; identities survive every disabled gate.
HOTSPOT_CAPTURE_LINE = 0
HOTSPOT_CANDIDATE_ACQUISITION = 1
HOTSPOT_CAPTURE_CONSTRUCTION = 2
HOTSPOT_CAPTURE_APPEND = 3
HOTSPOT_BLOCK_GROW = 4
HOTSPOT_REPLAY_LINE = 5
HOTSPOT_CAPTURE_PREVIEW = 6
HOTSPOT_PACKED_WRITE = 7
HOTSPOT_NAME_BIND = 8
HOTSPOT_SCOPE_BIND = 9
HOTSPOT_IMPORT_RESOLVE = 10
HOTSPOT_EXPRESSION_COMPILE = 11
HOTSPOT_RECORD_PROCESS = 12
HOTSPOT_PLAN_BIND = 13
HOTSPOT_TOKENIZE = 14
HOTSPOT_SCOPE_LOOKUP = 15
HOTSPOT_SLOT_COUNT = 16
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_HOTSPOT_TELEMETRY
	.use debug.amigaos.hotspot_profile as hotspot_profile
.endif
.endif
.endif

HOTSPOT_CLEAR	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_HOTSPOT_TELEMETRY
	move.w ccr, -(sp)
	jsr hotspot_profile.clear
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

HOTSPOT_ENTER	.macro slot
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_HOTSPOT_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .slot, d0
	jsr hotspot_profile.enter
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

HOTSPOT_LEAVE	.macro slot
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_HOTSPOT_TELEMETRY
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .slot, d0
	jsr hotspot_profile.leave
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro

HOTSPOT_REPORT	.macro dosbase
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_HOTSPOT_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr hotspot_profile.report
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro
