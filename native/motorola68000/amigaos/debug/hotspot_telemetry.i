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
HOTSPOT_SOURCE_CONTROL = 16
HOTSPOT_DECLARATION = 17
HOTSPOT_CONDITIONAL = 18
HOTSPOT_METADATA = 19
HOTSPOT_CONFIGURATION = 20
HOTSPOT_OPERAND_BIND = 21
HOTSPOT_SCOPE_LINE = 22
HOTSPOT_VALUE_CAPTURE = 23
HOTSPOT_FILE_LINE = 24
HOTSPOT_DATA_PREPARE = 25
HOTSPOT_EXPRESSION_PREPARE = 26
HOTSPOT_PREPARED_COPY = 27
HOTSPOT_GRAPH_LINE = 28
HOTSPOT_SCOPE_COUNT = 29
HOTSPOT_LOOKUP_HASH = 30
HOTSPOT_LOOKUP_CHAIN = 31
HOTSPOT_SLOT_COUNT = 32
HOTSPOT_CAPTURE_EMPTY_RECORDS = 0
HOTSPOT_CAPTURE_EMPTY_BYTES = 1
HOTSPOT_REPLAY_EMPTY_RECORDS = 2
HOTSPOT_PROCESS_EMPTY_INPUTS = 3
HOTSPOT_PROCESS_EMPTY_OUTPUTS = 4
HOTSPOT_SOURCE_CONSUMED = 5
HOTSPOT_CONDITION_CONSUMED = 6
HOTSPOT_LOOKUP_HASH_BYTES = 7
HOTSPOT_LOOKUP_CHAIN_PROBES = 8
HOTSPOT_LOOKUP_NAME_COMPARES = 9
HOTSPOT_LOOKUP_COMPARE_BYTES = 10
HOTSPOT_LOOKUP_HITS = 11
HOTSPOT_LOOKUP_MISSES = 12
HOTSPOT_LOOKUP_NAMESPACE_REJECTS = 13
HOTSPOT_LOOKUP_LENGTH_REJECTS = 14
HOTSPOT_PROCESS_RECORDS = 15
HOTSPOT_COUNTER_COUNT = 16
; Select definitions before macro capture; every missing gate keeps only no-ops.
.ifndef OPFORGE_DEBUG_CONTRACTS
.include "hotspot_telemetry_noop.i"
.else
.ifndef OPFORGE_MEMORY_TELEMETRY
.include "hotspot_telemetry_noop.i"
.else
.ifndef OPFORGE_HOTSPOT_TELEMETRY
.include "hotspot_telemetry_noop.i"
.else
	.use debug.amigaos.hotspot_profile as hotspot_profile

HOTSPOT_CLEAR	.macro
	move.w ccr, -(sp)
	jsr hotspot_profile.clear
	move.w (sp)+, ccr
.endmacro

HOTSPOT_ENTER	.macro slot
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .slot, d0
	jsr hotspot_profile.enter
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endmacro

HOTSPOT_LEAVE	.macro slot
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .slot, d0
	jsr hotspot_profile.leave
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endmacro

.ifdef OPFORGE_PREPARATION_PROGRESS
HOTSPOT_REPORT	.macro dosbase
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr hotspot_profile.report
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endmacro
.else
HOTSPOT_REPORT	.macro dosbase
.endmacro
.endif

; Static counter identity (without #); amount is a long source operand.
; Dedicated bounded storage, no clock reads/calls; preserve all registers/CCR/SP.
HOTSPOT_COUNT .macro counterID, amount
 move.w ccr, -(sp)
 move.l d0, -(sp)
 move.l .amount, d0
 tst.l hotspot_profile.Stopped
 bne.w done
 add.l d0, hotspot_profile.Counters+4*.counterID
 bcc.w done
 move.l #$ffffffff, hotspot_profile.Counters+4*.counterID
 ori.l #hotspot_profile.ARITHMETIC_OVERFLOW, hotspot_profile.Errors
 move.l #1, hotspot_profile.Stopped
done
 move.l (sp)+, d0
 move.w (sp)+, ccr
.endmacro

; Long-valued predicates remain wholly within the telemetry gate.
HOTSPOT_COUNT_IF_ZERO .macro value, counterID, amount
 move.w ccr, -(sp)
 tst.l .value
 bne.w done
 .HOTSPOT_COUNT .counterID,.amount
done
 move.w (sp)+, ccr
.endmacro

; Explicit byte predicate for retained record kind fields.
HOTSPOT_COUNT_IF_EQ_BYTE .macro value, expected, counterID, amount
 move.w ccr, -(sp)
 cmpi.b .expected,.value
 bne.w done
 .HOTSPOT_COUNT .counterID,.amount
done
 move.w (sp)+, ccr
.endmacro

HOTSPOT_COUNT_IF_EQ .macro value, expected, counterID, amount
 move.w ccr, -(sp)
 cmpi.l .expected,.value
 bne.w done
 .HOTSPOT_COUNT .counterID,.amount
done
 move.w (sp)+, ccr
.endmacro

; Expected may be an immediate, memory or register operand except D1.
HOTSPOT_COUNT_IF_NE_WORD .macro value, expected, counterID, amount
 move.w ccr, -(sp)
 move.l d1, -(sp)
 move.w .value, d1
 cmp.w .expected,d1
 beq.w done
 .HOTSPOT_COUNT .counterID,.amount
done
 move.l (sp)+, d1
 move.w (sp)+, ccr
.endmacro
.endif
.endif
.endif

; Measured calls retain the callee's result registers/CCR exactly. Disabled
; forms emit only the original call; passive leave preserves its result flags.
HOTSPOT_CALL .macro slot, target
 .HOTSPOT_ENTER .slot
 jsr .target
 .HOTSPOT_LEAVE .slot
.endmacro

HOTSPOT_BSR .macro slot, target
 .HOTSPOT_ENTER .slot
 bsr.w .target
 .HOTSPOT_LEAVE .slot
.endmacro
