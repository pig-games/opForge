; Capture-store progress schema (source / line / records fields):
; READ: completed exact read calls / bytes read / zero.
; WRITE: completed exact write calls / bytes written / zero.
; CACHE: fault attempts / completed evictions / peak resident payload capacity.
CAPTURE_STORE_READ = 360
CAPTURE_STORE_WRITE = 361
CAPTURE_STORE_CACHE = 362
; ORDERED_WORK: attempts / successful identity reuses / actual record-copy bytes.
; ORDERED_REUSE: avoided record-copy bytes / avoided replacement capacity / zero.
CAPTURE_ORDERED_WORK = 363
CAPTURE_ORDERED_SAVING = 364
; Fixed-size capture accounting. Missing any gate emits no import or calls.
; Call-site identities remain available when the observer alias is gated out.
CAPTURE_ACQUISITION = 0
CAPTURE_REPLAY = 1
CAPTURE_RELEASE = 2
; Select definitions before macro capture; every missing gate keeps only no-ops.
.ifndef OPFORGE_DEBUG_CONTRACTS
.include "capture_telemetry_noop.i"
.else
.ifndef OPFORGE_MEMORY_TELEMETRY
.include "capture_telemetry_noop.i"
.else
.ifndef OPFORGE_CAPTURE_TELEMETRY
.include "capture_telemetry_noop.i"
.else
	.use opforge.debug.capture_profile as capture_profile

CAPTURE_CLEAR	.macro
	move.w ccr, -(sp)
	jsr capture_profile.clear
	move.w (sp)+, ccr
.endmacro

CAPTURE_GROW_BEGIN	.macro
	move.w ccr, -(sp)
	jsr capture_profile.growBegin
	move.w (sp)+, ccr
.endmacro

CAPTURE_GROW_END	.macro
	move.w ccr, -(sp)
	jsr capture_profile.growEnd
	move.w (sp)+, ccr
.endmacro

CAPTURE_REPLAY_BEGIN	.macro
	move.w ccr, -(sp)
	jsr capture_profile.replayBegin
	move.w (sp)+, ccr
.endmacro

CAPTURE_CONFIGURATION_BEGIN .macro
	move.w ccr, -(sp)
	jsr capture_profile.configurationBegin
	move.w (sp)+, ccr
.endmacro

; Constructor-local clear accounting. The counter register is an output;
; preserve CCR and emit nothing unless the complete observer gate is enabled.
CAPTURE_CONSTRUCTION_CLEAR_BEGIN	.macro counter
	move.w ccr, -(sp)
	moveq #0, .counter  ; compact header has no reserved storage
	move.w (sp)+, ccr
.endmacro

CAPTURE_CONSTRUCTION_CLEAR_GAP	.macro counter, bytes
	move.w ccr, -(sp)
	add.l .bytes, .counter
	move.w (sp)+, ccr
.endmacro

; clearBytes counts explicit zero writes, excluding overwritten fields/payload.
CAPTURE_CONSTRUCTION	.macro clearBytes, count, lexemeBytes, planBytes
	move.w ccr, -(sp)
	movem.l d0-d3, -(sp)
	move.l .clearBytes, d0
	move.l .count, d1
	move.l .lexemeBytes, d2
	move.l .planBytes, d3
	jsr capture_profile.construction
	movem.l (sp)+, d0-d3
	move.w (sp)+, ccr
.endmacro

; countOffset identifies the compact capture header's unsigned byte count.
CAPTURE_APPEND	.macro arena, headerBytes, countOffset, lexemeOffset, planOffset, sourceOffset, bytes
	move.w ccr, -(sp)
	movem.l d1-d6/a0, -(sp)
	move.l .bytes, d5
	lea .arena, a0
	movea.l capture_profile.BlockView.Pointer(a0), a0
	moveq #0, d1
	move.b .countOffset(a0), d1
	move.l .lexemeOffset(a0), d2
	move.l .planOffset(a0), d3
	move.l .sourceOffset(a0), d4
	move.l #.headerBytes, d6
	jsr capture_profile.append
	movem.l (sp)+, d1-d6/a0
	move.w (sp)+, ccr
.endmacro

CAPTURE_COPY	.macro amount
	move.w ccr, -(sp)
	movem.l d0, -(sp)
	move.l .amount, d0
	jsr capture_profile.materialize
	movem.l (sp)+, d0
	move.w (sp)+, ccr
.endmacro

CAPTURE_GROW	.macro used, oldCapacity, newCapacity
	move.w ccr, -(sp)
	movem.l d1-d3, -(sp)
	move.l .used, d1
	move.l .oldCapacity, d2
	move.l .newCapacity, d3
	jsr capture_profile.grow
	movem.l (sp)+, d1-d3
	move.w (sp)+, ccr
.endmacro

CAPTURE_RETAINED_RELEASE	.macro capacity
	move.w ccr, -(sp)
	movem.l d0, -(sp)
	move.l .capacity, d0
	jsr capture_profile.retainedRelease
	movem.l (sp)+, d0
	move.w (sp)+, ccr
.endmacro

CAPTURE_SNAPSHOT	.macro boundary, table, count, temp, order, stride, arenaOffset, orderCount, orderStride
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
.endmacro

; Chunk ownership uses the same snapshot schema: Regions counts allocations,
; and Table describes their directory. Logical source regions are a separate
; index accounted by MEMD. No per-record or per-token observer lookup is added.
CAPTURE_CHUNK_SNAPSHOT	.macro boundary, directory, rowShift, rowBytes, storageOffset, temp, order, orderCount, orderStride
	move.w ccr, -(sp)
	move.l d1, -(sp)
	move.l .directory+capture_profile.BlockView.Used, d1
	lsr.l #.rowShift, d1
	.CAPTURE_SNAPSHOT .boundary, .directory, d1, .temp, .order, .rowBytes, .storageOffset, .orderCount, .orderStride
	move.l (sp)+, d1
	move.w (sp)+, ccr
.endmacro

; Caller supplies gated backend globals; inactive definitions capture no references.
CAPTURE_ORDERED_COUNT	.macro counter, amount
	.MEMORY_COUNTER_ADD .counter, .amount
.endmacro

; Exact capacity reserveBounded would request, excluding already available storage.
; Only telemetry executes this duplicate of its doubling/clamp arithmetic.
CAPTURE_ORDERED_REUSE	.macro bytes, savedCapacity, used, block, limit
	move.w ccr, -(sp)
	movem.l d0-d2/a0, -(sp)
	move.l .used, d0
	add.l d0, .bytes
	movea.l .block, a0
	cmp.l capture_profile.BlockView.Capacity(a0), d0
	bls.w done
	move.l .limit, d2
	move.l #256, d1
	cmp.l d2, d1
	bls.w sizeLoop
	move.l d2, d1
sizeLoop
	cmp.l d0, d1
	bhs.w ready
	add.l d1, d1
	bcs.w capped
	cmp.l d2, d1
	bls.w sizeLoop
capped
	move.l d2, d1
	bra.w sizeLoop
ready
	add.l d1, .savedCapacity
done
	movem.l (sp)+, d0-d2/a0
	move.w (sp)+, ccr
.endmacro

CAPTURE_ORDERED_CLEAR	.macro calls, reuses, copied, avoided, capacity
	.MEMORY_COUNTER_CLEAR .calls
	.MEMORY_COUNTER_CLEAR .reuses
	.MEMORY_COUNTER_CLEAR .copied
	.MEMORY_COUNTER_CLEAR .avoided
	.MEMORY_COUNTER_CLEAR .capacity
.endmacro

CAPTURE_ORDERED_REPORT	.macro dosbase, calls, reuses, copied, avoided, capacity
	.MEMORY_PROGRESS .dosbase, #CAPTURE_ORDERED_WORK, .calls, .reuses, .copied
	.MEMORY_PROGRESS .dosbase, #CAPTURE_ORDERED_SAVING, .avoided, .capacity, #0
.endmacro

CAPTURE_STORE_CLEAR	.macro reads, readBytes, writes, writeBytes, faults, evictions, peak
	.MEMORY_COUNTER_CLEAR .reads
	.MEMORY_COUNTER_CLEAR .readBytes
	.MEMORY_COUNTER_CLEAR .writes
	.MEMORY_COUNTER_CLEAR .writeBytes
	.MEMORY_COUNTER_CLEAR .faults
	.MEMORY_COUNTER_CLEAR .evictions
	.MEMORY_COUNTER_CLEAR .peak
.endmacro

CAPTURE_STORE_REPORT	.macro dosbase, reads, readBytes, writes, writeBytes, faults, evictions, peak
	.MEMORY_PROGRESS .dosbase, #CAPTURE_STORE_READ, .reads, .readBytes, #0
	.MEMORY_PROGRESS .dosbase, #CAPTURE_STORE_WRITE, .writes, .writeBytes, #0
	.MEMORY_PROGRESS .dosbase, #CAPTURE_STORE_CACHE, .faults, .evictions, .peak
.endmacro

.ifdef OPFORGE_PREPARATION_PROGRESS
CAPTURE_REPORT	.macro dosbase
	move.w ccr, -(sp)
	movem.l a0, -(sp)
	movea.l .dosbase, a0
	jsr capture_profile.report
	movem.l (sp)+, a0
	move.w (sp)+, ccr
.endmacro
.else
CAPTURE_REPORT	.macro dosbase
.endmacro
.endif
.endif
.endif
.endif
