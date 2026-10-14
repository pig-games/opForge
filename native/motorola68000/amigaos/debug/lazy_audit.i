	.include "lazy_work.i"
; Opt-in passive shadow audit. Work requires only OPFORGE_LAZY_AUDIT.
; Reporting additionally uses the approved memory progress bridge.
.ifdef OPFORGE_LAZY_AUDIT
	.use debug.amigaos.lazy_audit as lazy_audit
	.use debug.amigaos.lazy_units as lazy_units
.endif

LAZY_CLEAR	.macro  ; no parameters
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	jsr lazy_audit.clear
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_COUNT	.macro field, amount
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l .amount, d1
	move.l #.field, d0
	jsr lazy_audit.count
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_REGION	.macro descriptor, index
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l .index, d0
	lea .descriptor, a0
	jsr lazy_audit.observeRegion
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_LOOKUP	.macro  ; no parameters
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	jsr lazy_audit.lookup
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_PIN	.macro delta
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l #.delta, d0
	jsr lazy_audit.pin
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_SCHEDULE	.macro table, count, stride
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	lea .table, a0
	move.l .count, d0
	move.l #.stride, d1
	jsr lazy_audit.schedule
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_SPAN	.macro start, end, file, prepared
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l .start, d0
	move.l .end, d1
	move.l .file, d2
	move.l .prepared, d3
	jsr lazy_audit.observeSpan
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_SPAN_END	.macro prepared
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l .prepared, d0
	jsr lazy_audit.spanEnd
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_BLOCKS	.macro table, count
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	movea.l .table, a0
	move.l .count, d0
	jsr lazy_audit.blocks
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_REPORT .macro dosbase
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l a0, -(sp)
	movea.l .dosbase, a0
	jsr lazy_audit.report
	movea.l (sp)+, a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro

LAZY_CLASSIFY .macro
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	moveq #1, d1
	tst.l d0
	bne.w unavailable
	moveq #lazy_audit.CLASSIFIED, d0
	bra.w observed
unavailable
	moveq #lazy_audit.UNAVAILABLE, d0
observed
	jsr lazy_audit.count
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endmacro
LAZY_SELECTED .macro table, count
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	moveq #0, d0
	move.w .count, d0
	movea.l .table, a0
	jsr lazy_audit.selected
	.WORK_SELECTED .table, .count
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
.endif
.endmacro

LAZY_PREPARED .macro frame, offset, captureOffset, origin, line
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d5/a0, -(sp)
	lea .frame, a0
	jsr frontend.expansionDepth
	move.l d0, d5
	move.l .offset, d1
	move.l .captureOffset, d2
	move.l .origin, d3
	move.l .line, d4
	move.l frontend.Frame.Used(a0), d0
	movea.l frontend.Frame.Output(a0), a0
	jsr lazy_units.prepared
	movem.l (sp)+, d0-d5/a0
	move.w (sp)+, ccr
.endif
.endmacro
LAZY_REMAP .macro start, end, destination
.ifdef OPFORGE_LAZY_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d2, -(sp)
	move.l .start, d0
	move.l .end, d1
	move.l .destination, d2
	jsr lazy_units.remap
	movem.l (sp)+, d0-d2
	move.w (sp)+, ccr
.endif
.endmacro
