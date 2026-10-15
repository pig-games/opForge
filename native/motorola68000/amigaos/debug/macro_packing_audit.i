; Opt-in passive packing observations. SP-relative operands are outside this API.
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	.use debug.amigaos.lazy_work as packing_audit
.endif
.endif
.endif
; scratch is a frame address register; offsets select template and writer state.
PACKING_WRITE_BEGIN	.macro scratch, templateoffset, writerframeoffset, mode
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	move.l .mode, -(sp)
	movea.l .scratch, a0
	movea.l a0, a1
	adda.l #.writerframeoffset, a1
	adda.l #.templateoffset, a0
	move.l templates.State.AuditSession(a0), d2
	moveq #0, d0
	moveq #0, d1
	move.w templates.State.Open(a0), d0
	bne.w internal
	move.w templates.State.ExternalOpen(a0), d0
	beq.w internal
	moveq #1, d1
internal
	move.l writer.Frame.Count(a1), d3
	move.l writer.Frame.LexemeBytes(a1), d4
	move.l (sp)+, d5
	jsr packing_audit.packingBegin
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
PACKING_WRITE_END	.macro status, bytes
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .status, -(sp)
	move.l .bytes, -(sp)
	move.l (sp)+, d1
	move.l (sp)+, d0
	jsr packing_audit.packingEnd
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
PACKING_COUNT	.macro field, amount
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	move.l .amount, d1
	move.l #.field, d0
	jsr packing_audit.packingCount
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
; Instantiated inside the template owner module, where State is its own layout.
PACKING_STORED	.macro state, bytes
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_MACRO_PACKING_AUDIT
	move.w ccr, -(sp)
	movem.l d0-d3/a0, -(sp)
	move.l .bytes, -(sp)
	movea.l .state, a0
	move.l State.AuditSession(a0), d2
	moveq #0, d0
	moveq #0, d1
	move.w State.Open(a0), d0
	bne.w storedInternal
	move.w State.ExternalOpen(a0), d0
	beq.w storedInternal
	moveq #1, d1
storedInternal
	move.l (sp)+, d3
	jsr packing_audit.packingStored
	movem.l (sp)+, d0-d3/a0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
