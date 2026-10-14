; Optional source dictionary for numeric audit exemplars. Included by app.
; Uses only live, owned OriginPaths; never the mutable diagnostic fallback.
.ifdef OPFORGE_LAZY_AUDIT
.ifdef OPFORGE_LAZY_WORK
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
AuditPathWords .struct
First .long ?
Second .long ?
Third .long ?
.endstruct
AUDIT_PATH_WORD_BYTES = AuditPathWords.Third+4
AUDIT_PATH_SLOTS = lazy_work.TOP_COUNT+lazy_work.RANGE_EXAMPLE_COUNT
AUDIT_PATH_PHASE = 2300
AUDIT_PATH_STRIDE = 24 ; one header and up to 22 twelve-byte chunks
; Before OriginPaths release. No inputs/outputs; preserves all registers/CCR/SP.
workOriginPaths .block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	suba.w #AUDIT_PATH_WORD_BYTES, sp
	movea.l sp, a3
	jsr lazy_work.getState
	movea.l a0, a5
	movea.l DosBase, a4
	move.l a4, d0
	beq.w done
	moveq #0, d6
slot
	moveq #0, d7
	suba.l a2, a2
	move.l d6, d0
	cmpi.l #lazy_work.TOP_COUNT, d0
	bhs.w rangeOrigin
	mulu.w #lazy_work.TOP_BYTES, d0
	lea lazy_work.WorkFields.TopRows+lazy_work.Top.Origin(a5), a0
	move.l 0(a0, d0.l), d1
	bra.w origin
rangeOrigin
	subi.l #lazy_work.TOP_COUNT, d0
	mulu.w #lazy_work.RANGE_EXAMPLE_BYTES, d0
	lea lazy_work.WorkFields.RangeExamples+lazy_work.RangeExample.Origin(a5), a0
	move.l 0(a0, d0.l), d1
origin
	move.l d6, d5
	mulu.w #AUDIT_PATH_STRIDE, d5
	addi.l #AUDIT_PATH_PHASE, d5
	bsr.w workWritePath
	addq.l #1, d6
	cmpi.l #AUDIT_PATH_SLOTS, d6
	blo.w slot
done
	bsr.w workTemplatePaths
	adda.w #AUDIT_PATH_WORD_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend ; workOriginPaths

; D1=origin,D5=phase,D6=zero-based slot,A3=12-byte scratch,A4=DOS base.
; Borrow owned paths only. Clobbers D0-D5/D7/A0-A2, preserves D6/A3-A6.
workWritePath .block
	moveq #0, d7
	suba.l a2, a2
	move.l d1, d4
	beq.w header
	cmpi.l #ORIGIN_LIMIT, d1
	bhi.w header
	lsl.l #8, d1 ; existing origin slots are PATH_BYTES=256
	move.l d1, d2
	addi.l #PATH_BYTES, d2
	lea OriginPaths, a0
	cmp.l memory.Block.Used(a0), d2
	bhi.w header
	cmp.l memory.Block.Capacity(a0), d2
	bhi.w header
	move.l memory.Block.Pointer(a0), d0
	beq.w header
	add.l d1, d0
	bcs.w header
	movea.l d0, a2
length
	tst.b 0(a2, d7.w)
	beq.w header
	addq.w #1, d7
	cmpi.w #PATH_BYTES, d7
	blo.w length
	moveq #0, d7 ; no bounded terminator: omit the path, never read beyond owner
header
	move.l d5, d0
	move.l d4, d1
	move.l d7, d2
	move.l d6, d3
	addq.l #1, d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d5
chunk
	tst.l d7
	beq.w done
	clr.l AuditPathWords.First(a3)
	clr.l AuditPathWords.Second(a3)
	clr.l AuditPathWords.Third(a3)
	movea.l a3, a1
	moveq #0, d4
byte
	tst.l d7
	beq.w writeChunk
	move.b (a2)+, (a1)+
	subq.l #1, d7
	addq.w #1, d4
	cmpi.w #AUDIT_PATH_WORD_BYTES, d4
	blo.w byte
writeChunk
	move.l d5, d0
	move.l AuditPathWords.First(a3), d1
	move.l AuditPathWords.Second(a3), d2
	move.l AuditPathWords.Third(a3), d3
	movea.l a4, a0
	jsr memory_profile.progress
	addq.l #1, d5
	bra.w chunk
done
	rts
	.bend ; workWritePath

; All used inventory rows/edges have an optional source-path exemplar. Numeric
; identities and counts remain authoritative; missing paths remain empty.
; No inputs/outputs; preserves registers/CCR/SP, before OriginPaths release.
workTemplatePaths .block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	suba.w #AUDIT_PATH_WORD_BYTES, sp
	movea.l sp, a3
	movea.l DosBase, a4
	move.l a4, d0
	beq.w done
	jsr lazy_work.getTemplates
	movea.l a0, a5
	move.l #lazy_work.TEMPLATE_PATH_HEADER, d0
	moveq #1, d1
	move.l lazy_work.TemplateFields.Count(a5), d2
	move.l lazy_work.TemplateFields.EdgeCount(a5), d3
	movea.l a4, a0
	jsr memory_profile.progress
	moveq #0, d6
slot
	move.l lazy_work.TemplateFields.Count(a5), d0
	add.l lazy_work.TemplateFields.EdgeCount(a5), d0
	cmp.l d0, d6
	bhs.w done
	move.l d6, d0
	cmp.l lazy_work.TemplateFields.Count(a5), d0
	bhs.w edgeOrigin
	mulu.w #lazy_work.TEMPLATE_DEFINITION_BYTES, d0
	movea.l a5, a0
	adda.l #lazy_work.TemplateFields.Rows+lazy_work.TemplateDefinition.Origin, a0
	move.l 0(a0, d0.l), d1
	bra.w path
edgeOrigin
	sub.l lazy_work.TemplateFields.Count(a5), d0
	mulu.w #lazy_work.TEMPLATE_EDGE_BYTES, d0
	movea.l a5, a0
	adda.l #lazy_work.TemplateFields.Edges+lazy_work.TemplateEdge.Origin, a0
	move.l 0(a0, d0.l), d1
path
	move.l d6, d5
	mulu.w #AUDIT_PATH_STRIDE, d5
	addi.l #lazy_work.TEMPLATE_PATH_FIRST, d5
	bsr.w workWritePath
	addq.l #1, d6
	bra.w slot
done
	adda.w #AUDIT_PATH_WORD_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend ; workTemplatePaths
.endif
.endif
.endif
.endif
.endif
