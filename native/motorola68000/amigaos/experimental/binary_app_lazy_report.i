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
	move.l d6, d5
	mulu.w #AUDIT_PATH_STRIDE, d5
	addi.l #AUDIT_PATH_PHASE, d5
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
	beq.w nextSlot
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
nextSlot
	addq.l #1, d6
	cmpi.l #AUDIT_PATH_SLOTS, d6
	blo.w slot
done
	adda.w #AUDIT_PATH_WORD_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend ; workOriginPaths
.endif
.endif
.endif
.endif
.endif
