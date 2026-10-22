; A self-build may replace the executable with identical bytes and a new date.
; Stream only the tool image for compatibility identity, never source files.
; Bounded heap buffer avoids imposing another multi-KiB stack requirement.
incrementalToolIdentity	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -INCREMENTAL_BLOCK_BYTES(sp), sp
	movea.l sp, a5
	clr.l memory.Block.Pointer(a5)
	clr.l memory.Block.Capacity(a5)
	clr.l memory.Block.Used(a5)
	movea.l a5, a0
	move.l #CACHE_TOOL_BUFFER_BYTES, d0
	jsr memory.reserveExact
	bne.w failed
	movea.l DosBase, a6
	move.l #IncrementalToolPath, d1
	move.l #CACHE_DOS_OLD_FILE, d2
	jsr CACHE_DOS_OPEN(a6)
	move.l d0, d7
	beq.w release
	move.l #$811c9dc5, d5
	move.l #$9e3779b9, d6
next
	move.l d7, d1
	move.l memory.Block.Pointer(a5), d2
	move.l #CACHE_TOOL_BUFFER_BYTES, d3
	jsr CACHE_DOS_READ(a6)
	tst.l d0
	bmi.w closeBad
	beq.w finish
	movea.l memory.Block.Pointer(a5), a0
	move.l d5, d1
	move.l d6, d2
	bsr.w incrementalDigestBytes
	move.l d1, d5
	move.l d2, d6
	bra.w next
finish
	move.l d7, d1
	jsr CACHE_DOS_CLOSE(a6)
	tst.l d0
	beq.w release
	move.l d5, IncrementalToolInfo
	move.l d6, IncrementalToolInfo+4
	moveq #0, d7
	bra.w free
closeBad
	move.l d7, d1
	jsr CACHE_DOS_CLOSE(a6)
release
	moveq #1, d7
free
	movea.l a5, a0
	jsr memory.release
	move.l d7, d0
	bra.w done
failed
	moveq #1, d0
done
	lea INCREMENTAL_BLOCK_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalToolIdentity
