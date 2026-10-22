; Acquisition stamps are captured before reading, then checked before cache
; publication. Includes and binary assets belong to their physical root file.
; SourceOrdinal is stable across nested includes; no source bytes are hashed.
IncrementalSource	.struct
Owner	.long ?
Path	.res PATH_BYTES
Info	.res 16
.endstruct
INCREMENTAL_SOURCE_BYTES = IncrementalSource.Info+16

; A0=acquired path. Advisory: an unavailable timestamp disables caching for
; this execution rather than changing source-language behavior. Registers kept.
incrementalRecordSource	.block
	movem.l d0-d7/a0-a6, -(sp)
	tst.l IncrementalEnabled
	beq.w done
	movea.l a0, a5
	movea.l IncrementalSources+memory.Block.Pointer, a4
	move.l IncrementalSources+memory.Block.Used, d7
next
	tst.l d7
	beq.w append
	move.l SourceOrdinal, d0
	cmp.l IncrementalSource.Owner(a4), d0
	bne.w advance
	movea.l a5, a0
	lea IncrementalSource.Path(a4), a1
	bsr.w sameNormalizedPath
	beq.w done
advance
	adda.l #INCREMENTAL_SOURCE_BYTES, a4
	subi.l #INCREMENTAL_SOURCE_BYTES, d7
	bra.w next
append
	lea IncrementalSources, a0
	move.l memory.Block.Used(a0), d6
	move.l d6, d0
	addi.l #INCREMENTAL_SOURCE_BYTES, d0
	jsr memory.reserve
	bne.w disable
	movea.l memory.Block.Pointer(a0), a4
	adda.l d6, a4
	move.l SourceOrdinal, IncrementalSource.Owner(a4)
	movea.l a5, a0
	lea IncrementalSource.Path(a4), a1
	move.l #PATH_BYTES, d0
	bsr.w copy
	movea.l a5, a0
	lea IncrementalSource.Info(a4), a1
	movea.l DosBase, a2
	jsr incrementalStore.stamp
	bne.w disable
	addi.l #INCREMENTAL_SOURCE_BYTES, IncrementalSources+memory.Block.Used
	bra.w done
disable
	clr.l IncrementalEnabled
done
	movem.l (sp)+, d0-d7/a0-a6
	rts
	.bend  ; incrementalRecordSource

; A0=first source row,D7=count. Validate exact size/date, including backdated
; edits. Rechecks acquisition stamps before publication, and saved ones on hit.
; D0/CCR=status; all other registers kept.
incrementalCheckSources	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -16(sp), sp
	movea.l a0, a4
next
	tst.l d7
	beq.w good
	lea IncrementalSource.Path(a4), a0
	movea.l a0, a1
	move.w #PATH_BYTES-1, d0
validatePath
	tst.b (a1)+
	beq.w pathReady
	dbra d0, validatePath
	bra.w bad
pathReady
	movea.l sp, a1
	movea.l DosBase, a2
	jsr incrementalStore.stamp
	bne.w bad
	lea IncrementalSource.Info(a4), a0
	movea.l sp, a1
	moveq #16, d0
	bsr.w incrementalEqual
	bne.w bad
	adda.l #INCREMENTAL_SOURCE_BYTES, a4
	subq.l #1, d7
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	lea 16(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalCheckSources

; A0=saved rows,D7=count. Require the same currently acquired physical inputs.
; This catches search-order shadowing even when old files retain their stamps.
; Dynamic file effects not yet acquired conservatively miss, never reuse stale
; data. D0/CCR=status; all other registers preserved.
incrementalCurrentSources	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l IncrementalSources+memory.Block.Pointer, a4
	move.l IncrementalSources+memory.Block.Used, d6
	moveq #0, d5
next
	tst.l d6
	beq.w counted
	move.l IncrementalSource.Owner(a4), d0
	cmp.l PublishedCapture+Span.File, d0
	bne.w advance
	movea.l a5, a3
	move.l d7, d4
find
	tst.l d4
	beq.w bad
	; Validate every saved string before the path comparer can scan it.
	lea IncrementalSource.Path(a3), a0
	movea.l a0, a1
	move.w #PATH_BYTES-1, d0
bounded
	tst.b (a1)+
	beq.w compare
	dbra d0, bounded
	bra.w bad
compare
	lea IncrementalSource.Path(a4), a1
	bsr.w sameNormalizedPath
	beq.w matched
	adda.l #INCREMENTAL_SOURCE_BYTES, a3
	subq.l #1, d4
	bra.w find
matched
	addq.l #1, d5
advance
	adda.l #INCREMENTAL_SOURCE_BYTES, a4
	subi.l #INCREMENTAL_SOURCE_BYTES, d6
	bra.w next
counted
	cmp.l d7, d5
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalCurrentSources
