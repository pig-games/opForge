; Cache publication is optional: a failed read/stat/write never turns a valid
; assembly into a cache hit. The old marker is removed before the image changes.
; No borrowed image or publication pointer survives privatePublish.
incrementalSave	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l IncrementalEnabled
	beq.w good
	tst.l IncrementalKey+memory.Block.Used
	beq.w good
	bsr.w incrementalSavePublication
	bne.w good
	; Count source/include/asset rows acquired by this physical module file.
	movea.l IncrementalSources+memory.Block.Pointer, a4
	move.l IncrementalSources+memory.Block.Used, d7
	moveq #0, d6
sourceCount
	tst.l d7
	beq.w sourceReady
	move.l IncrementalSource.Owner(a4), d0
	cmp.l PublishedCapture+Span.File, d0
	bne.w sourceNext
	addq.l #1, d6
sourceNext
	adda.l #INCREMENTAL_SOURCE_BYTES, a4
	subi.l #INCREMENTAL_SOURCE_BYTES, d7
	bra.w sourceCount
sourceReady
	tst.l d6
	beq.w good
	move.l PrivateDependencies+memory.Block.Used, d5
	lsr.l #2, d5
	move.l d6, d0
	mulu.l #INCREMENTAL_SOURCE_BYTES, d0
	move.l d5, d1
	mulu.l #INCREMENTAL_DEP_BYTES, d1
	add.l d1, d0
	bcs.w good
	addi.l #INCREMENTAL_HEADER_BYTES, d0
	bcs.w good
	add.l IncrementalKey+memory.Block.Used, d0
	bcs.w good
	add.l IncrementalPublication+memory.Block.Used, d0
	bcs.w good
	move.l d0, d4
	lea IncrementalEntry, a0
	jsr memory.reserveExact
	bne.w good
	move.l d4, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a6
	movea.l a6, a1
	bsr.w privateClear
	move.l #INCREMENTAL_MAGIC, IncrementalHeader.Magic(a6)
	move.l d4, IncrementalHeader.Bytes(a6)
	move.l IncrementalNextGeneration, IncrementalHeader.Generation(a6)
	move.l IncrementalKey+memory.Block.Used, IncrementalHeader.KeyBytes(a6)
	move.l d6, IncrementalHeader.SourceCount(a6)
	move.l d5, IncrementalHeader.DependencyCount(a6)
	move.l IncrementalPublication+memory.Block.Used, IncrementalHeader.PublicationBytes(a6)
	move.l PrivateImage+memory.Block.Used, IncrementalHeader.ImageBytes(a6)
	lea INCREMENTAL_HEADER_BYTES(a6), a3
	movea.l IncrementalKey+memory.Block.Pointer, a0
	movea.l a3, a1
	move.l IncrementalKey+memory.Block.Used, d0
	bsr.w copy
	movea.l a1, a3
	movea.l IncrementalSources+memory.Block.Pointer, a4
	move.l IncrementalSources+memory.Block.Used, d7
sources
	tst.l d7
	beq.w sourcesDone
	move.l IncrementalSource.Owner(a4), d0
	cmp.l PublishedCapture+Span.File, d0
	bne.w nextSource
	movea.l a4, a0
	movea.l a3, a1
	move.l #INCREMENTAL_SOURCE_BYTES, d0
	bsr.w copy
	adda.l #INCREMENTAL_SOURCE_BYTES, a3
nextSource
	adda.l #INCREMENTAL_SOURCE_BYTES, a4
	subi.l #INCREMENTAL_SOURCE_BYTES, d7
	bra.w sources
sourcesDone
	lea INCREMENTAL_HEADER_BYTES(a6), a0
	adda.l IncrementalKey+memory.Block.Used, a0
	move.l d6, d7
	bsr.w incrementalCheckSources
	bne.w good
	movea.l PrivateDependencies+memory.Block.Pointer, a4
	move.l d5, d7
dependencies
	tst.l d7
	beq.w publication
	move.l (a4)+, d1
	bsr.w storedInterfaceModuleId
	bne.w good
	move.l d1, IncrementalDependency.Module(a3)
	move.l d1, d0
	mulu.l #INCREMENTAL_GENERATION_BYTES, d0
	move.l d0, d2
	addi.l #INCREMENTAL_GENERATION_BYTES, d0
	cmp.l IncrementalGenerations+memory.Block.Used, d0
	bhi.w good
	movea.l IncrementalGenerations+memory.Block.Pointer, a0
	move.l IncrementalGeneration.Number(a0, d2.l), d0
	beq.w good  ; dependency could not persist: do not cache its consumer
	move.l d0, IncrementalDependency.Generation(a3)
	move.l d1, d0
	mulu.l #moduleRegistry.MODULE_BYTES, d0
	movea.l PrivateRegistry+moduleRegistry.State.Modules+memory.Block.Pointer, a0
	adda.l d0, a0
	lea moduleRegistry.Module.FileName(a0), a0
	lea IncrementalDependency.Name(a3), a1
	move.l #moduleObjectStore.NAME_BYTES, d0
	bsr.w copy
	adda.w #INCREMENTAL_DEP_BYTES, a3
	subq.l #1, d7
	bra.w dependencies
publication
	movea.l IncrementalPublication+memory.Block.Pointer, a0
	movea.l a3, a1
	move.l IncrementalPublication+memory.Block.Used, d0
	bsr.w copy
	; Remove the validity marker first, even when the subsequent replacement fails.
	movea.l DosBase, a6
	move.l #IncrementalMetaPath, d1
	jsr CACHE_DOS_DELETE(a6)
	lea IncrementalImagePath, a0
	lea PrivateImage, a1
	movea.l DosBase, a2
	jsr incrementalStore.save
	bne.w good
	lea IncrementalImagePath, a0
	movea.l IncrementalEntry+memory.Block.Pointer, a1
	lea IncrementalHeader.ImageInfo(a1), a1
	jsr incrementalStore.stamp
	bne.w good
	lea IncrementalMetaPath, a0
	lea IncrementalEntry, a1
	jsr incrementalStore.save
	bne.w good
	move.l #1, IncrementalSaved
good
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; incrementalSave

; Append one small generation row after the ordinary publication succeeds.
; Consumers consult it before reusing their saved local import identities.
incrementalPublished	.block
	movem.l d1-d4/a0-a2, -(sp)
	tst.l IncrementalEnabled
	beq.w good
	move.l PrivateModuleID, d3
	mulu.l #INCREMENTAL_GENERATION_BYTES, d3
	cmp.l IncrementalGenerations+memory.Block.Used, d3
	bne.w bad
	lea IncrementalGenerations, a0
	move.l d3, d0
	addi.l #INCREMENTAL_GENERATION_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d3, a1
	clr.l IncrementalGeneration.Number(a1)
	move.l #1, IncrementalGeneration.Changed(a1)
	tst.l IncrementalHit
	bne.w hit
	addq.l #1, IncrementalMisses
	tst.l IncrementalSaved
	beq.w commit
	move.l IncrementalNextGeneration, IncrementalGeneration.Number(a1)
	bra.w commit
hit
	clr.l IncrementalGeneration.Changed(a1)
	move.l IncrementalNextGeneration, IncrementalGeneration.Number(a1)
commit
	addi.l #INCREMENTAL_GENERATION_BYTES, memory.Block.Used(a0)
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; incrementalPublished

; A0/A1=equal-size bytes,D0=length. D0/CCR=0 equal,1 unequal; others kept.
incrementalEqual	.block
	movem.l d1/a0-a1, -(sp)
	tst.l d0
	beq.w good
next
	cmpm.b (a0)+, (a1)+
	bne.w bad
	subq.l #1, d0
	bne.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; incrementalEqual

; Release per-attempt serialization, before the next compiler can add a peak.
incrementalReleaseAttempt	.block
	movem.l d0/a0, -(sp)
	lea IncrementalKey, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea IncrementalEntry, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea IncrementalPublication, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, d0/a0
	rts
	.bend  ; incrementalReleaseAttempt

; Reuse is visible in normal CLI output; counters describe accepted modules,
; not discovery records or source reads. Existing fresh-run protocol is separate.
incrementalFinish	.block
	movem.l d0-d7/a0-a6, -(sp)
	tst.l IncrementalEnabled
	beq.w release
	tst.w CliQuiet
	bne.w release
	move.l IncrementalHits, d0
	lea IncrementalReportHits, a0
	bsr.w incrementalHex
	move.l IncrementalMisses, d0
	lea IncrementalReportMisses, a0
	bsr.w incrementalHex
	movea.l DosBase, a6
	jsr CACHE_DOS_OUTPUT(a6)
	move.l d0, d1
	move.l #IncrementalReport, d2
	move.l #IncrementalReportEnd-IncrementalReport, d3
	jsr CACHE_DOS_WRITE(a6)
release
	bsr.w incrementalReleaseAttempt
	lea IncrementalSources, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea IncrementalGenerations, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l IncrementalEnabled
	movem.l (sp)+, d0-d7/a0-a6
	rts
	.bend  ; incrementalFinish
incrementalHex	.block
	movem.l d0-d3/a0, -(sp)
	moveq #7, d3
next
	rol.l #4, d0
	move.l d0, d1
	andi.l #15, d1
	addi.b #'0', d1
	cmpi.b #'9', d1
	bls.w digit
	addq.b #7, d1
digit
	move.b d1, (a0)+
	dbra d3, next
	movem.l (sp)+, d0-d3/a0
	rts
	.bend  ; incrementalHex
	.endsection
	.section data, kind=data
IncrementalEmpty	.byte 0
IncrementalUnavailable	.byte "incremental: cache unavailable; assembling fresh", 10, 0
IncrementalReport	.byte "incremental: reused=0x"
IncrementalReportHits	.byte "00000000"
	.byte " rebuilt=0x"
IncrementalReportMisses	.byte "00000000"
	.byte 10
IncrementalReportEnd
	.endsection
	.section code, kind=code
