; Preparation-only discovery selection. Every candidate is read once for
; declaration names; only reachable files enter the binary frontend.

; D0/CCR=status; other registers preserved. Paths remain owned by CandidatePaths, independently of stationary traversal scratch.
indexCandidates	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #1, SourceOrdinal
nextCandidate
	move.l SourceOrdinal, d0
	cmp.l CandidateCount, d0
	bhi.w success
	bsr.w openSource
	bne.w bad
	bsr.w pathStem
	beq.w closeBad
	lea DeclarationBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	move.l SourceOrdinal, d1
	jsr declarations.fileDerived
	bne.w closeBad
	clr.l LineUsed
	clr.l IndexOverflow
read
	bsr.w readByte
	cmpi.l #-1, d0
	beq.w endFile
	tst.l d0
	bmi.w closeBad
	cmpi.b #10, d0
	beq.w endLine
	tst.l IndexOverflow
	bne.w read
	move.l LineUsed, d1
	cmpi.l #LINE_BYTES, d1
	bhs.w overflow
	movea.l LineBuffer, a0
	move.b d0, 0(a0, d1.l)
	addq.l #1, LineUsed
	bra.w read
overflow
	move.l #1, IndexOverflow
	bra.w read
endLine
	bsr.w indexLine
	bne.w closeBad
	bra.w read
endFile
	bsr.w indexLine
	bne.w closeBad
	bsr.w closeSource
	bne.w bad
	addq.l #1, SourceOrdinal
	bra.w nextCandidate
closeBad
	bsr.w closeSource
bad
	moveq #1, d0
	bra.w done
success
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; indexCandidates

; Return A1/D0 as the basename without its final extension, prefixing ASCII
; digit-leading names with '_'. SourcePath is unchanged; prefixed names use
; StemName until the next call. Other registers preserved; CCR reflects D0.
; Zero length means an invalid path.
pathStem	.block
	movem.l d1-d2/a0/a2-a3, -(sp)
	lea SourcePath, a0
	movea.l a0, a2
	suba.l a3, a3
	moveq #0, d1
scanStem
	cmpi.w #PATH_BYTES, d1
	bhs.w stemBad
	moveq #0, d2
	move.b (a0)+, d2
	beq.w stemEnd
	cmpi.b #'/', d2
	beq.w separator
	cmpi.b #':', d2
	beq.w separator
	cmpi.b #'.', d2
	bne.w stemNext
	movea.l a0, a3
	subq.l #1, a3
	bra.w stemNext
separator
	movea.l a0, a2
	suba.l a3, a3
stemNext
	addq.w #1, d1
	bra.w scanStem
stemEnd
	move.l a3, d0
	beq.w stemBad
	move.l a2, d2
	sub.l d2, d0
	ble.w stemBad
	movea.l a2, a1
	cmpi.b #'0', (a1)
	blo.w stemDone
	cmpi.b #'9', (a1)
	bhi.w stemDone
	lea StemName, a0
	movea.l a0, a1
	move.b #'_', (a0)+
	move.l d0, d1
copyStem
	move.b (a2)+, (a0)+
	subq.l #1, d1
	bne.w copyStem
	clr.b (a0)
	addq.l #1, d0
	bra.w stemDone
stemBad
	moveq #0, d0
stemDone
	movem.l (sp)+, d1-d2/a0/a2-a3
	tst.l d0
	rts
	.bend  ; pathStem

; Inspect one bounded line. Long irrelevant lines are ignored by the indexer;
; the full frontend retains its ordinary size/error checks for selected files.
indexLine	.block
	tst.l IndexOverflow
	bne.w skip
	lea DeclarationBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	movea.l LineBuffer, a1
	move.l LineUsed, d0
	move.l SourceOrdinal, d1
	jsr declarations.line
	bra.w done
skip
	moveq #0, d0
done
	clr.l LineUsed
	clr.l IndexOverflow
	tst.l d0
	rts
	.bend  ; indexLine

; Retry numeric graph ordering after each selected file. D0=0 when complete,
; 2 with SourceOrdinal set to the next required file, 1 for invalid graph.
resolveGraph	.block
	.CAPTURE_LIFE_PHASE #capture_life.CONFIGURATION
	movem.l d2-d7/a0-a6, -(sp)
retry
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Capacity(a0), d0
	lea Front, a0
	; The discovery graph owns configuration IDs. A ready prefix can now be
	; prepared before the next missing dependency is acquired.
	movea.l a1, a2
	movea.l frontend.Frame.Scratch(a0), a1
	adda.l #frontend.SCOPE_STATE, a1
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	; Private publication must also consume the final dependency-complete set.
	; orderRemaining seals that set, so yielding begins before that call. Once
	; every yielded module is acknowledged, orderNext closes with zero spans.
	; Reference and bounded-disk policies retain their original schedule.
	tst.w ReferenceModules
	bne.w referenceOrder
	tst.l CaptureStore+capture_store.State.Configured
	bne.w referenceOrder
	jsr graph.orderNext
	cmpi.l #graph.READY, d0
	beq.w publish
	tst.l d0
	beq.w closed
	bra.w unresolved
referenceOrder
	jsr graph.orderRemaining
	beq.w closed
	cmpi.l #1, d0
	beq.w bad
	; Bounded capture already removes accumulated payload residency. Keep one
	; growing preparation owner there: overlapping sessions cost more than the
	; capture budget can save. Resident capture benefits from early retirement.
	tst.l CaptureStore+capture_store.State.Configured
	bne.w unresolved
	; Until configuration closes, publish a dependency-complete prefix rather
	; than accumulating every file while later dependencies are discovered.
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a2
	move.l memory.Block.Capacity(a0), d0
	lea Front, a0
	movea.l frontend.Frame.Scratch(a0), a1
	adda.l #frontend.SCOPE_STATE, a1
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	jsr graph.orderNext
	cmpi.l #graph.READY, d0
	beq.w publish
	tst.l d0
	bne.w unresolved
closed
	move.l d1, OrderedCount
	bra.w done
publish
	bsr.w publishCapturedModule
	bne.w bad
	bra.w retry
unresolved
	cmpi.l #3, d0
	bne.w missing
	bsr.w configureCapturedModule
	bne.w bad
	bra.w retry
missing
	cmpi.l #2, d0
	bne.w bad
	bsr.w selectCandidate
	bne.w bad
	moveq #2, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; resolveGraph

; D1=missing numeric module index+1. Look up its spelling in the preparation
; store, then retain an owned spelling for the serial candidate-file selection.
; D0/CCR=status; other registers preserved.
selectCandidate	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d1, d7
	beq.w bad
	move.l d7, RequestedModule
	subq.l #1, d7
	lea Front, a0
	movea.l frontend.Frame.Scratch(a0), a5
	adda.l #frontend.SCOPE_STATE, a5
	cmp.w layout.State.Count(a5), d7
	bhs.w bad
	move.l d7, d0
	movem.l a0-a1, -(sp)
	lea layout.LEXICAL_STATE(a5), a0
	lea LexicalEntry, a1
	jsr lexical.readEntry
	movem.l (sp)+, a0-a1
	bne.w bad
	lea LexicalEntry, a4
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	beq.w bad
	move.l d0, RequestedNameBytes
	move.l records.Entry.Name(a4), d1
	move.l d0, -(sp)
	move.l d0, d2
	move.l d1, d0
	move.l d2, d1
	lea layout.LEXICAL_STATE(a5), a0
	lea RequestedName, a1
	jsr lexical.copyName
	movem.l (sp)+, d0
	bne.w bad
	lea DeclarationBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	jsr declarations.find
	bne.w bad
	move.l d2, SelectedFileDerived
	move.l d1, d7
	beq.w bad
	cmp.l CandidateCount, d7
	bhi.w bad
	move.l d7, SourceOrdinal
	clr.l SelectionState
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; selectCandidate

; Retire physical discovery after its final configuration/selection consumer.
; Captured replay uses retained origin paths and include roots, not candidates
; or declaration lookup. Also used by final cleanup and non-captured inputs.
; No inputs; preserves all registers, CCR unspecified. Safe after partial setup
; and repeated calls; never releases the paths needed by active source includes.
releaseDiscovery	.block
	move.l a0, -(sp)
	lea CandidatePaths, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l CandidateCount
	lea DiscoveryBlock, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l DiscoveryScratch
	lea InputPlan, a0
	clr.l inputs.Frame.Scratch(a0)
	lea DeclarationBlock, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movea.l (sp)+, a0
	rts
	.bend  ; releaseDiscovery
