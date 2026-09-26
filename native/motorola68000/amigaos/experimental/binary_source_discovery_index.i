; Preparation-only discovery selection. Every candidate is read once for
; declaration names; only reachable files enter the binary frontend.

; D0/CCR=status; other registers preserved. Paths remain owned by DiscoveryBlock.
indexCandidates .block
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
	.bend ; indexCandidates

; Return A1/D0 as the basename without its final extension. SourcePath is
; bounded and remains preparation-only. Zero length means an invalid path.
pathStem .block
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
	bra.w stemDone
stemBad
	moveq #0, d0
stemDone
	movem.l (sp)+, d1-d2/a0/a2-a3
	rts
	.bend ; pathStem

; Inspect one bounded line. Long irrelevant lines are ignored by the indexer;
; the full frontend retains its ordinary size/error checks for selected files.
indexLine .block
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
	.bend ; indexLine

; Retry numeric graph ordering after each selected file. D0=0 when complete,
; 2 with SourceOrdinal set to the next required file, 1 for invalid graph.
resolveGraph .block
	movem.l d2-d7/a0-a6, -(sp)
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Capacity(a0), d0
	lea Front, a0
	jsr frontend.orderGraph
	tst.l d0
	bne.w unresolved
	move.l d1, OrderedCount
	bra.w done
unresolved
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
	.bend ; resolveGraph

; D1=missing numeric module index+1. Look up its spelling in the preparation
; arena, then select its unique candidate file. No name survives preparation.
; D0/CCR=status; other registers preserved.
selectCandidate .block
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
	lsl.l #4, d0
	lea layout.ENTRIES(a5), a4
	adda.l d0, a4
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	beq.w bad
	move.l d0, RequestedNameBytes
	movea.l layout.ARENA_POINTER(a5), a1
	moveq #0, d1
	move.w records.Entry.Name(a4), d1
	adda.l d1, a1
	move.l a1, RequestedName
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
	.bend ; selectCandidate
