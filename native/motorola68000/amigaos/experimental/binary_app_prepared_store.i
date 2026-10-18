; Module spans own ranges of the prepared store; global identities remain a
; build-local bridge. Complete each global phase over bounded pages before the
; next phase. No page pointer escapes a callback or survives a view transition.
; Included in binary_app so source acquisition and output share one owner.
completePreparedStore	.block
	lea PreparedStore, a0
	jsr prepared_store.seal
	bne.w done
	movea.l a0, a1
	lea PreparedView, a0
	jsr views.initStore
	bne.w done
	lea Front, a0
	jsr frontend.completeBegin
	bne.w done
	lea frontend.completeLayouts, a5
	bsr.w visitPreparedPages
	bne.w done
	lea Front, a0
	jsr frontend.completeLayoutsEnd
	bne.w done
	lea frontend.completeOutputs, a5
	bsr.w visitPreparedPages
	bne.w done
	lea Front, a0
	jsr frontend.completeBindings
	bne.w done
	lea frontend.completeRecords, a5
	bsr.w visitPreparedPages
	bne.w done
	lea Front, a0
	jsr frontend.completeEnd
done
	tst.l d0
	rts
	.bend  ; completePreparedStore

; A5=frontend batch callback (A0Frame,A1page,D0bytes). Each physical page
; ends between records. Edits preserve extents and are persisted before eviction.
; D0/CCR=status; other registers preserved. No leases remain on return.
visitPreparedPages	.block
	movem.l d1-d3/a0-a2, -(sp)
	moveq #0, d1
next
	lea PreparedView, a0
	jsr views.seek
	cmpi.l #2, d0
	beq.w good
	tst.l d0
	bne.w done
	move.l d2, d3
	move.l d2, d0
	lea Front, a0
	jsr (a5)
	bne.w done
	lea PreparedView, a0
	jsr views.changed
	bne.w done
	add.l d3, d1
	bra.w next
good
	moveq #0, d0
done
	move.l d0, -(sp)
	lea PreparedView, a0
	jsr views.finish
	move.l (sp)+, d1
	tst.l d1
	bne.w failed
	tst.l d0
	bne.w failed
	lea PreparedStore, a0
	jsr prepared_store.seal
	bra.w exit
failed
	moveq #1, d0
exit
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; visitPreparedPages

; Establish the dependency-ordered module view without copying record payload.
; Retire names at the same frontier as frontend.indexBlocks. The numeric index
; has independent ownership and survives release of the frontend scratch.
indexPreparedStore	.block
	movem.l d1-d2/a0-a4, -(sp)
	tst.l GraphMode
	beq.w viewReady
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a1
	lea ModuleStarts, a0
	movea.l memory.Block.Pointer(a0), a2
	move.l OrderedCount, d0
	move.l Records+memory.Block.Used, d1
	lea PreparedView, a0
	jsr views.map
	bne.w done
viewReady
	movea.l Front+frontend.Frame.Scratch, a2
	adda.l #frontend.SCOPE_STATE, a2
	lea scopes.ARENA(a2), a0
	jsr nameStore.release
	clr.l layout.State.ArenaUsed(a2)
	lea PreparedView, a0
	lea PreparedIndex, a1
	move.l Records+memory.Block.Used, d0
	jsr blocks.indexView
	bne.w done
	move.l d1, PreparedCount
	beq.w good
	tst.l GraphMode
	beq.w good
	move.w #STEP_SELECT, PrepStep
	movea.l PreparedIndex+memory.Block.Pointer, a1
	movea.l Front+frontend.Frame.Graph, a3
	movea.l Front+frontend.Frame.Package, a4
	move.l Records+memory.Block.Used, d0
	jsr blocks.selectView
	bne.w done
	.MEMORY_PROGRESS DosBase, #PROGRESS_BLOCK_WORK, blocks.QueueAdds, blocks.Scanned, blocks.Marks
good
	moveq #0, d0
done
	move.l d0, -(sp)
	lea PreparedView, a0
	jsr views.finish
	move.l (sp)+, d1
	tst.l d1
	beq.w exit
	moveq #1, d0
exit
	movem.l (sp)+, d1-d2/a0-a4
	tst.l d0
	rts
	.bend  ; indexPreparedStore

; Frontend/template/lexical scratch has already retired. Reconstruct exactly one
; contiguous execution stream, then apply retained whole-block reachability.
; The assembly engine keeps its existing no-growth frontier and record format.
materializePreparedStore	.block
	movem.l d1-d4/a0-a2, -(sp)
	lea Records, a0
	move.l memory.Block.Used(a0), d3
	move.l d3, d0
	; Used is a logical extent until this first allocation, not a live prefix.
	clr.l memory.Block.Used(a0)
	move.l #RECORD_LIMIT, d1
	jsr memory.reserveBounded
	move.l d0, d4
	move.l d3, memory.Block.Used(a0)
	tst.l d4
	bne.w done
	movea.l memory.Block.Pointer(a0), a2
	moveq #0, d1
next
	lea PreparedView, a0
	jsr views.seek
	cmpi.l #2, d0
	beq.w copied
	tst.l d0
	bne.w done
	move.l d2, d4
	movea.l a1, a0
	movea.l a2, a1
	move.l d2, d0
	bsr.w copy
	adda.l d4, a2
	add.l d4, d1
	bra.w next
copied
	lea PreparedView, a0
	jsr views.finish
	bne.w done
	tst.l GraphMode
	beq.w releaseStore
	move.l PreparedCount, d1
	beq.w releaseStore
	movea.l Records+memory.Block.Pointer, a0
	move.l Records+memory.Block.Used, d0
	movea.l PreparedIndex+memory.Block.Pointer, a1
	jsr blocks.applySelection
	bne.w done
releaseStore
	lea PreparedIndex, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PreparedStore, a0
	jsr prepared_store.cleanup
	lea GraphSpans, a0
	jsr memory.release
	moveq #0, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; materializePreparedStore
