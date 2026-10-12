; Private caller-frame/pool bounds. Shared inputs owns span/overlap arithmetic.
; D0=status/CCR; helpers preserve other registers unless explicitly noted.
; A0/D0=frame/available bytes,D1=required bytes.
frame	.block
	movem.l d1-d2, -(sp)
	cmp.l d1, d0
	blo.w invalid
	move.l a0, d2
	andi.l #3, d2
	bne.w invalid
	move.l d1, d0
	jsr inputs.span
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1-d2
	rts
	.bend  ; frame
; A4=zeroed/prior owned State. Validate metadata before reading any pool bytes.
checkState	.block
	movem.l d1-d3/a0-a1, -(sp)
	movea.l a4, a0
	moveq #STATE_BYTES, d0
	moveq #STATE_BYTES, d1
	bsr.w frame
	bne.w done
	movea.l a4, a1
	moveq #4, d3
next
	move.l memory.Block.Capacity(a1), d2
	cmpi.l #memory.LIMIT, d2
	bhi.w invalid
	move.l memory.Block.Used(a1), d0
	cmp.l d2, d0
	bhi.w invalid
	movea.l (a1), a0  ; memory.Block.Pointer (zero offset)
	move.l a0, d0
	andi.l #3, d0
	bne.w invalid
	move.l d2, d0
	jsr inputs.span
	bne.w done
	adda.w #BLOCK_BYTES, a1
	dbra d3, next
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1-d3/a0-a1
	rts
	.bend  ; checkState
; A0/D0=readonly span,A4=State. Disjoint from mutable fixed State fields.
readonly	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.l d0, d2
	jsr inputs.span
	bne.w done
	move.l d2, d0
	movea.l a4, a1
	moveq #STATE_BYTES, d1
	jsr inputs.separate
done
	tst.l d0
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; readonly
; A0/D0=span,A4=State. Disallow borrowing any owner pool across its mutation.
poolsSeparate	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l d0, d2
	movea.l a0, a2
	movea.l a4, a1
	moveq #4, d3
next
	move.l a1, -(sp)
	move.l memory.Block.Capacity(a1), d1
	movea.l (a1), a1  ; memory.Block.Pointer (zero offset)
	movea.l a2, a0
	move.l d2, d0
	jsr inputs.separate
	movea.l (sp)+, a1
	bne.w done
	adda.w #BLOCK_BYTES, a1
	dbra d3, next
	moveq #abi.OK, d0
done
	tst.l d0
	movem.l (sp)+, d1-d3/a0-a2
	rts
	.bend  ; poolsSeparate
readonlyPools	.block
	move.l d2, -(sp)
	move.l d0, d2
	bsr.w readonly
	bne.w done
	move.l d2, d0
	bsr.w poolsSeparate
done
	tst.l d0
	movem.l (sp)+, d2
	rts
	.bend  ; readonlyPools
; A4=State,A5=PrepareFrame. Validate all borrowed inputs before reset/allocation.
prepareCheck	.block
	bsr.w checkState
	bne.w done
	tst.l PrepareFrame.StepBudget(a5)
	bmi.w invalid
	movea.l a5, a0
	moveq #PREPARE_FRAME_BYTES, d0
	bsr.w readonlyPools
	bne.w done
	movea.l PrepareFrame.Definitions(a5), a0
	move.l #store.STATE_BYTES, d0
	bsr.w readonlyPools
	bne.w done
	movea.l PrepareFrame.CallerScope(a5), a0
	move.l #scopes.SCRATCH_BYTES, d0
	bsr.w readonlyPools
	bne.w done
	movea.l PrepareFrame.Program(a5), a0
	move.l PrepareFrame.ProgramBytes(a5), d0
	bsr.w readonlyPools
	bne.w done
	move.l PrepareFrame.HeadCount(a5), d0
	cmpi.l #$03ffffff, d0
	bhi.w invalid
	lsl.l #5, d0  ; abi.TOKEN_BYTES
	movea.l PrepareFrame.HeadViews(a5), a0
	bsr.w readonlyPools
	bne.w done
	movea.l PrepareFrame.HeadArena(a5), a0
	move.l PrepareFrame.HeadArenaBytes(a5), d0
	bsr.w readonlyPools
	bne.w done
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	rts
	.bend  ; prepareCheck
; A0/D0=mutable caller output span,A4=State,A5=SelectFrame.
output	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.l d0, d2
	bsr.w readonly
	bne.w done
	move.l d2, d0
	bsr.w poolsSeparate
	bne.w done
	move.l d2, d0
	movea.l a5, a1
	moveq #SELECT_FRAME_BYTES, d1
	jsr inputs.separate
done
	tst.l d0
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; output
; A4=State,A5=SelectFrame. Pure selector owns inter-output/input disjointness.
selectCheck	.block
	movea.l a5, a0
	moveq #SELECT_FRAME_BYTES, d0
	bsr.w readonly
	bne.w done
	move.l SelectFrame.Count(a5), d0
	cmpi.l #$03ffffff, d0
	bhi.w invalid
	lsl.l #5, d0
	movea.l SelectFrame.Views(a5), a0
	bsr.w readonly
	bne.w done
	movea.l SelectFrame.Arena(a5), a0
	move.l SelectFrame.ArenaBytes(a5), d0
	bsr.w readonly
	bne.w done
	movea.l SelectFrame.Output(a5), a0
	move.l SelectFrame.OutputCapacity(a5), d0
	bsr.w output
	bne.w done
	movea.l SelectFrame.Fallback(a5), a0
	move.l SelectFrame.FallbackCapacity(a5), d0
	bsr.w output
	bne.w done
	movea.l SelectFrame.Workspace(a5), a0
	move.l SelectFrame.WorkspaceCapacity(a5), d0
	bsr.w output
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	rts
	.bend  ; selectCheck
