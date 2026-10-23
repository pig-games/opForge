; Focused storage checks, independent of target instruction semantics.
; @opforge-owner: opforge.tests.assembler.capture_store_probe
	.module opforge.tests.assembler.capture_store_probe
	.cpu 68020
	.use opforge.assembler.source.capture_store as store
	.use opforge.platform.memory as memory
	.pub
	.section code, kind=code
; No inputs. D0/CCR=0 success,84 storage/handle failure,85 cleanup failure.
; Other registers preserved. Always releases probe allocations before returning.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea State, a0
	jsr store.cleanup
	lea Source, a1
	move.l #$12345678, (a1)
	move.l #4095, d0
	jsr store.append
	bne.w failed
	tst.l d1
	bne.w failed
	jsr store.resolve
	bne.w failed
	move.l memory.Block.Pointer(a1), d7
	cmpi.l #4095, d2
	bne.w failed
	; Enough rows to move the directory. Regions share the same global store;
	; no caller-region marker affects physical placement or handle allocation.
	moveq #1, d6
appendLoop
	lea Source, a1
	move.l #4095, d0
	jsr store.append
	bne.w failed
	move.l d6, d5
	lsl.l #8, d5
	lsl.l #4, d5
	sub.l d6, d5
	cmp.l d5, d1
	bne.w failed
	addq.l #1, d6
	cmpi.l #160, d6
	blo.w appendLoop
	cmpi.l #655200, store.State.LogicalUsed(a0)
	bne.w failed
	cmpi.l #655360, store.State.Reserved(a0)
	bne.w failed
	moveq #0, d1
	jsr store.resolve
	bne.w failed
	cmp.l memory.Block.Pointer(a1), d7
	bne.w failed
	cmpi.l #32760, d2
	bne.w failed
	movea.l d7, a2
	cmpi.l #$12345678, (a2)
	bne.w failed
	; Last byte of one chunk and first byte of the next are adjacent handles.
	move.l #32759, d1
	jsr store.resolve
	bne.w failed
	cmpi.l #32759, d1
	bne.w failed
	cmpi.l #1, d2
	bne.w failed
	move.l #32760, d1
	jsr store.resolve
	bne.w failed
	tst.l d1
	bne.w failed
	; A dedicated extent is exact (memory rounds allocation to eight bytes).
	lea Source, a1
	move.l #5000, d0
	jsr store.append
	bne.w failed
	cmpi.l #655200, d1
	bne.w failed
	jsr store.resolve
	bne.w failed
	cmpi.l #5000, memory.Block.Capacity(a1)
	bne.w failed
	cmpi.l #5000, d2
	bne.w failed
	; Small records following a dedicated record start a fresh shared chunk.
	lea Source, a1
	moveq #3, d0
	jsr store.append
	bne.w failed
	cmpi.l #660200, d1
	bne.w failed
	jsr store.resolve
	bne.w failed
	cmpi.l #3, d2
	bne.w failed
	cmpi.l #32768, memory.Block.Capacity(a1)
	bne.w failed
	move.l #660202, d1
	jsr store.resolve
	bne.w failed
	cmpi.l #2, d1
	bne.w failed
	cmpi.l #1, d2
	bne.w failed
	move.l #660203, d1
	jsr store.resolve
	beq.w failed
	moveq #-1, d1
	jsr store.resolve
	beq.w failed
	lea Source, a1
	moveq #0, d0
	jsr store.append
	beq.w failed
	move.l #memory.LIMIT+1, d0
	jsr store.append
	beq.w failed
	cmpi.l #660203, store.State.LogicalUsed(a0)
	bne.w failed
	; Three owners share one chunk and retire out of order. A failed lease
	; or overlapping retirement preserves ownership; appends keep global offsets.
	jsr store.cleanup
	moveq #3, d6
owners
	lea Source, a1
	moveq #4, d0
	jsr store.append
	bne.w failed
	subq.l #1, d6
	bne.w owners
	moveq #4, d1
	moveq #8, d2
	jsr store.retire
	bne.w failed
	cmpi.l #32768, store.State.Reserved(a0)
	bne.w failed
	moveq #0, d1
	jsr store.resolve
	bne.w failed
	cmpi.l #4, d2
	bne.w failed
	moveq #4, d1
	jsr store.resolve
	beq.w failed
	moveq #8, d1
	jsr store.acquire
	bne.w failed
	moveq #8, d1
	moveq #12, d2
	jsr store.retire
	beq.w failed
	jsr store.releaseView
	moveq #0, d1
	moveq #4, d2
	jsr store.retire
	bne.w failed
	jsr store.retire
	beq.w failed
	moveq #8, d1
	moveq #12, d2
	jsr store.retire
	bne.w failed
	tst.l store.State.Reserved(a0)
	bne.w failed
	moveq #8, d1
	jsr store.resolve
	beq.w failed
	lea Source, a1
	moveq #4, d0
	jsr store.append
	bne.w failed
	cmpi.l #12, d1
	bne.w failed
	jsr store.seal
	bne.w failed
	tst.l store.State.Active(a0)
	bne.w failed
	moveq #0, d4
	bra.w finish
failed
	moveq #84, d4
finish
	jsr store.cleanup
	movea.l a0, a1
	move.l #store.STATE_BYTES/4-1, d3
cleared
	tst.l (a1)+
	bne.w cleanupFailed
	dbra d3, cleared
	jsr store.cleanup
	move.l d4, d0
	bra.w done
cleanupFailed
	moveq #85, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.section bss, kind=bss
State	.res byte, store.STATE_BYTES
Source	.res byte, 5000
	.endsection
	.endmodule
