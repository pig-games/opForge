; Real-DOS page fault, mutable writeback and lease ownership component checks.
; @opforge-owner: opforge.tests.assembler.prepared_store_probe
	.module opforge.tests.assembler.prepared_store_probe
	.cpu 68020
	.use opforge.assembler.prepare.record_store as store
	.use opforge.assembler.source.record_view as views
	.use opforge.tests.assembler.prepared_store_reuse_probe as reuse
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
PROBE_FAILED = 88
CLEANUP_FAILED = 89
RECORD_BYTES = 256
RECORDS_PER_PAGE = store.PAGE_BYTES/RECORD_BYTES
PAGE_COUNT = 5
	.pub
	.section code, kind=code
; No inputs. D0/CCR=status; all other registers preserved. Always cleans up.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #PROBE_FAILED, d4
	lea State, a0
	jsr store.cleanup
	movea.l 4.w, a6
	lea DosName, a1
	moveq #36, d0
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	movea.l d0, a1
	lea State, a0
	lea Scratch, a2
	jsr store.configure
	bne.w finish
	moveq #0, d6
appendLoop
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	bne.w finish
	move.l d6, d5
	lsl.l #8, d5
	cmp.l d5, d1
	bne.w finish
	cmpi.l #store.PAYLOAD_BUDGET, store.State.Reserved(a0)
	bhi.w finish
	addq.l #1, d6
	cmpi.l #PAGE_COUNT*RECORDS_PER_PAGE, d6
	blo.w appendLoop
	jsr store.seal
	bne.w finish
	; Mutate an evicted page. Seal cannot write a still-pinned mutable view.
	moveq #0, d1
	jsr store.acquire
	bne.w finish
	cmpi.l #1, d3
	bne.w finish
	cmpi.l #store.PAGE_BYTES, d2
	bne.w finish
	move.b #99, (a1)
	move.l d3, d1
	jsr store.commitView
	bne.w finish
	jsr store.seal
	beq.w finish
	jsr store.releaseView
	bne.w finish
	jsr store.seal
	bne.w finish
	; Fault all other pages, evicting the edit; a new fault must retain it.
	moveq #1, d6
faultLoop
	move.l d6, d1
	lsl.l #8, d1
	lsl.l #7, d1
	jsr store.acquire
	bne.w finish
	move.l d3, d1
	jsr store.releaseView
	bne.w finish
	addq.l #1, d6
	cmpi.l #PAGE_COUNT, d6
	blo.w faultLoop
	moveq #0, d1
	jsr store.acquire
	bne.w finish
	cmpi.b #99, (a1)
	bne.w finish
	bsr.w verifyViews
	bne.w finish
	move.l a1, d7
	; Four pins fill the budget. Failed fault and append commit no new bytes.
	moveq #1, d6
pinLoop
	move.l d6, d1
	lsl.l #8, d1
	lsl.l #7, d1
	jsr store.acquire
	bne.w finish
	addq.l #1, d6
	cmpi.l #4, d6
	blo.w pinLoop
	move.l #4*store.PAGE_BYTES, d1
	jsr store.acquire
	beq.w finish
	move.l a1, d0
	or.l d2, d0
	or.l d3, d0
	bne.w finish
	cmpi.l #store.FAILURE_BUDGET, store.State.Failure(a0)
	bne.w finish
	cmpi.l #store.PAYLOAD_BUDGET, store.State.Reserved(a0)
	bne.w finish
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	beq.w finish
	cmpi.l #PAGE_COUNT*store.PAGE_BYTES, store.State.LogicalUsed(a0)
	bne.w finish
	moveq #4, d1
	jsr store.releaseView
	bne.w finish
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	bne.w finish
	cmpi.l #PAGE_COUNT*store.PAGE_BYTES, d1
	bne.w finish
	movea.l d7, a1
	cmpi.b #99, (a1)
	bne.w finish
	moveq #0, d4
finish
	lea View, a0
	jsr views.finish
	lea State, a0
	jsr store.cleanup
	movea.l a0, a1
	move.w #store.STATE_BYTES/4-1, d3
cleared
	tst.l (a1)+
	bne.w cleanupFailed
	dbra d3, cleared
	jsr store.cleanup
	tst.l d4
	bne.w closeDos
	movea.l DosBase, a0
	jsr reuse.run
	beq.w closeDos
	moveq #PROBE_FAILED, d4
	bra.w closeDos
cleanupFailed
	moveq #CLEANUP_FAILED, d4
closeDos
	movea.l DosBase, a1
	movea.l 4.w, a6
	jsr CLOSE_LIBRARY(a6)
	clr.l DosBase
done
	move.l d4, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run

; The edited first physical page follows the last physical page in this borrowed
; module schedule. Empty modules duplicate boundaries. A three-page module must
; still expose only one physical page per seek; no payload copying is involved.
; D0/CCR=status; other registers preserved. Always releases the view lease.
verifyViews	.block
	movem.l d1-d3/a0-a2, -(sp)
	lea View, a0
	lea State, a1
	jsr views.initStore
	bne.w failed
	lea MappedSpans, a1
	lea MappedStarts, a2
	moveq #5, d0
	move.l #PAGE_COUNT*store.PAGE_BYTES, d1
	jsr views.map
	bne.w failed
	moveq #0, d1
	moveq #42, d3
	jsr views.seek
	bne.w failed
	cmpi.l #42, d3
	bne.w failed
	tst.l d1
	bne.w failed
	tst.b (a1)
	bne.w failed
	cmpi.l #store.PAGE_BYTES, d2
	bne.w failed
	; The last of the duplicated starts selects the nonempty first raw page.
	move.l #store.PAGE_BYTES, d1
	jsr views.seek
	bne.w failed
	cmpi.b #99, (a1)
	bne.w failed
	cmpi.l #store.PAGE_BYTES, d2
	bne.w failed
	addi.l #RECORD_BYTES, d1
	jsr views.seek
	bne.w failed
	cmpi.l #store.PAGE_BYTES-RECORD_BYTES, d2
	bne.w failed
	cmp.l views.Frame.Offset(a0), d1
	bne.w failed
	; This module crosses three physical pages; availability stops at each page.
	move.l #2*store.PAGE_BYTES, d1
	jsr views.seek
	bne.w failed
	cmpi.l #store.PAGE_BYTES, d2
	bne.w failed
	move.l #3*store.PAGE_BYTES, d1
	jsr views.seek
	bne.w failed
	cmpi.l #store.PAGE_BYTES, d2
	bne.w failed
	jsr views.finish
	bne.w failed
	; Finish preserves mapping and logical extent for reuse.
	cmpi.l #PAGE_COUNT*store.PAGE_BYTES, views.Frame.Bytes(a0)
	bne.w failed
	move.l #store.PAGE_BYTES, d1
	jsr views.seek
	bne.w failed
	cmpi.b #99, (a1)
	bne.w failed
	move.l #PAGE_COUNT*store.PAGE_BYTES, d1
	jsr views.seek
	cmpi.l #views.STATUS_EOF, d0
	bne.w failed
	tst.l d2
	bne.w failed
	move.l a1, d0
	bne.w failed
	addq.l #1, d1
	jsr views.seek
	cmpi.l #views.STATUS_BAD, d0
	bne.w failed
	; Buffer initialization releases the old store lease and clears mapping.
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr views.initBuffer
	bne.w failed
	tst.l views.Frame.Count(a0)
	bne.w failed
	move.l #RECORD_BYTES/2, d1
	jsr views.seek
	bne.w failed
	lea Source+RECORD_BYTES/2, a2
	cmpa.l a2, a1
	bne.w failed
	cmpi.l #RECORD_BYTES/2, d2
	bne.w failed
	jsr views.changed
	bne.w failed
	moveq #0, d3
	bra.w finish
failed
	moveq #1, d3
finish
	lea View, a0
	jsr views.finish
	tst.l d3
	beq.w done
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; verifyViews
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
Scratch	.byte "Work:", 0
	.align 4
MappedSpans
	.long 4*store.PAGE_BYTES, 5*store.PAGE_BYTES, 1
	.long 0, 0, 1
	.long 0, store.PAGE_BYTES, 1
	.long 0, 0, 1
	.long store.PAGE_BYTES, 4*store.PAGE_BYTES, 1
MappedStarts	.long 0, store.PAGE_BYTES, store.PAGE_BYTES, 2*store.PAGE_BYTES, 2*store.PAGE_BYTES
	.endsection
	.section bss, kind=bss
DosBase	.res long, 1
State	.res byte, store.STATE_BYTES
View	.res byte, views.FRAME_BYTES
Source	.res byte, RECORD_BYTES
	.endsection
	.endmodule
