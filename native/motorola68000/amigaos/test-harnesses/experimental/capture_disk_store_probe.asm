; Disk cache component checks; real DOS IO, independent of target semantics.
; @opforge-owner: tests.amigaos.capture_disk_store_probe
	.module tests.amigaos.capture_disk_store_probe
	.cpu 68020
	.use experimental.amigaos.binary_capture_store as store
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_capture_io as io
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
DOS_LOCK = -84
DOS_UNLOCK = -90
ACCESS_READ = -2
BUDGET = 65536
RECORD_BYTES = 32768
RECORD_SHIFT = 15
PROBE_FAILED = 86
CLEANUP_FAILED = 87
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
	lea State, a0
	movea.l d0, a1
	lea Scratch, a2
	move.l #BUDGET, d0
	jsr store.configure
	bne.w finish
	lea store.State.IO+io.Context.Path(a0), a1
	move.l store.State.IO+io.Context.End(a0), d0
	sub.l a1, d0
	move.l d0, DirectoryLength
	lea SavedPath, a2
copyPath
	move.b (a1)+, (a2)+
	bne.w copyPath
	; Each row has byte(local offset + row index), distinguishing wrong seeks.
	moveq #0, d6
appendLoop
	move.l d6, d0
	bsr.w fillSource
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	bne.w finish
	move.l d6, d5
	mulu.w #RECORD_BYTES, d5
	cmp.l d5, d1
	bne.w finish
	cmpi.l #BUDGET, store.State.Reserved(a0)
	bhi.w finish
	addq.l #1, d6
	cmpi.l #3, d6
	blo.w appendLoop
	; Reload both sides of each row boundary, including the evicted first row.
	moveq #0, d6
verifyLoop
	move.l d6, d1
	bsr.w verifyByte
	bne.w finish
	add.l #RECORD_BYTES-1, d6
	move.l d6, d1
	bsr.w verifyByte
	bne.w finish
	addq.l #1, d6
	cmpi.l #3*RECORD_BYTES, d6
	blo.w verifyLoop
	; Two pinned rows consume the budget. Failed append must commit nothing.
	moveq #0, d1
	jsr store.acquire
	bne.w finish
	move.l memory.Block.Pointer(a1), d7
	move.l #RECORD_BYTES, d1
	jsr store.acquire
	bne.w finish
	moveq #3, d0
	bsr.w fillSource
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	beq.w finish
	cmpi.l #3*RECORD_BYTES, store.State.LogicalUsed(a0)
	bne.w finish
	move.l #RECORD_BYTES, d1
	jsr store.releaseView
	lea Source, a1
	move.l #RECORD_BYTES, d0
	jsr store.append
	bne.w finish
	cmpi.l #3*RECORD_BYTES, d1
	bne.w finish
	cmpi.l #BUDGET, store.State.Reserved(a0)
	bhi.w finish
	; Nested acquire and append preserve the pinned payload, even if rows move.
	movea.l d7, a2
	cmpi.b #0, (a2)
	bne.w finish
	cmpi.b #255, RECORD_BYTES-1(a2)
	bne.w finish
	moveq #0, d1
	jsr store.acquire
	bne.w finish
	cmp.l memory.Block.Pointer(a1), d7
	bne.w finish
	moveq #0, d1
	jsr store.releaseView
	jsr store.releaseView
	move.l #3*RECORD_BYTES, d1
	bsr.w verifyByte
	bne.w finish
	move.l #4*RECORD_BYTES-1, d1
	bsr.w verifyByte
	bne.w finish
	lea Source, a1
	move.l #BUDGET+1, d0
	jsr store.append
	beq.w finish
	cmpi.l #4*RECORD_BYTES, store.State.LogicalUsed(a0)
	bne.w finish
	moveq #0, d4
finish
	lea State, a0
	jsr store.cleanup
	movea.l a0, a1
	move.l #store.STATE_BYTES/4-1, d3
cleared
	tst.l (a1)+
	bne.w cleanupFailed
	dbra d3, cleared
	jsr store.cleanup
	tst.l d4
	bne.w closeDos
	; Neither the scratch file nor its private directory may survive cleanup.
	lea SavedPath, a1
	bsr.w expectAbsent
	bne.w cleanupFailed
	lea SavedPath, a1
	move.l DirectoryLength, d0
	clr.b 0(a1, d0.l)
	bsr.w expectAbsent
	bne.w cleanupFailed
	lea State, a0
	; A nonexistent parent must reject scratch setup and leave cleanup safe.
	movea.l DosBase, a1
	lea MissingScratch, a2
	move.l #BUDGET, d0
	jsr store.configure
	beq.w directoryFailed
	jsr store.cleanup
	bra.w closeDos
directoryFailed
	moveq #PROBE_FAILED, d4
	jsr store.cleanup
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

; D0=row index. Fill Source with byte(local offset + row index).
; D0 clobbered; other registers preserved; CCR unspecified.
fillSource	.block
	movem.l d3/a1, -(sp)
	lea Source, a1
	move.l #RECORD_BYTES-1, d3
fill
	move.b d0, (a1)+
	addq.b #1, d0
	dbra d3, fill
	movem.l (sp)+, d3/a1
	rts
	.bend  ; fillSource

; A0=State,D1=global offset. D0/CCR=status; D1-D3/A1-A2 clobbered.
; Release by original handle, not the local offset returned by acquire.
verifyByte	.block
	move.l d1, -(sp)
	jsr store.acquire
	bne.w failed
	movea.l memory.Block.Pointer(a1), a2
	move.l (sp), d3
	move.l d3, d0
	lsr.l #8, d0
	lsr.l #RECORD_SHIFT-8, d0
	add.b d0, d3
	cmp.b 0(a2, d1.l), d3
	bne.w releaseFailed
	move.l (sp), d3
	andi.l #RECORD_BYTES-1, d3
	cmp.l d3, d1
	bne.w releaseFailed
	neg.l d3
	addi.l #RECORD_BYTES, d3
	cmp.l d3, d2
	bne.w releaseFailed
	cmpi.l #BUDGET, store.State.Reserved(a0)
	bhi.w releaseFailed
	moveq #0, d0
	bra.w release
releaseFailed
	moveq #1, d0
release
	move.l (sp)+, d1
	jsr store.releaseView
	tst.l d0
	rts
failed
	addq.l #4, sp
	moveq #1, d0
	tst.l d0
	rts
	.bend  ; verifyByte
; A1=path. D0/CCR=0 only when absent. Preserves all other registers.
expectAbsent	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a1, d1
	moveq #ACCESS_READ, d2
	movea.l DosBase, a6
	jsr DOS_LOCK(a6)
	tst.l d0
	beq.w absent
	move.l d0, d1
	jsr DOS_UNLOCK(a6)
	moveq #1, d0
absent
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; expectAbsent
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
Scratch	.byte "Work:", 0
MissingScratch	.byte "Work:opforge-missing-parent/capture", 0
	.endsection
	.section bss, kind=bss
DosBase	.res long, 1
DirectoryLength	.res long, 1
SavedPath	.res byte, 512
State	.res byte, store.STATE_BYTES
Source	.res byte, RECORD_BYTES
	.endsection
	.endmodule
