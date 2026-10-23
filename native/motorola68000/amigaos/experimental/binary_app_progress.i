; CLI-only presentation. No callback or progress state enters semantic execution.
; Public app entry frames select quiet independently of debug telemetry.
beginProgress .block
	movem.l d0/a0-a1, -(sp)
	lea BuildProgress, a0
	move.l DosBase, progress.Frame.DosBase(a0)
	clr.w progress.Frame.Active(a0)
	clr.w progress.Frame.Enabled(a0)
	lea ModuleProgress, a1
	move.l DosBase, progress.Frame.DosBase(a1)
	clr.w progress.Frame.Active(a1)
	clr.w progress.Frame.Enabled(a1)
	cmpi.w #2, CliMode
	bne.w done
	tst.w CliQuiet
	bne.w done
	move.w #1, progress.Frame.Enabled(a0)
	move.w #1, progress.Frame.Enabled(a1)
	lea IncrementalRootPath, a1
	moveq #progress.PREPARING, d0
	jsr progress.begin
done
	movem.l (sp)+, d0/a0-a1
	rts
	.bend ; beginProgress

; Complete one private attempt; a fallback is deferred, never called assembled.
; D0=attempt status,D1=handled. All registers and incoming CCR preserved.
finishModuleProgress .block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea ModuleProgress, a0
	tst.l d0
	bne.w failed
	moveq #progress.DEFERRED, d0
	tst.l d1
	beq.w report
	moveq #progress.REUSED, d0
	tst.w PrivateReused
	bne.w report
	moveq #progress.ASSEMBLED, d0
	bra.w report
failed
	moveq #progress.FAILED, d0
report
	jsr progress.finish
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend ; finishModuleProgress

; End after output and cleanup. A pending root/link phase failed; successfully
; linked output was already closed. All registers/CCR preserved, including DOS.
finishProgress .block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea ModuleProgress, a0
	moveq #progress.FAILED, d0
	jsr progress.finish
	lea BuildProgress, a0
	tst.l ReturnCode
	bne.w report
	moveq #progress.FINISHED, d0
report
	jsr progress.finish
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend ; finishProgress

; Ordinary shared replay has no private module name publication. Label its
; captured source path without changing diagnostic cursors or semantic owners.
; A0=Span. D0/CCR=replay status; other registers preserved. No borrowed pointer
; survives begin, which copies the path into its bounded presentation frame.
replayReportedSpan .block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	move.l Span.File(a4), d1
	lsl.l #8, d1
	move.l d1, d0
	addi.l #PATH_BYTES, d0
	bcs.w replay
	cmp.l OriginPaths+memory.Block.Used, d0
	bhi.w replay
	movea.l OriginPaths+memory.Block.Pointer, a1
	adda.l d1, a1
	lea ModuleProgress, a0
	moveq #progress.PREPARING, d0
	jsr progress.begin
replay
	movea.l a4, a0
	bsr.w replayCapturedSpan
	move.l d0, d7
	lea ModuleProgress, a0
	moveq #progress.PREPARED, d0
	tst.l d7
	beq.w report
	moveq #progress.FAILED, d0
report
	jsr progress.finish
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; replayReportedSpan
