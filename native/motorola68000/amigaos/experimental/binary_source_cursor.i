; Selected source acquisition and resumable token cursors. Included by binary_app.
; Callbacks only queue a bounded request; no frontend routine reenters itself.
SourceCursorState	.struct
Start	.long ?
Offset	.long ?
Next	.long ?
End	.long ?
.endstruct
SOURCE_CURSOR_BYTES = SourceCursorState.End+4
SourceResume	.struct
Cursor	.res SOURCE_CURSOR_BYTES
ScheduleCursor	.long ?
ScheduleEnd	.long ?
CaptureRegion	.long ?
CaptureDerived	.long ?
TemplateFloor	.long ?
Owned	.word ?
RootFile	.word ?
ExpansionFloor	.word ?
Padding	.word ?
.endstruct
SOURCE_RESUME_BYTES = SourceResume.Padding+2

; A0=logical region,D1=global capture offset. D0/CCR=status,A0=borrowed
; physical memory.Block,D1=local handle,D2=global chunk/region end. Others kept.
; Release using the original offset after use. Pinned bytes stay resident;
; the directory descriptor expires on append, so retain no descriptor across it.
captureView	.block
	movem.l d3-d4/a1, -(sp)
	move.l d1, d3
	move.l capture.Region.End(a0), d4
	lea CaptureStore, a0
	jsr capture_store.acquire
	bne.w done
	movea.l a1, a0
	addq.l #1, d1
	add.l d3, d2
	cmp.l d4, d2
	bls.w done
	move.l d4, d2
done
	movem.l (sp)+, d3-d4/a1
	; Clamping changes CCR; the status remains the resolver's D0.
	tst.l d0
	rts
	.bend  ; captureView

; Release an acquired capture view by its original global offset in D1.
; All registers and CCR survive so failed frontend status remains authoritative.
releaseCaptureView	.block
	move.w ccr, -(sp)
	movem.l d0/a0, -(sp)
	lea CaptureStore, a0
	jsr capture_store.releaseView
	movem.l (sp)+, d0/a0
	move.w (sp)+, ccr
	rts
	.bend  ; releaseCaptureView

; A0=session,D0=0 view / 1 seek,D1=logical capture offset for seek.
; D0/CCR=status,D1=current offset,D2=end,D3=stream identity (0 physical).
; Other registers preserved.
sourceCursor	.block
	move.l d4, -(sp)
	move.l d0, d4
	jsr frontend.expansionDepth
	cmp.w frontend.Frame.ExpansionFloor(a0), d0
	bls.w physical
	move.l d4, d0
	jsr frontend.expansionCursor
	move.l (sp)+, d4
	tst.l d0
	rts
physical
	move.l d4, d0
	move.l (sp)+, d4
	cmpi.l #1, d0
	beq.w seek
	tst.l d0
	bne.w bad
	bra.w view
seek
	cmp.l SourceCursorState.Start+SourceCursorData, d1
	blo.w bad
	cmp.l SourceCursorState.End+SourceCursorData, d1
	bhi.w bad
	move.l d1, SourceCursorState.Next+SourceCursorData
view
	moveq #0, d3
	move.l SourceCursorState.Offset+SourceCursorData, d1
	move.l SourceCursorState.End+SourceCursorData, d2
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; sourceCursor

; A0=session,D1=logical capture offset. D0/CCR=status,A1=numeric writer
; preview,D1=next offset. Other registers preserved. Next peek replaces the view.
sourcePeek	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
	jsr frontend.expansionDepth
	cmp.w frontend.Frame.ExpansionFloor(a0), d0
	bls.w physical
	lea PeekRecord, a2
	jsr frontend.previewExpansion
	bra.w done
physical
	cmp.l SourceCursorState.Start+SourceCursorData, d1
	blo.w bad
	cmp.l SourceCursorState.End+SourceCursorData, d1
	bhs.w bad
	move.l d1, d6
	bsr.w locateCapture
	bne.w bad
	bsr.w captureView
	bne.w bad
	movea.l a0, a4
	move.l d1, d5
	jsr capture.resolve
	bne.w pinnedBad
	move.l capture.Record.Bytes(a1), d7
	add.l d6, d7
	bcs.w pinnedBad
	cmp.l SourceCursorState.End+SourceCursorData, d7
	bhi.w pinnedBad
	lea Front, a0
	movea.l a4, a1
	move.l d5, d1
	lea PeekRecord, a2
	jsr frontend.previewCapture
	bne.w pinnedBad
	move.l d7, d1
	moveq #0, d0
	bra.w unpin
pinnedBad
	moveq #1, d0
unpin
	move.l d1, -(sp)
	move.l d6, d1
	bsr.w releaseCaptureView
	move.l (sp)+, d1
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; sourcePeek

; Drain the current frontend result and expansions above TemplateFloor.
; Selected child sources run before the suspended caller template resumes.
; D0/CCR=status; other registers preserved.
drainPrepared	.block
	movem.l d1-d7/a0-a6, -(sp)
published
	lea Front, a0
	tst.w frontend.Frame.SourcePending(a0)
	beq.w append
	.WORK_CALL lazy_work.sampleEnd
	bsr.w materializePendingSource
	bne.w bad
	.WORK_SAMPLE_BEGIN OriginId
	bra.w expansion
append
	tst.l frontend.Frame.Used(a0)
	beq.w expansion
	.MEMORY_DETAIL_BEGIN #3
	bsr.w appendPrepared
	.MEMORY_DETAIL_END #3
	bne.w bad
expansion
	lea Front, a0
	jsr frontend.expansionDepth
	cmp.l TemplateFloor, d0
	bls.w good
	lea Front, a0
	jsr frontend.nextExpansion
	bne.w bad
	bra.w published
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; drainPrepared

; Replay [ScheduleCursor,ScheduleEnd) using owned captures, never original text.
; SourceCursorData is the active bounded frontier. D0/CCR=status; others preserved.
replaySourceSlice	.block
	movem.l d1-d7/a0-a6, -(sp)
next
	move.l ScheduleCursor, d5
	move.l d5, d1
	cmp.l ScheduleEnd, d1
	beq.w good
	bhi.w bad
	move.l d1, SourceCursorState.Offset+SourceCursorData
	bsr.w locateCapture
	bne.w bad
	bsr.w captureView
	bne.w bad
	movea.l a0, a4
	jsr capture.resolve
	bne.w pinnedBad
	move.l capture.Record.Bytes(a1), d7
	move.l ScheduleCursor, d0
	add.l d7, d0
	bcs.w pinnedBad
	cmp.l ScheduleEnd, d0
	bhi.w pinnedBad
	move.l d0, SourceCursorState.Next+SourceCursorData
	move.l capture.Record.Origin(a1), OriginId
	moveq #0, d0
	move.w capture.Record.Line(a1), d0
	move.l d0, SourceLine
	move.l d1, d6
	move.l OriginId, d1
	bsr.w captureSourcePath
	bne.w pinnedBad
	move.l d6, d1
	lea Front, a0
	movea.l a4, a1
	jsr frontend.replayLine
	move.l d5, d1
	bsr.w releaseCaptureView
	tst.l d0
	bne.w bad
	bsr.w drainPrepared
	bne.w bad
	.WORK_CALL lazy_work.sampleEnd
	move.l SourceCursorState.Next+SourceCursorData, ScheduleCursor
	bra.w next
pinnedBad
	move.l d5, d1
	bsr.w releaseCaptureView
	bra.w bad
good
	moveq #0, d0
	bra.w done
bad
	.WORK_CALL lazy_work.sampleEnd
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; replaySourceSlice

; Acquire one selected request, capture its entire token frontier, then prepare
; it in the caller's current module/scope. Registers and parent cursor survive.
; D0/CCR=status. Handles close on success; execute owns failure cleanup.
materializePendingSource	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.l #SOURCE_RESUME_BYTES, sp
	movea.l sp, a5
	.LAZY_PIN 1
	lea SourceCursorData, a0
	lea SourceResume.Cursor(a5), a1
	moveq #SOURCE_CURSOR_BYTES, d0
	bsr.w copy
	move.l ScheduleCursor, SourceResume.ScheduleCursor(a5)
	move.l ScheduleEnd, SourceResume.ScheduleEnd(a5)
	move.l CaptureRegionCurrent, SourceResume.CaptureRegion(a5)
	move.l CaptureDerived, SourceResume.CaptureDerived(a5)
	move.l TemplateFloor, SourceResume.TemplateFloor(a5)
	lea Front, a0
	move.w frontend.Frame.SourceOwned(a0), SourceResume.Owned(a5)
	move.w frontend.Frame.RootFile(a0), SourceResume.RootFile(a5)
	move.w frontend.Frame.ExpansionFloor(a0), SourceResume.ExpansionFloor(a5)
	jsr frontend.expansionDepth
	move.l d0, TemplateFloor
	move.w d0, frontend.Frame.ExpansionFloor(a0)
	bsr.w enterPendingSource
	bne.w bad
	lea Front, a0
	clr.w frontend.Frame.SourcePending(a0)
	move.w #1, frontend.Frame.SourceOwned(a0)
	move.l CaptureBytes, ScheduleCursor
	clr.l CaptureDerived
	bsr.w newCaptureRegion
	bne.w bad
	move.l #1, AcquiringSource
read
	lea Stream, a0
	movea.l LineBuffer, a1
	move.l #LINE_BYTES, d0
	jsr line_input.next
	move.l d1, LineUsed
	cmpi.l #line_input.EOF, d0
	beq.w captured
	tst.l d0
	bne.w bad
	bsr.w capturePhysicalLine
	bne.w bad
	addq.l #1, SourceLine
	bra.w read
captured
	tst.l LineUsed
	beq.w close
	bsr.w capturePhysicalLine
	bne.w bad
close
	clr.l AcquiringSource
	bsr.w closeSource
	bne.w bad
	move.l CaptureBytes, ScheduleEnd
	move.l ScheduleCursor, SourceCursorState.Start+SourceCursorData
	move.l ScheduleEnd, SourceCursorState.End+SourceCursorData
	bsr.w replaySourceSlice
	bne.w bad
	bsr.w popInclude
	bne.w bad
	tst.l ModulePipeline
	beq.w published
	lea CaptureStore, a0
	move.l SourceCursorState.Start+SourceCursorData, d1
	move.l SourceCursorState.End+SourceCursorData, d2
	cmp.l d1, d2
	beq.w published
	jsr capture_store.retire
	bne.w bad
published
	moveq #0, d0
	bra.w restore
bad
	clr.l AcquiringSource
	moveq #1, d0
restore
	.LAZY_PIN -1
	move.l d0, d7
	lea SourceResume.Cursor(a5), a0
	lea SourceCursorData, a1
	moveq #SOURCE_CURSOR_BYTES, d0
	bsr.w copy
	move.l SourceResume.ScheduleCursor(a5), ScheduleCursor
	move.l SourceResume.ScheduleEnd(a5), ScheduleEnd
	move.l SourceResume.CaptureRegion(a5), CaptureRegionCurrent
	move.l SourceResume.CaptureDerived(a5), CaptureDerived
	move.l SourceResume.TemplateFloor(a5), TemplateFloor
	lea Front, a0
	move.w SourceResume.Owned(a5), frontend.Frame.SourceOwned(a0)
	move.w SourceResume.RootFile(a5), frontend.Frame.RootFile(a0)
	move.w SourceResume.ExpansionFloor(a5), frontend.Frame.ExpansionFloor(a0)
	clr.l frontend.Frame.Used(a0)
	move.l d7, d0
	adda.l #SOURCE_RESUME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; materializePendingSource
