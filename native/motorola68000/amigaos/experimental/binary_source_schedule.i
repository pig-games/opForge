; Owned source capture, configuration discovery and dependency-first preparation.
; Included by binary_app; all storage belongs to execute's cleanup path.

; Prepare the selected physical file's capture region and index-only boundary.
; D0/CCR=status; other registers preserved. No module body is prepared here.
captureFileBegin .block
	movem.l d1-d7/a0-a6, -(sp)
	lea ConfigScan, a0
	move.l #Front, configuration.Frame.Session(a0)
	lea Front, a1
	movea.l frontend.Frame.Scratch(a1), a1
	adda.l #frontend.SCOPE_STATE, a1
	move.l a1, configuration.Frame.Scope(a0)
	lea GraphBlock, a1
	move.l memory.Block.Pointer(a1), configuration.Frame.Graph(a0)
	move.l #frontend.bindCapture, configuration.Frame.BindCapture(a0)
	lea Front, a1
	move.l frontend.Frame.Output(a1), configuration.Frame.Output(a0)
	move.l frontend.Frame.Capacity(a1), configuration.Frame.Capacity(a0)
	clr.w configuration.Frame.Priority(a0)
	cmpi.l #1, SourceOrdinal
	bne.w indexMode
	move.w #1, configuration.Frame.Priority(a0)
indexMode
	jsr configuration.begin
	bne.w bad
	clr.l CaptureDerived
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	move.l SourceOrdinal, graph.GraphState.SourceIndex(a0)
	tst.l SelectedFileDerived
	beq.w region
	bsr.w pathStem
	beq.w bad
	movea.l a1, a0
	lea ConfigScan, a1
	movea.l configuration.Frame.Scope(a1), a1
	jsr scopes.bind
	bne.w bad
	lea ConfigScan, a0
	movea.l configuration.Frame.Scope(a0), a1
	sub.w layout.State.Base(a1), d1
	bcs.w bad
	addq.l #1, d1
	move.l d1, CaptureDerived
	jsr configuration.indexDerived
	bne.w bad
region
	bsr.w newCaptureRegion
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; captureFileBegin

; Allocate a descriptor, initially empty. Arenas grow only up to memory.LIMIT.
; D0/CCR=status; other registers preserved. Descriptors contain offsets only.
newCaptureRegion .block
	movem.l d1-d3/a0-a2, -(sp)
	lea CaptureRegions, a0
	move.l memory.Block.Used(a0), d0
	addi.l #CAPTURE_REGION_BYTES, d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	move.l CaptureRegionCount, CaptureRegionCurrent
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l CaptureBytes, CaptureRegion.Base(a1)
	move.l CaptureBytes, CaptureRegion.End(a1)
	move.l SourceOrdinal, CaptureRegion.File(a1)
	move.l CaptureDerived, CaptureRegion.Derived(a1)
	clr.l CaptureRegion.Arena+memory.Block.Pointer(a1)
	clr.l CaptureRegion.Arena+memory.Block.Capacity(a1)
	clr.l CaptureRegion.Arena+memory.Block.Used(a1)
	addi.l #CAPTURE_REGION_BYTES, memory.Block.Used(a0)
	addq.l #1, CaptureRegionCount
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend ; newCaptureRegion

; D0=current descriptor index, A0=descriptor; no allocation occurs here.
; Clobbers D0/A0/CCR. The caller reacquires this view after table growth.
currentCaptureRegion .block
	move.l CaptureRegionCurrent, d0
	mulu.w #CAPTURE_REGION_BYTES, d0
	lea CaptureRegions, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	rts
	.bend ; currentCaptureRegion

; Capture one already selected physical line. The temporary line arena makes
; rollover independent of allocator failures. It is copied once, then reused.
; D0/CCR=status; other registers preserved; SourceLine remains caller-owned.
capturePhysicalLine .block
	movem.l d1-d7/a0-a6, -(sp)
	lea Front, a0
	move.l OriginId, frontend.Frame.Origin(a0)
	move.l LineBuffer, frontend.Frame.Source(a0)
	move.l LineUsed, d0
	beq.w trimmed
	movea.l LineBuffer, a1
	cmpi.b #13, -1(a1, d0.l)
	bne.w trimmed
	subq.l #1, d0
trimmed
	move.l d0, frontend.Frame.SourceBytes(a0)
	move.l SourceLine, d0
	jsr frontend.setLine
	bne.w bad
	lea CaptureLine, a1
	clr.l memory.Block.Used(a1)
	lea CaptureRequest, a1
	move.l #CaptureLine, capture.Frame.Arena(a1)
	jsr frontend.captureLine
	bne.w bad
	lea CaptureLine, a0
	move.l memory.Block.Used(a0), d7
	bsr.w currentCaptureRegion
	move.l CaptureRegion.Arena+memory.Block.Used(a0), d6
	move.l d6, d0
	add.l d7, d0
	bcs.w bad
	cmpi.l #memory.LIMIT, d0
	bls.w fits
	bsr.w newCaptureRegion
	bne.w bad
	bsr.w currentCaptureRegion
	moveq #0, d6
fits
	lea CaptureRegion.Arena(a0), a4
	move.l d6, d0
	add.l d7, d0
	movea.l a4, a0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a4), a1
	adda.l d6, a1
	lea CaptureLine, a0
	movea.l memory.Block.Pointer(a0), a0
	move.l d7, d0
	bsr.w copy
	add.l d7, memory.Block.Used(a4)
	move.l CaptureBytes, d0
	add.l d7, d0
	bcs.w bad
	move.l d0, CaptureBytes
	bsr.w currentCaptureRegion
	move.l CaptureBytes, CaptureRegion.End(a0)
	lea CaptureRegion.Arena(a0), a1
	lea ConfigScan, a0
	move.l a1, configuration.Frame.Arena(a0)
	move.l d6, configuration.Frame.Cursor(a0)
	move.l memory.Block.Used(a1), configuration.Frame.End(a0)
	jsr configuration.index
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; capturePhysicalLine

; End one physical capture file. D0/CCR=status; other registers preserved.
captureFileEnd .block
	movem.l a0, -(sp)
	lea ConfigScan, a0
	tst.l CaptureDerived
	beq.w validate
	jsr configuration.endDerived
	bne.w done
validate
	jsr configuration.endIndex
	bne.w done
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	jsr graph.endFile
done
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend ; captureFileEnd

; D1=logical capture offset. A0=containing region on success, D0/CCR=status.
; Preserve other registers. Empty regions never match a record lookup.
locateCapture .block
	movem.l d2-d3/a1, -(sp)
	lea CaptureRegions, a0
	movea.l memory.Block.Pointer(a0), a0
	move.l CaptureRegionCount, d2
next
	tst.l d2
	beq.w bad
	cmp.l CaptureRegion.Base(a0), d1
	blo.w bad
	cmp.l CaptureRegion.End(a0), d1
	blo.w good
	bne.w advance
	cmp.l CaptureRegion.Base(a0), d1
	bne.w advance
	move.l ScheduleFile, d3
	cmp.l CaptureRegion.File(a0), d3
	beq.w good
advance
	adda.w #CAPTURE_REGION_BYTES, a0
	subq.l #1, d2
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d3/a1
	tst.l d0
	rts
	.bend ; locateCapture

; D1=retained physical origin index+1. Restore its path for module basename,
; active assets and diagnostics. D0/CCR=status; other registers preserved.
captureSourcePath .block
	movem.l d1-d3/a0-a2, -(sp)
	tst.l d1
	beq.w bad
	lsl.l #8, d1
	move.l d1, d0
	addi.l #PATH_BYTES, d0
	lea OriginPaths, a0
	cmp.l memory.Block.Used(a0), d0
	bhi.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d1, a1
	lea SourcePath, a0
	move.w #PATH_BYTES-1, d2
copyPath
	move.b (a1)+, (a0)+
	dbra d2, copyPath
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend ; captureSourcePath

; D1=reachable graph binding. Configure all of its captured regions before
; graph traversal reads its edges. No struct, instruction or asset is executed.
; D0/CCR=status; other registers preserved.
configureCapturedModule .block
	movem.l d1-d7/a0-a6, -(sp)
	lea ConfigScan, a0
	move.l d1, configuration.Frame.Binding(a0)
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	jsr graph.nodeForBinding
	bne.w bad
	move.l graph.Node.Start(a0), ScheduleCursor
	move.l graph.Node.End(a0), ScheduleEnd
	moveq #0, d1
	move.w graph.Node.File(a0), d1
	move.l d1, ScheduleFile
	move.l d1, SourceOrdinal
	bsr.w captureSourcePath
	bne.w bad
	move.l ScheduleCursor, d1
	bsr.w locateCapture
	bne.w bad
	move.l CaptureRegion.Derived(a0), ScheduleDerived
	beq.w beginScan
	bsr.w pathStem
	beq.w bad
	lea ConfigScan, a0
	movea.l configuration.Frame.Scope(a0), a0
	jsr scopes.beginFileDerived
	bne.w bad
beginScan
	lea ConfigScan, a0
	jsr configuration.beginConfiguration
	bne.w bad
part
	move.l ScheduleCursor, d1
	cmp.l ScheduleEnd, d1
	beq.w endScan
	bsr.w locateCapture
	bne.w bad
	move.l CaptureRegion.End(a0), d7
	cmp.l ScheduleEnd, d7
	bls.w bounded
	move.l ScheduleEnd, d7
bounded
	move.l d7, d2
	sub.l CaptureRegion.Base(a0), d1
	sub.l CaptureRegion.Base(a0), d2
	lea CaptureRegion.Arena(a0), a1
	lea ConfigScan, a0
	move.l a1, configuration.Frame.Arena(a0)
	move.l d1, configuration.Frame.Cursor(a0)
	move.l d2, configuration.Frame.End(a0)
	jsr configuration.configurePart
	beq.w scanned
	bsr.w locateConfigurationFailure
	bra.w bad
scanned
	move.l d7, ScheduleCursor
	bra.w part
endScan
	lea ConfigScan, a0
	movea.l configuration.Frame.Scope(a0), a0
	jsr scopes.endFileDerived
	bne.w bad
	moveq #0, d0
	jsr scopes.endFile
	bne.w bad
	lea ConfigScan, a0
	jsr configuration.endConfiguration
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; configureCapturedModule

; Keep the selected capture line's real origin when configuration rejects it.
; Preserve all registers; CCR unspecified. An invalid capture retains the
; requested module/file location already supplied by configureCapturedModule.
locateConfigurationFailure .block
	movem.l d0-d2/a0-a2, -(sp)
	lea ConfigScan, a0
	move.l configuration.Frame.Cursor(a0), d1
	addq.l #1, d1
	movea.l configuration.Frame.Arena(a0), a0
	jsr capture.resolve
	bne.w done
	move.l capture.Record.Origin(a1), OriginId
	move.l capture.Record.Origin(a1), SourceOrdinal
	move.l capture.Record.Line(a1), SourceLine
	move.l capture.Record.Origin(a1), d1
	bsr.w captureSourcePath
done
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend ; locateConfigurationFailure

; Resolve the captured graph, including non-discovery manifests. Discovery
; callers use resolveGraph, which handles the missing-file status separately.
; D0/CCR=status; other registers preserved.
orderCapturedSources .block
	movem.l d1-d7/a0-a6, -(sp)
retry
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Capacity(a0), d0
	lea Front, a0
	jsr frontend.orderGraph
	beq.w good
	cmpi.l #3, d0
	bne.w bad
	bsr.w configureCapturedModule
	bne.w bad
	bra.w retry
good
	move.l d1, OrderedCount
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; orderCapturedSources

; Replace the configuration frontend with fresh semantic state. Canonical
; parameter spellings are rebound before the configuration scope is released.
; D0/CCR=status; other registers preserved. Old IO/output storage stays live.
beginCapturedReplay .block
	movem.l d1-d7/a0-a6, -(sp)
	lea Front, a0
	lea ConfigFront, a1
	move.l #frontend.FRAME_BYTES, d0
	bsr.w copy
	lea Front, a0
	movea.l frontend.Frame.Package(a0), a0
	jsr frontend.scratchSize
	bne.w bad
	move.l d1, d0
	lea SemanticScratch, a0
	jsr memory.reserveExact
	bne.w bad
	move.l memory.Block.Pointer(a0), d1
	lea Front, a0
	move.l d1, frontend.Frame.Scratch(a0)
	move.l #1, ConfigFrontStarted
	clr.l frontend.Frame.Source(a0)
	clr.l frontend.Frame.SourceBytes(a0)
	; Ownership now belongs to ConfigFront, even if fresh begin fails.
	move.l #1, FrontStarted
	jsr frontend.begin
	bne.w bad
	lea ConfigFront, a0
	movea.l frontend.Frame.Scratch(a0), a0
	adda.l #frontend.SCOPE_STATE, a0
	lea Front, a1
	movea.l frontend.Frame.Scratch(a1), a1
	adda.l #frontend.SCOPE_STATE, a1
	jsr scopes.seedConfiguration
	bne.w bad
	lea ConfigFront, a0
	jsr frontend.finish
	clr.l ConfigFrontStarted
	lea Front, a0
	jsr frontend.activate
	bne.w bad
	lea GraphSpans, a0
	lea CaptureOrder, a1
	move.l #memory.Block.Used+4, d0
	bsr.w copy
	lea GraphSpans, a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	move.l OrderedCount, CaptureOrderCount
	clr.l OrderedCount
	tst.l GraphMode
	beq.w ready
	move.l #frontend.GRAPH_SPAN_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	lea Front, a0
	jsr frontend.beginGraph
	bne.w bad
ready
	clr.l SpanCount
	lea FileSpans, a0
	clr.l memory.Block.Used(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; beginCapturedReplay

; Replay one capture span in graph order. Source ordinals remain physical;
; prepared offsets and semantic graph identities are rebuilt from fresh state.
; D0/CCR=status; other registers preserved.
replayCapturedSources .block
	movem.l d1-d7/a0-a6, -(sp)
	bsr.w beginCapturedReplay
	bne.w bad
	clr.l CaptureOrderIndex
nextSpan
	move.l CaptureOrderIndex, d0
	cmp.l CaptureOrderCount, d0
	bhs.w good
	mulu.w #SPAN_BYTES, d0
	lea CaptureOrder, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	move.l Span.Start(a0), ScheduleCursor
	move.l Span.End(a0), ScheduleEnd
	move.l Span.File(a0), ScheduleFile
	move.l Span.File(a0), SourceOrdinal
	move.l Span.File(a0), d1
	bsr.w captureSourcePath
	bne.w bad
	lea FileSpans, a0
	move.l memory.Block.Used(a0), d0
	addi.l #SPAN_BYTES, d0
	jsr memory.reserve
	bne.w bad
	bsr.w fileSpan
	lea Records, a1
	move.l memory.Block.Used(a1), Span.Start(a0)
	move.l ScheduleFile, Span.File(a0)
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	move.l ScheduleFile, graph.GraphState.SourceIndex(a0)
	move.l ScheduleCursor, d1
	bsr.w locateCapture
	bne.w bad
	move.l CaptureRegion.Derived(a0), ScheduleDerived
	beq.w nextRecord
	bsr.w pathStem
	beq.w bad
	lea Front, a0
	jsr frontend.beginFileDerived
	bne.w bad
nextRecord
	move.l ScheduleCursor, d1
	cmp.l ScheduleEnd, d1
	beq.w endSpan
	bsr.w locateCapture
	bne.w bad
	sub.l CaptureRegion.Base(a0), d1
	addq.l #1, d1
	lea CaptureRegion.Arena(a0), a4
	movea.l a4, a0
	jsr capture.resolve
	bne.w bad
	move.l capture.Record.Bytes(a1), d7
	move.l ScheduleEnd, d0
	sub.l ScheduleCursor, d0
	cmp.l d7, d0
	blo.w bad
	move.l capture.Record.Origin(a1), OriginId
	move.l capture.Record.Line(a1), SourceLine
	move.l capture.Record.Origin(a1), d2
	move.l d1, d6
	move.l d2, d1
	bsr.w captureSourcePath
	bne.w bad
	move.l d6, d1
	lea Front, a0
	movea.l a4, a1
	jsr frontend.replayLine
	bne.w bad
	.MEMORY_DETAIL_BEGIN #3
	bsr.w appendPrepared
	.MEMORY_DETAIL_END #3
	bne.w bad
expanded
	lea Front, a0
	jsr frontend.nextExpansion
	bne.w bad
	tst.l frontend.Frame.Used(a0)
	beq.w recordDone
	.MEMORY_DETAIL_BEGIN #3
	bsr.w appendPrepared
	.MEMORY_DETAIL_END #3
	bne.w bad
	bra.w expanded
recordDone
	add.l d7, ScheduleCursor
	bra.w nextRecord
endSpan
	lea Front, a0
	jsr frontend.endFileDerived
	bne.w bad
	moveq #0, d0
	jsr frontend.endFile
	bne.w bad
	bsr.w fileSpan
	lea Records, a1
	move.l memory.Block.Used(a1), Span.End(a0)
	lea FileSpans, a0
	addi.l #SPAN_BYTES, memory.Block.Used(a0)
	addq.l #1, SpanCount
	addq.l #1, CaptureOrderIndex
	bra.w nextSpan
good
	bsr.w releaseCapturedSources
	moveq #0, d0
	bra.w done
bad
	move.l OriginId, SourceOrdinal
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; replayCapturedSources

; A single CLI file without explicit modules keeps its normal semantic path.
; Its captured stream still replays once; indexing errors are then diagnosed
; by the ordinary frontend. D0/CCR=status; other registers preserved.
prepareCapturedSources .block
	movem.l d1-d3/a0-a2, -(sp)
	cmpi.w #1, CliMode
	bne.w ordered
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	tst.w graph.GraphState.Count(a0)
	bne.w ordered
	clr.l GraphMode
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a0
	clr.l Span.Start(a0)
	move.l CaptureBytes, Span.End(a0)
	move.l #1, Span.File(a0)
	move.l #1, OrderedCount
	bra.w replay
ordered
	tst.l OrderedCount
	bne.w replay
	bsr.w orderCapturedSources
	bne.w done
replay
	bsr.w replayCapturedSources
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend ; prepareCapturedSources

; Free all captured source ownership, including partially published regions.
; Preserve all registers; CCR unspecified. Safe on every execute exit.
releaseCapturedSources .block
	movem.l d0-d2/a0-a2, -(sp)
	lea CaptureRegions, a0
	movea.l memory.Block.Pointer(a0), a2
	move.l CaptureRegionCount, d2
next
	tst.l d2
	beq.w table
	lea CaptureRegion.Arena(a2), a0
	jsr memory.release
	adda.w #CAPTURE_REGION_BYTES, a2
	subq.l #1, d2
	bra.w next
table
	lea CaptureRegions, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l CaptureRegionCount
	clr.l CaptureRegionCurrent
	clr.l CaptureBytes
	lea CaptureLine, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea CaptureOrder, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l CaptureOrderCount
	movem.l (sp)+, d0-d2/a0-a2
	rts
	.bend ; releaseCapturedSources

; Finish both frontend ownership states before scratch allocation release.
; Preserve all registers; CCR unspecified. Called after Front's normal finish.
releaseScheduledFrontend .block
	movem.l a0, -(sp)
	tst.l ConfigFrontStarted
	beq.w scratch
	lea ConfigFront, a0
	jsr frontend.finish
	clr.l ConfigFrontStarted
scratch
	lea SemanticScratch, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, a0
	rts
	.bend ; releaseScheduledFrontend
