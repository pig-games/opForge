; Dependency-first publication during discovery. Configuration and semantic
; sessions have independent owners. Only the active Front owns grammar control.
; A published span owns numeric records/templates; its lexical captures retire.
; Binding proxies and placement recipes remain live until global completion.

; Begin semantic ownership without destroying ongoing configuration/discovery.
; Returns with configuration active and semantic state in ConfigFront.
; D0/CCR=status; other registers preserved. Partial owners use normal cleanup.
beginModulePipeline	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea Front, a0
	lea ConfigFront, a1
	move.l #frontend.FRAME_BYTES, d0
	bsr.w copy
	move.l #1, ConfigFrontStarted
	lea Front, a0
	clr.l frontend.Frame.Scratch(a0)
	clr.l frontend.Frame.Graph(a0)
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
	clr.l frontend.Frame.Source(a0)
	clr.l frontend.Frame.SourceBytes(a0)
	clr.l frontend.Frame.Graph(a0)
	jsr frontend.begin
	bne.w bad
	bsr.w configureLexicalStorage
	bne.w bad
	jsr source_control.begin
	bne.w bad
	lea SemanticGraph, a0
	move.l #frontend.GRAPH_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	lea Front, a0
	jsr frontend.beginGraph
	bne.w bad
	bsr.w switchModuleSession
	bne.w bad
	move.l #1, ModulePipeline
	.SOURCE_CAPTURE_SNAPSHOT CAPTURE_ACQUISITION
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; beginModulePipeline

; Swap session frames, then restore the selected package's shared VM control.
; Owned scratch/graph allocations never move. No borrowed line crosses this call.
; D0/CCR=status; other registers preserved.
switchModuleSession	.block
	movem.l d1-d3/a0-a1, -(sp)
	lea Front, a0
	lea ConfigFront, a1
	move.l #frontend.FRAME_BYTES/4, d3
next
	move.l (a0), d1
	move.l (a1), d2
	move.l d2, (a0)+
	move.l d1, (a1)+
	subq.l #1, d3
	bne.w next
	lea Front, a0
	jsr frontend.activate
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; switchModuleSession

; GraphSpans holds the next dependency-complete raw capture extent.
; Prepare it with the shared frontend, publish owned records, then retire source.
; Discovery resumes with configuration active. D0/CCR=status; others preserved.
publishCapturedModule	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l SourceOrdinal, -(sp)
	move.l OriginId, -(sp)
	move.l SourceLine, -(sp)
	move.l CaptureRegionCurrent, -(sp)
	move.l CaptureDerived, -(sp)
	lea GraphSpans, a0
	movea.l memory.Block.Pointer(a0), a0
	lea PublishedCapture, a1
	moveq #SPAN_BYTES, d0
	bsr.w copy
	tst.l ModulePipeline
	bne.w begun
	bsr.w beginModulePipeline
	bne.w bad
begun
	bsr.w switchModuleSession
	bne.w bad
	.CAPTURE_REPLAY_BEGIN
	bsr.w seedModuleConfiguration
	bne.w semanticBad
	bsr.w tryPrivateModule
	bne.w semanticBad
	tst.l d1
	bne.w published
	; replayCapturedSpan uses the active semantic graph, not the discovery graph.
	lea GraphBlock, a0
	lea SemanticGraph, a1
	bsr.w swapModuleGraphs
	lea PublishedCapture, a0
	bsr.w replayReportedSpan
	move.l d0, d7
	lea GraphBlock, a0
	lea SemanticGraph, a1
	bsr.w swapModuleGraphs
	tst.l d7
	bne.w semanticBad
published
	bsr.w switchModuleSession
	bne.w bad
	.CAPTURE_CONFIGURATION_BEGIN
	lea CaptureStore, a0
	move.l PublishedCapture+Span.Start, d1
	move.l PublishedCapture+Span.End, d2
	cmp.l d1, d2
	beq.w acknowledge
	jsr capture_store.retire
	bne.w bad
acknowledge
	lea GraphBlock, a0
	movea.l memory.Block.Pointer(a0), a0
	jsr graph.acknowledge
	bne.w bad
	moveq #0, d0
	bra.w restore
semanticBad
	move.l d0, d7
	bsr.w switchModuleSession
	move.l d7, d0
	bra.w failedLocation
bad
	moveq #1, d0
failedLocation
	; Keep the failed semantic origin; restore acquisition ownership only.
	move.l (sp)+, CaptureDerived
	move.l (sp)+, CaptureRegionCurrent
	adda.w #12, sp
	bra.w done
restore
	move.l (sp)+, CaptureDerived
	move.l (sp)+, CaptureRegionCurrent
	move.l (sp)+, SourceLine
	move.l (sp)+, OriginId
	move.l (sp)+, SourceOrdinal
	move.l SourceOrdinal, d1
	bsr.w captureSourcePath
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; publishCapturedModule

; Transfer appended configuration into the active semantic session. The dormant
; ConfigFront owns its snapshots. Existing observations are never overwritten.
; D0/CCR=status; other registers preserved.
seedModuleConfiguration	.block
	movem.l d1-d2/a0-a1, -(sp)
	lea ConfigFront, a0
	movea.l frontend.Frame.Scratch(a0), a0
	adda.l #frontend.SCOPE_STATE, a0
	lea Front, a1
	movea.l frontend.Frame.Scratch(a1), a1
	adda.l #frontend.SCOPE_STATE, a1
	move.l SeededParameters, d0
	move.l SeededMaps, d1
	jsr scopes.seedConfigurationRange
	bne.w done
	move.l d1, SeededParameters
	move.l d2, SeededMaps

done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; seedModuleConfiguration

; A0/A1=Block descriptors. Swap ownership; all registers preserved, CCR unspecified.
swapModuleGraphs	.block
	movem.l d0-d2/a0-a1, -(sp)
	moveq #(memory.Block.Used+4)/4-1, d2
next
	move.l (a0), d0
	move.l (a1), d1
	move.l d1, (a0)+
	move.l d0, (a1)+
	dbra d2, next
	movem.l (sp)+, d0-d2/a0-a1
	rts
	.bend  ; swapModuleGraphs

; Discovery exhausted: retain the remaining module schedule, transfer final
; configuration, then retire that owner BEFORE preparing the remaining bodies.
; Earlier published records/templates stay owned. Global binding/layout still
; run once against the complete numeric universe. D0/CCR=status; others kept.
finishModulePipeline	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea GraphSpans, a0
	lea CaptureOrder, a1
	move.l #memory.Block.Used+4, d0
	bsr.w copy
	lea GraphSpans, a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	move.l OrderedCount, CaptureOrderCount
	move.l OrderedCount, d0
	mulu.w #SPAN_BYTES, d0
	move.l d0, CaptureOrder+memory.Block.Used
	bsr.w switchModuleSession
	bne.w bad
	bsr.w seedModuleConfiguration
	bne.w bad
	lea ConfigFront, a0
	jsr frontend.finish
	clr.l ConfigFrontStarted
	clr.l ConfigFront+frontend.Frame.Scratch
	lea PrepBlock, a0
	jsr memory.release
	bsr.w releaseGraph
	lea GraphBlock, a0
	lea SemanticGraph, a1
	bsr.w swapModuleGraphs
	bsr.w releaseDiscovery
	lea Front, a0
	jsr frontend.activate
	bne.w bad
	lea GraphSpans, a0
	move.l #frontend.GRAPH_SPAN_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	.CAPTURE_REPLAY_BEGIN
	clr.l CaptureOrderIndex
next
	move.l CaptureOrderIndex, d0
	cmp.l CaptureOrderCount, d0
	bhs.w complete
	mulu.w #SPAN_BYTES, d0
	lea CaptureOrder, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	bsr.w replayReportedSpan
	bne.w bad
	move.l Span.Start(a0), d1
	move.l Span.End(a0), d2
	cmp.l d1, d2
	beq.w advance
	lea CaptureStore, a0
	jsr capture_store.retire
	bne.w bad
advance
	addq.l #1, CaptureOrderIndex
	bra.w next
complete
	.SOURCE_CAPTURE_SNAPSHOT CAPTURE_REPLAY
	bsr.w releaseCapturedSources
	clr.l OrderedCount
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finishModulePipeline
