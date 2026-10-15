; Disposable component proof for owned first-use maps and normal-plan parity.
; @opforge-owner: tests.amigaos.binary_deferred_plan_probe
	.module tests.amigaos.binary_deferred_plan_probe
	.cpu 68020
	.use experimental.amigaos.binary_deferred_plans as deferred
	.use experimental.amigaos.binary_macro_plans as plans
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_memory as memory
	.use prvm.amigaos.abi as abi
	.include "deferred_plan_telemetry.i"
CAPTURE_OFFSET = $12340
	.section code, kind=code
	.pub
; Run isolated allocation, malformed-input, relocation, reuse and parity checks.
; D0/CCR=0 success/1 failure; all other registers preserved. Both paths release
; every owned allocation. This probe never changes production selection policy.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	bsr.w cleanup
	clr.l CallbackCount
	move.l #1, IdentityMap
	move.l #$00040008, ReplayMap
	; Construct an ordinary captured candidate through its owning constructor.
	lea SourcePlans, a0
	move.l a0, PlanFrame+plans.Frame.Arena
	lea RootRow, a0
	move.l a0, PlanFrame+plans.Frame.Events
	move.l #1, PlanFrame+plans.Frame.Count
	lea Spelling, a0
	move.l a0, PlanFrame+plans.Frame.Source
	move.l #1, PlanFrame+plans.Frame.SourceBytes
	lea IdentityMap, a0
	move.l a0, PlanFrame+plans.Frame.PackedMap
	move.l #1, PlanFrame+plans.Frame.TokenCount
	lea FragmentRow, a0
	move.l a0, PlanFrame+plans.Frame.RecipeEvents
	move.l #1, PlanFrame+plans.Frame.RecipeCount
	lea PlanFrame, a0
	jsr plans.create
	bne.w bad
	move.l d1, Candidate
	lea CaptureArena, a0
	move.l a0, CaptureFrame+capture.Frame.Arena
	lea SourcePlans, a0
	move.l a0, CaptureFrame+capture.Frame.Plans
	move.l #1, CaptureFrame+capture.Frame.SourceBytes
	move.l #1, CaptureFrame+capture.Frame.Origin
	move.l #1, CaptureFrame+capture.Frame.Line
	move.l d1, CaptureFrame+capture.Frame.LineHandle
	lea CaptureFrame, a0
	jsr capture.create
	bne.w bad
	move.l d1, CaptureHandle
	lea Owner, a0
	move.l a0, CreateFrame+deferred.CreateFrame.Arena
	lea SourcePlans, a0
	move.l a0, CreateFrame+deferred.CreateFrame.Source
	move.l #CAPTURE_OFFSET, CreateFrame+deferred.CreateFrame.Capture
	move.l Candidate, CreateFrame+deferred.CreateFrame.Handle
	lea ReplayMap, a0
	move.l a0, CreateFrame+deferred.CreateFrame.PackedMap
	move.l #1, CreateFrame+deferred.CreateFrame.TokenCount
	move.w #1, CreateFrame+deferred.CreateFrame.IncludeRecipes
	; Rejections must leave the empty recipe owner unpublished.
	move.l #capture.TOKEN_LIMIT+1, CreateFrame+deferred.CreateFrame.TokenCount
	bsr.w rejectCreate
	bne.w bad
	move.l #1, CreateFrame+deferred.CreateFrame.TokenCount
	move.w #257, ReplayMap+2
	bsr.w rejectCreate
	bne.w bad
	move.w #8, ReplayMap+2
	lea SourcePlans, a0
	move.l Candidate, d1
	jsr plans.resolve
	bne.w bad
	move.l plans.Plan.Recipes(a1), d2
	movea.l SourcePlans+memory.Block.Pointer, a2
	adda.l d2, a2
	clr.w plans.RecipeSet.Rows+plans.Row.Kind(a2)
	bsr.w rejectCreate
	bne.w bad
	move.w #abi.PRVM_RESULT_MACRO_LITERAL, plans.RecipeSet.Rows+plans.Row.Kind(a2)
	; The ordinary binder provides an independent materialized-plan oracle.
	lea Expected, a0
	move.l a0, BindFrame+plans.BindFrame.Arena
	lea SourcePlans, a0
	move.l a0, BindFrame+plans.BindFrame.Source
	move.l Candidate, BindFrame+plans.BindFrame.Handle
	lea ReplayMap, a0
	move.l a0, BindFrame+plans.BindFrame.PackedMap
	move.l #1, BindFrame+plans.BindFrame.TokenCount
	move.w #1, BindFrame+plans.BindFrame.IncludeRecipes
	lea BindFrame, a0
	jsr plans.bind
	bne.w bad
	move.l d1, ExpectedHandle
	lea CreateFrame, a0
	movem.l d2-d7/a0-a6, Before
	jsr deferred.create
	movem.l d2-d7/a0-a6, After
	bne.w bad
	move.l d1, Recipe
	bsr.w preserved
	bne.w bad
	; Overwrite the original map. Realization must consume the owned copy.
	move.l #$000c0010, ReplayMap
	; Relocate captured storage after retaining the numeric recipe offset.
	move.l CaptureArena+memory.Block.Pointer, d7
	lea CaptureArena, a0
	move.l #1024, d0
	jsr memory.reserve
	bne.w bad
	cmp.l CaptureArena+memory.Block.Pointer, d7
	beq.w bad
	lea Owner, a0
	move.l a0, RealizeFrame+deferred.RealizeFrame.Arena
	lea Actual, a0
	move.l a0, RealizeFrame+deferred.RealizeFrame.Plans
	move.l Recipe, RealizeFrame+deferred.RealizeFrame.Handle
	lea CaptureArena, a0
	move.l a0, RealizeFrame+deferred.RealizeFrame.Context
	move.l #resolveCapture, RealizeFrame+deferred.RealizeFrame.Resolve
	; A malformed relocated candidate must not publish output or cache.
	lea CaptureArena, a0
	move.l CaptureHandle, d1
	jsr capture.resolve
	bne.w bad
	jsr capture.planView
	lea plans.HEADER_BYTES(a0), a2
	move.l #2, plans.Row.PackedEnd(a2)
	lea RealizeFrame, a0
	jsr deferred.realize
	beq.w bad
	tst.l Actual+memory.Block.Used
	bne.w bad
	movea.l Owner+memory.Block.Pointer, a0
	tst.l deferred.Entry.Cache(a0)
	bne.w bad
	move.l #1, plans.Row.PackedEnd(a2)
	clr.l CallbackCount
	lea RealizeFrame, a0
	movem.l d2-d7/a0-a6, Before
	jsr deferred.realize
	movem.l d2-d7/a0-a6, After
	bne.w bad
	move.l d1, ActualHandle
	bsr.w preserved
	bne.w bad
	move.l Actual+memory.Block.Used, d7
	lea RealizeFrame, a0
	jsr deferred.realize
	bne.w bad
	cmp.l ActualHandle, d1
	bne.w bad
	cmp.l Actual+memory.Block.Used, d7
	bne.w bad
	cmpi.l #1, CallbackCount
	bne.w bad
	lea Expected, a0
	move.l ExpectedHandle, d1
	jsr plans.resolve
	bne.w bad
	movea.l a1, a5
	move.l plans.Plan.Bytes(a5), d7
	lea Actual, a0
	move.l ActualHandle, d1
	jsr plans.resolve
	bne.w bad
	cmp.l plans.Plan.Bytes(a1), d7
	bne.w bad
compare
	move.b (a5)+, d0
	cmp.b (a1)+, d0
	bne.w bad
	subq.l #1, d7
	bne.w compare
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	lea Owner, a0
	jsr deferred.getStats
	cmpi.l #1, d1
	bne.w bad
	cmpi.l #1, d2
	bne.w bad
	cmpi.l #1, d3
	bne.w bad
	cmpi.l #deferred.ENTRY_BYTES+4, d4
	bne.w bad
	; Passive telemetry preserves registers, CCR and stack exactly.
	lea Owner, a4
	movem.l d0-d7/a0-a6, TelemetryBefore
	move.l sp, SavedSp
	move.w #$15, ccr
	.DEFERRED_COUNTER a4, deferred.State.Hits, #1
	move.w ccr, Flags
	movem.l d0-d7/a0-a6, TelemetryAfter
	cmpi.w #$15, Flags
	bne.w bad
	move.l sp, d0
	cmp.l SavedSp, d0
	bne.w bad
	lea TelemetryBefore, a0
	lea TelemetryAfter, a1
	moveq #14, d2
telemetryPreserved
	move.l (a0)+, d0
	cmp.l (a1)+, d0
	bne.w bad
	dbra d2, telemetryPreserved
.endif
.endif
.endif
	bsr.w cleanup
	lea Owner, a0
	jsr deferred.begin
	tst.l memory.Block.Pointer(a0)
	bne.w bad
	tst.l memory.Block.Capacity(a0)
	bne.w bad
	tst.l memory.Block.Used(a0)
	bne.w bad
	jsr deferred.getStats
	or.l d2, d1
	or.l d3, d1
	or.l d4, d1
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	bsr.w cleanup
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
; Reject an invalid create and verify that no recipe was published.
; D0/CCR=status; other registers preserved.
rejectCreate	.block
	movem.l d1/a0, -(sp)
	lea CreateFrame, a0
	jsr deferred.create
	beq.w bad
	tst.l d1
	bne.w bad
	tst.l Owner+memory.Block.Used
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; rejectCreate
; Verify API-preserved registers D2-D7/A0-A6 after the recorded call.
preserved	.block
	movem.l d1-d2/a0-a1, -(sp)
	lea Before, a0
	lea After, a1
	moveq #12, d2
next
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w bad
	dbra d2, next
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; preserved
; A0=capture owner,D1=global numeric offset. Resolve fresh after relocation.
; D0/CCR=status,A1=borrowed record; all other registers preserved.
resolveCapture	.block
	cmpi.l #CAPTURE_OFFSET, d1
	bne.w bad
	move.l d1, -(sp)
	move.l CaptureHandle, d1
	jsr capture.resolve
	move.l (sp)+, d1
	tst.l d0
	bne.w done
	addq.l #1, CallbackCount
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; resolveCapture
; Release all probe-owned pools on either outcome; preserve registers.
cleanup	.block
	movem.l d0/a0, -(sp)
	lea Owner, a0
	jsr deferred.finish
	lea SourcePlans, a0
	jsr plans.finish
	lea Expected, a0
	jsr plans.finish
	lea Actual, a0
	jsr plans.finish
	lea CaptureArena, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, d0/a0
	rts
	.bend  ; cleanup
	.endsection
	.section data, kind=data
RootRow	.word abi.PRVM_RESULT_MACRO_LINE, 0
	.long 0, 1, 0, 1, 0, 0, -1
FragmentRow	.word abi.PRVM_RESULT_MACRO_LITERAL, 0
	.long 0, 0, 0, 1, 0, 0, 0
Spelling	.byte "x"
	.align 2
IdentityMap	.word 0, 1
ReplayMap	.word 4, 8
	.endsection
	.section bss, kind=bss
Owner	.res byte, deferred.STATE_BYTES
SourcePlans	.res byte, plans.STATE_BYTES
Expected	.res byte, plans.STATE_BYTES
Actual	.res byte, plans.STATE_BYTES
CaptureArena	.res byte, memory.Block.Used+4
PlanFrame	.res byte, plans.FRAME_BYTES
CaptureFrame	.res byte, capture.FRAME_BYTES
CreateFrame	.res byte, deferred.CREATE_FRAME_BYTES
RealizeFrame	.res byte, deferred.REALIZE_FRAME_BYTES
BindFrame	.res byte, plans.BIND_FRAME_BYTES
Candidate	.res long, 1
CaptureHandle	.res long, 1
Recipe	.res long, 1
ExpectedHandle	.res long, 1
ActualHandle	.res long, 1
CallbackCount	.res long, 1
Before	.res long, 13
After	.res long, 13
TelemetryBefore	.res long, 15
TelemetryAfter	.res long, 15
Flags	.res word, 1
SavedSp	.res long, 1
	.endsection
	.endmodule
