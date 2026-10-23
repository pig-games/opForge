; Closed preparation facets. All stored references are offsets or numeric IDs.
	.pub
ClosedHeader	.struct
Magic	.long ?
Bytes	.long ?
Definition	.long ?
Body	.long ?
BodyBytes	.long ?
Defaults	.long ?
DefaultBytes	.long ?
Text	.long ?
TextBytes	.long ?
Plans	.long ?
PlanBytes	.long ?
Roots	.long ?
RootCount	.long ?
	.endstruct
CLOSED_HEADER_BYTES = ClosedHeader.RootCount+4
CLOSED_MAGIC = $4f464d54; OFMT, current closed template contract
ExportFrame	.struct
State	.long ?
Handle	.long ?  ; one based in the selected native pool
External	.long ?  ; zero=macro/segment, one=external statement carrier
Destination	.long ?  ; empty owned memory.Block
	.endstruct
EXPORT_FRAME_BYTES = ExportFrame.Destination+4
SeedFrame	.struct
State	.long ?
Scope	.long ?
Image	.long ?
Bytes	.long ?
Name	.long ?  ; remapped canonical facet identity
	.endstruct
SEED_FRAME_BYTES = SeedFrame.Name+4
	.priv
ClosedMap	.struct
Source	.long ?
Target	.long ?
	.endstruct
CLOSED_MAP_BYTES = ClosedMap.Target+4
ClosedWork	.struct
Caller	.long ?
State	.long ?
Handle	.long ?
Definition	.res DEF_BYTES
SourcePlans	.long ?
TargetPlans	.long ?
View	.res BLOCK_BYTES
Body	.res BLOCK_BYTES
Defaults	.res BLOCK_BYTES
Text	.res BLOCK_BYTES
Plans	.res BLOCK_BYTES
Map	.res BLOCK_BYTES
Image	.res BLOCK_BYTES
DefaultBase	.long ?
TextBase	.long ?
	.endstruct
CLOSED_WORK_BYTES = ClosedWork.TextBase+4
	.endsection
	.section bss, kind=bss
ClosedBusy	.res long, 1
ClosedScratch	.res byte, CLOSED_WORK_BYTES
	.endsection
	.section code, kind=code
	.pub
; A0=ExportFrame. Copy one available closed facet and its selected plans.
; Numeric source identities remain for remapClosed; no source pointer survives.
; D0/CCR=status; others preserved. Success transfers Destination, failure leaves
; it empty. Source template/plan pools stay immutable throughout this call.
exportClosed	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l ClosedBusy
	bne.w busy
	move.l #1, ClosedBusy
	lea ClosedScratch, a6
	move.l a0, ClosedWork.Caller(a6)
	movea.l ExportFrame.State(a0), a5
	move.l a5, ClosedWork.State(a6)
	move.l a5, d0
	beq.w failed
	tst.w State.Open(a5)
	bne.w failed
	tst.w State.ExternalOpen(a5)
	bne.w failed
	tst.w State.Skipping(a5)
	bne.w failed
	tst.w State.Depth(a5)
	bne.w failed
	movea.l ClosedWork.Caller(a6), a0
	movea.l ExportFrame.Destination(a0), a1
	move.l a1, d0
	beq.w failed
	move.l memory.Block.Pointer(a1), d0
	or.l memory.Block.Capacity(a1), d0
	or.l memory.Block.Used(a1), d0
	bne.w failed
	movea.l ClosedWork.Caller(a6), a0
	move.l ExportFrame.Handle(a0), d0
	beq.w failed
	cmpi.l #65535, d0
	bhi.w failed
	move.l ExportFrame.External(a0), d1
	cmpi.l #1, d1
	bhi.w failed
	tst.l d1
	bne.w external
	cmp.w State.Count(a5), d0
	bhi.w failed
	movea.l DEFS+memory.Block.Pointer(a5), a4
	bra.w descriptor
external
	cmp.w State.ExternalCount(a5), d0
	bhi.w failed
	movea.l EXTERNALS+memory.Block.Pointer(a5), a4
descriptor
	subq.l #1, d0
	mulu.w #DEF_BYTES, d0
	adda.l d0, a4
	tst.l Def.Flags(a4)
	bne.w failed
	cmpi.w #PARAM_LIMIT, Def.ParamCount(a4)
	bhi.w failed
	cmpi.w #KIND_EXTERNAL, Def.Kind(a4)
	bhi.w failed
	movea.l ClosedWork.Caller(a6), a0
	tst.l ExportFrame.External(a0)
	beq.w internalKind
	cmpi.w #KIND_EXTERNAL, Def.Kind(a4)
	bne.w failed
	bra.w kindReady
internalKind
	cmpi.w #KIND_MACRO, Def.Kind(a4)
	bhi.w failed
kindReady
	movea.l a4, a0
	lea ClosedWork.Definition(a6), a1
	move.l #DEF_BYTES, d0
	bsr.w closedCopy
	movea.l State.Plans(a5), a0
	move.l a0, ClosedWork.SourcePlans(a6)
	lea ClosedWork.Plans(a6), a0
	move.l a0, ClosedWork.TargetPlans(a6)
	move.l Def.First(a4), d0
	move.l Def.Last(a4), d1
	lea BODY(a5), a0
	lea ClosedWork.Body(a6), a1
	bsr.w closedAppendSpan
	bne.w failed
	move.l d2, ClosedWork.Definition+Def.First(a6)
	move.l d3, ClosedWork.Definition+Def.Last(a6)
	moveq #0, d7
parameter
	cmp.w ClosedWork.Definition+Def.ParamCount(a6), d7
	bhs.w plansReady
	move.l d7, d4
	lsl.w #2, d4
	move.l Def.DefaultStart0(a4, d4.w), d0
	move.l Def.DefaultEnd0(a4, d4.w), d1
	lea DEFAULTS(a5), a0
	lea ClosedWork.Defaults(a6), a1
	bsr.w closedAppendSpan
	bne.w failed
	lea ClosedWork.Definition(a6), a0
	move.l d2, Def.DefaultStart0(a0, d4.w)
	move.l d3, Def.DefaultEnd0(a0, d4.w)
	move.l Def.TextStart0(a4, d4.w), d0
	lea Def.TextEnd0(a4), a0
	move.l 0(a0, d4.w), d1
	lea DEFAULT_TEXT(a5), a0
	lea ClosedWork.Text(a6), a1
	bsr.w closedAppendSpan
	bne.w failed
	lea ClosedWork.Definition(a6), a0
	move.l d2, Def.TextStart0(a0, d4.w)
	lea Def.TextEnd0(a0), a1
	move.l d3, 0(a1, d4.w)
	addq.w #1, d7
	bra.w parameter
plansReady
	move.l ClosedWork.Definition+Def.HeaderPlan(a6), d1
	bsr.w closedPlan
	bne.w failed
	move.l d1, ClosedWork.Definition+Def.HeaderPlan(a6)
	bsr.w closedBodyPlans
	bne.w failed
	bsr.w closedBuild
	bne.w failed
	movea.l ClosedWork.Caller(a6), a0
	movea.l ExportFrame.Destination(a0), a1
	lea ClosedWork.Image(a6), a0
	move.l memory.Block.Pointer(a0), memory.Block.Pointer(a1)
	move.l memory.Block.Capacity(a0), memory.Block.Capacity(a1)
	move.l memory.Block.Used(a0), memory.Block.Used(a1)
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	moveq #STATUS_OK, d7
	bra.w finish
failed
	moveq #STATUS_BAD, d7
finish
	bsr.w closedCleanup
	clr.l ClosedBusy
	move.l d7, d0
	bra.w done
busy
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; exportClosed

; A0=SeedFrame. Capsule numeric IDs must already belong to the destination.
; Caller publishes independent public facet/module metadata before this call.
; Normal index/argument/cursor machinery owns the installed body. D0=status,
; D1=one-based internal/external handle; other registers preserved. Failure may
; leave unavailable facets and appended pools: discard the importing session.
seedClosed	.block
	movem.l d2-d7/a0-a6, -(sp)
	tst.l ClosedBusy
	bne.w busy
	move.l #1, ClosedBusy
	lea ClosedScratch, a6
	move.l a0, ClosedWork.Caller(a6)
	movea.l SeedFrame.State(a0), a5
	move.l a5, ClosedWork.State(a6)
	move.l a5, d0
	beq.w failed
	tst.l SeedFrame.Scope(a0)
	beq.w failed
	movea.l ClosedWork.Caller(a6), a0
	movea.l SeedFrame.Image(a0), a4
	movea.l a4, a0
	movea.l ClosedWork.Caller(a6), a1
	move.l SeedFrame.Bytes(a1), d0
	bsr.w validateClosed
	bne.w failed
	movea.l a4, a0
	adda.l ClosedHeader.Definition(a4), a0
	lea ClosedWork.Definition(a6), a1
	move.l #DEF_BYTES, d0
	bsr.w closedCopy
	movea.l ClosedWork.Caller(a6), a0
	move.l SeedFrame.Name(a0), d0
	cmpi.l #65535, d0
	bhi.w failed
	move.w d0, ClosedWork.Definition+Def.Name(a6)
	moveq #0, d1
	move.w ClosedWork.Definition+Def.Kind(a6), d1
	movea.l SeedFrame.Scope(a0), a1
	movea.l a5, a0
	moveq #0, d1
	move.w ClosedWork.Definition+Def.Kind(a6), d1
	bsr.w seedUnavailable
	bne.w failed
	move.l d1, ClosedWork.Handle(a6)
	; A checked borrowed plan arena, never released by closedCleanup.
	movea.l a4, a0
	adda.l ClosedHeader.Plans(a4), a0
	move.l a0, ClosedWork.View+memory.Block.Pointer(a6)
	move.l ClosedHeader.PlanBytes(a4), ClosedWork.View+memory.Block.Used(a6)
	move.l ClosedHeader.PlanBytes(a4), ClosedWork.View+memory.Block.Capacity(a6)
	lea ClosedWork.View(a6), a0
	move.l a0, ClosedWork.SourcePlans(a6)
	move.l State.Plans(a5), ClosedWork.TargetPlans(a6)
	move.l ClosedWork.Definition+Def.HeaderPlan(a6), d1
	bsr.w closedPlan
	bne.w failed
	move.l d1, ClosedWork.Definition+Def.HeaderPlan(a6)
	movea.l a4, a0
	adda.l ClosedHeader.Body(a4), a0
	move.l a0, ClosedWork.View+memory.Block.Pointer(a6)
	move.l ClosedHeader.BodyBytes(a4), ClosedWork.View+memory.Block.Used(a6)
	lea ClosedWork.View(a6), a0
	lea ClosedWork.Body(a6), a1
	moveq #0, d0
	move.l ClosedHeader.BodyBytes(a4), d1
	bsr.w closedAppendSpan
	bne.w failed
	; Restore the plan view after copying the body through the same view slot.
	movea.l a4, a0
	adda.l ClosedHeader.Plans(a4), a0
	move.l a0, ClosedWork.View+memory.Block.Pointer(a6)
	move.l ClosedHeader.PlanBytes(a4), ClosedWork.View+memory.Block.Used(a6)
	move.l ClosedHeader.PlanBytes(a4), ClosedWork.View+memory.Block.Capacity(a6)
	bsr.w closedBodyPlans
	bne.w failed
	movea.l a4, a0
	adda.l ClosedHeader.Defaults(a4), a0
	move.l a0, ClosedWork.View+memory.Block.Pointer(a6)
	move.l ClosedHeader.DefaultBytes(a4), ClosedWork.View+memory.Block.Used(a6)
	lea ClosedWork.View(a6), a0
	lea DEFAULTS(a5), a1
	moveq #0, d0
	move.l ClosedHeader.DefaultBytes(a4), d1
	bsr.w closedAppendSpan
	bne.w failed
	move.l d2, ClosedWork.DefaultBase(a6)
	movea.l a4, a0
	adda.l ClosedHeader.Text(a4), a0
	move.l a0, ClosedWork.View+memory.Block.Pointer(a6)
	move.l ClosedHeader.TextBytes(a4), ClosedWork.View+memory.Block.Used(a6)
	lea ClosedWork.View(a6), a0
	lea DEFAULT_TEXT(a5), a1
	moveq #0, d0
	move.l ClosedHeader.TextBytes(a4), d1
	bsr.w closedAppendSpan
	bne.w failed
	move.l d2, ClosedWork.TextBase(a6)
	lea ClosedWork.Body(a6), a0
	lea BODY(a5), a1
	moveq #0, d0
	move.l ClosedHeader.BodyBytes(a4), d1
	bsr.w closedAppendSpan
	bne.w failed
	move.l d2, ClosedWork.Definition+Def.First(a6)
	move.l d3, ClosedWork.Definition+Def.Last(a6)
	move.l d3, State.Used(a5)
	lea ClosedWork.Definition(a6), a4
	moveq #0, d7
parameter
	cmp.w Def.ParamCount(a4), d7
	bhs.w descriptor
	move.l d7, d0
	lsl.w #2, d0
	move.l ClosedWork.DefaultBase(a6), d1
	add.l d1, Def.DefaultStart0(a4, d0.w)
	add.l d1, Def.DefaultEnd0(a4, d0.w)
	move.l ClosedWork.TextBase(a6), d1
	add.l d1, Def.TextStart0(a4, d0.w)
	lea Def.TextEnd0(a4), a0
	add.l d1, 0(a0, d0.w)
	addq.w #1, d7
	bra.w parameter
descriptor
	move.l ClosedWork.Handle(a6), d0
	subq.l #1, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a5), a1
	cmpi.w #KIND_EXTERNAL, Def.Kind(a4)
	bne.w copy
	movea.l EXTERNALS+memory.Block.Pointer(a5), a1
copy
	adda.l d0, a1
	movea.l a4, a0
	move.l #DEF_BYTES, d0
	bsr.w closedCopy
	moveq #STATUS_OK, d7
	bra.w finish
failed
	clr.l ClosedWork.Handle(a6)
	moveq #STATUS_BAD, d7
finish
	move.l ClosedWork.Handle(a6), d1
	bsr.w closedCleanup
	clr.l ClosedBusy
	move.l d7, d0
	bra.w done
busy
	moveq #0, d1
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seedClosed
	.priv

; A0=source Block,D0=start,D1=end,A1=destination Block.
; D0=status,D2/D3=appended start/end; other registers preserved.
closedAppendSpan	.block
	movem.l d4-d7/a0-a6, -(sp)
	movea.l a0, a4
	movea.l a1, a5
	move.l d0, d4
	move.l d1, d5
	sub.l d4, d5
	bcs.w bad
	cmp.l memory.Block.Used(a4), d1
	bhi.w bad
	move.l memory.Block.Used(a5), d2
	move.l d2, d3
	add.l d5, d3
	bcs.w bad
	tst.l d5
	beq.w good
	move.l memory.Block.Pointer(a4), d0
	beq.w bad
	movea.l a5, a0
	move.l d3, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a4), a0
	adda.l d4, a0
	movea.l memory.Block.Pointer(a5), a1
	adda.l d2, a1
	move.l d5, d0
	bsr.w closedCopy
	move.l d3, memory.Block.Used(a5)
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; closedAppendSpan

; A6=work,D1=old plan handle. Selected roots are copied once per operation.
; D0=status,D1=new handle; other registers preserved.
closedPlan	.block
	movem.l d2-d7/a0-a5, -(sp)
	tst.l d1
	beq.w good
	move.l d1, d6
	movea.l ClosedWork.Map+memory.Block.Pointer(a6), a0
	move.l ClosedWork.Map+memory.Block.Used(a6), d7
find
	tst.l d7
	beq.w append
	cmp.l ClosedMap.Source(a0), d6
	beq.w found
	adda.w #CLOSED_MAP_BYTES, a0
	subi.l #CLOSED_MAP_BYTES, d7
	bra.w find
found
	move.l ClosedMap.Target(a0), d1
	bra.w good
append
	movea.l ClosedWork.SourcePlans(a6), a0
	move.l a0, d0
	beq.w bad
	movea.l ClosedWork.TargetPlans(a6), a1
	move.l a1, d0
	beq.w bad
	jsr plans.clone
	bne.w bad
	move.l d1, d7
	lea ClosedWork.Map(a6), a0
	move.l memory.Block.Used(a0), d0
	addi.l #CLOSED_MAP_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d6, ClosedMap.Source(a1)
	move.l d7, ClosedMap.Target(a1)
	addi.l #CLOSED_MAP_BYTES, memory.Block.Used(a0)
	move.l d7, d1
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d2-d7/a0-a5
	tst.l d0
	rts
	.bend  ; closedPlan

; A6=work. Body records own at most one checked terminal plan handle.
closedBodyPlans	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l ClosedWork.Body+memory.Block.Pointer(a6), a4
	movea.l a4, a5
	adda.l ClosedWork.Body+memory.Block.Used(a6), a5
next
	cmpa.l a5, a4
	beq.w good
	bhi.w bad
	move.l a5, d0
	sub.l a4, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w bad
	moveq #0, d7
	move.b source.RecordHeader.Length(a4), d7
	addq.l #1, d7
	cmp.l d0, d7
	bhi.w bad
	movea.l a4, a0
	move.l d7, d0
	bsr.w validateBodyPlan
	bne.w bad
	moveq #0, d0
	move.b source.RecordHeader.Flags(a4), d0
	andi.w #source.FLAG_PLAN+LINE_PLAN_FLAG, d0
	beq.w advance
	movea.l a4, a3
	adda.l d7, a3
	suba.w #source.PLAN_TOKEN_BYTES, a3
	move.l source.PlanToken.Handle(a3), d1
	bsr.w closedPlan
	bne.w bad
	move.l d1, source.PlanToken.Handle(a3)
advance
	adda.l d7, a4
	bra.w next
good
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a5
	tst.l d0
	rts
	.bend  ; closedBodyPlans

; A0/A1=disjoint readable/writable bytes,D0=count. All registers preserved.
closedCopy	.block
	movem.l d0/a0-a1, -(sp)
next
	tst.l d0
	beq.w done
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra.w next
done
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; closedCopy

; Release only the six owned temporary blocks, never the borrowed View.
closedCleanup	.block
	movem.l d0-d1/a0, -(sp)
	lea ClosedWork.Body(a6), a0
	moveq #6-1, d1
release
	jsr memory.release
	adda.w #BLOCK_BYTES, a0
	dbra d1, release
	movea.l a6, a0
	move.l #CLOSED_WORK_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; closedCleanup

	.include "template_closed_image.i"
	.include "template_remap.i"
