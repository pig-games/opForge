; Owned declaration-time packed maps; normal descriptors are copied on first use.
; Recipes contain numeric capture offsets and handles, never borrowed pointers.
; @opforge-owner: experimental.amigaos.binary_deferred_plans
	.module experimental.amigaos.binary_deferred_plans
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_macro_plans as plans
	.use experimental.amigaos.binary_capture as capture
	.include "build_configuration.i"
	.include "deferred_plan_telemetry.i"
	.include "lazy_work.i"
	.pub
TAG = $80000000
HANDLE_MASK = $7fffffff
ARENA_LIMIT = $7ffffffe
.if ACQUIRE_MACRO_CANDIDATES
ACQUIRE_CALL = 1
ACQUIRE_TOKEN_LIMIT = 64
.endif
.if DEFER_MACRO_LINES
ACQUIRE_LINE = 2
.endif
State	.struct
Block	.res memory.Block.Used+4
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
Created	.long ?
Bound	.long ?
Hits	.long ?
StoredBytes	.long ?
.ifdef OPFORGE_DEFERRED_MACRO_CANDIDATES
AcquiredCreated	.long ?
AcquiredBound	.long ?
.endif
.ifdef OPFORGE_DEFERRED_MACRO_LINES
LinesCreated	.long ?
LinesBound	.long ?
.endif
.endif
.endif
.endif
.endstruct
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
.if DEFER_MACRO_LINES
STATE_BYTES = State.LinesBound+4
.else
.if ACQUIRE_MACRO_CANDIDATES
STATE_BYTES = State.AcquiredBound+4
.else
STATE_BYTES = State.StoredBytes+4
.endif
.endif
.else
STATE_BYTES = memory.Block.Used+4
.endif
.else
STATE_BYTES = memory.Block.Used+4
.endif
.else
STATE_BYTES = memory.Block.Used+4
.endif
Entry	.struct
Capture	.long ?
Plan	.long ?
Count	.long ?
Recipes	.word ?
Reserved	.word ?
Cache	.long ?
Map	.word ?
.endstruct
ENTRY_BYTES = Entry.Map
CreateFrame	.struct
Arena	.long ?
Source	.long ?
Capture	.long ?
Handle	.long ?
PackedMap	.long ?
TokenCount	.long ?
IncludeRecipes	.word ?
Reserved	.word ?
.endstruct
CREATE_FRAME_BYTES = CreateFrame.Reserved+2
RealizeFrame	.struct
Arena	.long ?
Plans	.long ?
Handle	.long ?
Context	.long ?
Resolve	.long ?
Acquire	.res ACQUIRE_MACRO_CANDIDATES*4
AcquireLine	.res DEFER_MACRO_LINES*4
.endstruct
.if DEFER_MACRO_LINES
REALIZE_FRAME_BYTES = RealizeFrame.AcquireLine+4
.else
.if ACQUIRE_MACRO_CANDIDATES
REALIZE_FRAME_BYTES = RealizeFrame.Acquire+4
.else
REALIZE_FRAME_BYTES = RealizeFrame.Resolve+4
.endif
.endif
	.section code, kind=code
; A0=zero-initialized State or existing owner. Release/reset all recipes.
; D0/CCR=zero; other registers preserved.
begin	.block
	jsr memory.release
	clr.l memory.Block.Used(a0)
	.DEFERRED_CLEAR a0
	moveq #0, d0
	rts
	.bend  ; begin
finish	.block
	bra.w begin
	.bend  ; finish

.if ACQUIRE_MACRO_CANDIDATES
; A0=CreateFrame with caller-validated declaration map of TokenCount+1 words.
; Handle=0, IncludeRecipes=1, TokenCount<=64. The caller bounds the map.
; Save numeric acquisition inputs only; source is unused. D0/CCR=status,
; D1=tagged handle or zero; all other registers preserved.
createAcquired	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	tst.l CreateFrame.Handle(a6)
	bne.w create.bad
	cmpi.w #1, CreateFrame.IncludeRecipes(a6)
	bne.w create.bad
	move.l CreateFrame.TokenCount(a6), d7
	cmpi.l #ACQUIRE_TOKEN_LIMIT, d7
	bhi.w create.bad
	moveq #ACQUIRE_CALL, d4
	bra.w create.store
	.bend  ; createAcquired
.endif

.if DEFER_MACRO_LINES
; A0=CreateFrame with caller-validated declaration map of TokenCount+1 words.
; Handle=positive owned whole-line proof handle, IncludeRecipes=1,
; TokenCount<=64, Reserved=0. Capture owns the proof named by Handle.
; Save numeric proof/capture inputs and map only. D0/CCR=status,
; D1=tagged handle or zero; all other registers preserved.
createLine	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	tst.l CreateFrame.Handle(a6)
	ble.w create.bad
	tst.w CreateFrame.Reserved(a6)
	bne.w create.bad
	cmpi.w #1, CreateFrame.IncludeRecipes(a6)
	bne.w create.bad
	move.l CreateFrame.TokenCount(a6), d7
	cmpi.l #ACQUIRE_TOKEN_LIMIT, d7
	bhi.w create.bad
	moveq #ACQUIRE_LINE, d4
	bra.w create.store
	.bend  ; createLine
.endif

; A0=CreateFrame. Validate selected source spans and save its declaration map.
; D0/CCR=status,D1=tagged handle or zero. Other registers preserved.
; Borrowed source/map/frame storage must not overlap the destination owner.
create	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l CreateFrame.TokenCount(a6), d7
	cmpi.l #capture.TOKEN_LIMIT, d7
	bhi.w bad
	suba.w #plans.BIND_FRAME_BYTES, sp
	movea.l sp, a0
	clr.l plans.BindFrame.Arena(a0)
	move.l CreateFrame.Source(a6), plans.BindFrame.Source(a0)
	move.l CreateFrame.Handle(a6), plans.BindFrame.Handle(a0)
	move.l CreateFrame.PackedMap(a6), plans.BindFrame.PackedMap(a0)
	move.l d7, plans.BindFrame.TokenCount(a0)
	move.w CreateFrame.IncludeRecipes(a6), plans.BindFrame.IncludeRecipes(a0)
	clr.w plans.BindFrame.Reserved(a0)
	jsr plans.validateBind
	adda.w #plans.BIND_FRAME_BYTES, sp
	tst.l d0
	bne.w bad
.if ACQUIRE_MACRO_CANDIDATES
	moveq #0, d4
store
.endif
	movea.l CreateFrame.Arena(a6), a4
	move.l memory.Block.Used(a4), d5
	move.l d7, d6
	addq.l #1, d6
	add.l d6, d6
	addi.l #ENTRY_BYTES, d6
	move.l d5, d0
	add.l d6, d0
	bcs.w bad
	move.l d0, d6
	movea.l a4, a0
	move.l #ARENA_LIMIT, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l memory.Block.Pointer(a4), a3
	adda.l d5, a3
	move.l CreateFrame.Capture(a6), Entry.Capture(a3)
	move.l CreateFrame.Handle(a6), Entry.Plan(a3)
	move.l d7, Entry.Count(a3)
	move.w CreateFrame.IncludeRecipes(a6), Entry.Recipes(a3)
.if ACQUIRE_MACRO_CANDIDATES
	move.w d4, Entry.Reserved(a3)
.else
	clr.w Entry.Reserved(a3)
.endif
	clr.l Entry.Cache(a3)
	lea Entry.Map(a3), a1
	movea.l CreateFrame.PackedMap(a6), a0
copyMap
	move.w (a0)+, (a1)+
	dbra d7, copyMap
	move.l d6, memory.Block.Used(a4)
	.DEFERRED_COUNTER a4, State.Created, #1
	.DEFERRED_ACQUISITION_COUNTER a4, State.AcquiredCreated, d4
	.DEFERRED_LINE_COUNTER a4, State.LinesCreated, d4
	sub.l d5, d6
	.DEFERRED_COUNTER a4, State.StoredBytes, d6
	move.l d5, d1
	addq.l #1, d1
	ori.l #TAG, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; create

; A0=RealizeFrame. Resolve callback: A0=Context,D1=global capture offset;
; D0=status,A1=borrowed validated capture.Record; all other registers preserved.
; Acquire callback (experimental): A0=Context,A1=borrowed Entry.Map,
; D1=global capture offset,D2=TokenCount; D0=status,D1=normal plan handle.
; It preserves every other register, owns the destination session plan arena,
; and must not mutate the deferred arena. The map contains Count+1 u16 words.
; AcquireLine additionally receives D3=Entry.Plan, the expected compact proof
; handle owned by the capture; it validates that identity and preserves D3.
; Cache only a successfully copied normal plan. D0/CCR=status,D1=normal handle;
; others preserved. Borrowed views never survive this call or arena mutation.
realize	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l RealizeFrame.Arena(a6), a4
	move.l RealizeFrame.Handle(a6), d5
	btst #31, d5
	beq.w bad
	andi.l #HANDLE_MASK, d5
	beq.w bad
	subq.l #1, d5
	btst #0, d5
	bne.w bad
	move.l memory.Block.Used(a4), d2
	sub.l d5, d2
	bcs.w bad
	cmpi.l #ENTRY_BYTES+2, d2
	blo.w bad
	movea.l memory.Block.Pointer(a4), a3
	adda.l d5, a3
	move.l Entry.Count(a3), d7
	cmpi.l #capture.TOKEN_LIMIT, d7
	bhi.w bad
	move.l d7, d0
	addq.l #1, d0
	add.l d0, d0
	addi.l #ENTRY_BYTES, d0
	cmp.l d2, d0
	bhi.w bad
.if ACQUIRE_MACRO_CANDIDATES
	move.w Entry.Reserved(a3), d0
	beq.w validTag
.if DEFER_MACRO_LINES
	cmpi.w #ACQUIRE_LINE, d0
	beq.w validLine
.endif
	cmpi.w #ACQUIRE_CALL, d0
	bne.w bad
	cmpi.l #ACQUIRE_TOKEN_LIMIT, d7
	bhi.w bad
	tst.l Entry.Plan(a3)
	bne.w bad
	cmpi.w #1, Entry.Recipes(a3)
	bne.w bad
.if DEFER_MACRO_LINES
	bra.w validTag
validLine
	cmpi.l #ACQUIRE_TOKEN_LIMIT, d7
	bhi.w bad
	tst.l Entry.Plan(a3)
	ble.w bad
	cmpi.w #1, Entry.Recipes(a3)
	bne.w bad
.endif
validTag
.endif
	move.l Entry.Cache(a3), d1
	beq.w uncached
.if ACQUIRE_MACRO_CANDIDATES
	tst.l d1
	bmi.w bad
.endif
	.DEFERRED_COUNTER a4, State.Hits, #1
	moveq #0, d0
	bra.w done
uncached
	movea.l RealizeFrame.Plans(a6), a2
	cmpa.l a4, a2
	beq.w bad
	move.l memory.Block.Pointer(a4), d0
	beq.w distinctOwners
	cmp.l memory.Block.Pointer(a2), d0
	beq.w bad
distinctOwners
.if ACQUIRE_MACRO_CANDIDATES
	cmpi.w #ACQUIRE_CALL, Entry.Reserved(a3)
	beq.w acquire
.if DEFER_MACRO_LINES
	cmpi.w #ACQUIRE_LINE, Entry.Reserved(a3)
	beq.w acquireLine
.endif
.endif
	move.l RealizeFrame.Resolve(a6), d0
	beq.w bad
	movea.l d0, a5
	move.l Entry.Capture(a3), d1
	movea.l RealizeFrame.Context(a6), a0
	jsr (a5)
	tst.l d0
	bne.w bad
	jsr capture.planView
	; Stack-local block borrows the immutable capture until bind completes.
	suba.w #memory.Block.Used+4+plans.BIND_FRAME_BYTES, sp
	move.l a0, memory.Block.Pointer(sp)
	move.l d0, memory.Block.Capacity(sp)
	move.l d0, memory.Block.Used(sp)
	lea memory.Block.Used+4(sp), a0
	move.l a2, plans.BindFrame.Arena(a0)
	move.l sp, plans.BindFrame.Source(a0)
	move.l Entry.Plan(a3), plans.BindFrame.Handle(a0)
	lea Entry.Map(a3), a1
	move.l a1, plans.BindFrame.PackedMap(a0)
	move.l d7, plans.BindFrame.TokenCount(a0)
	move.w Entry.Recipes(a3), plans.BindFrame.IncludeRecipes(a0)
	clr.w plans.BindFrame.Reserved(a0)
	.WORK_COUNT lazy_work.PLAN_BIND, #1
	.WORK_BOUND_BEGIN memory.Block.Used(a2)
	jsr plans.bind
	.WORK_BOUND_END memory.Block.Used(a2)
	adda.w #memory.Block.Used+4+plans.BIND_FRAME_BYTES, sp
	tst.l d0
	bne.w bad
.if ACQUIRE_MACRO_CANDIDATES
publish
	tst.l d1
	beq.w bad
.endif
	tst.l d1
	bmi.w bad
	movea.l memory.Block.Pointer(a4), a3
	adda.l d5, a3
	move.l d1, Entry.Cache(a3)
	.DEFERRED_COUNTER a4, State.Bound, #1
	.DEFERRED_LINE_COUNTER a4, State.LinesBound, Entry.Reserved(a3)
	moveq #0, d0
	bra.w done
.if ACQUIRE_MACRO_CANDIDATES
.if DEFER_MACRO_LINES
acquireLine
	move.l Entry.Plan(a3), d3
	move.l RealizeFrame.AcquireLine(a6), d0
	bra.w acquireCallback
.endif
acquire
	move.l RealizeFrame.Acquire(a6), d0
.if DEFER_MACRO_LINES
acquireCallback
.endif
	beq.w bad
	movea.l d0, a5
	movea.l RealizeFrame.Context(a6), a0
	lea Entry.Map(a3), a1
	move.l Entry.Capture(a3), d1
	move.l d7, d2
	jsr (a5)
	tst.l d0
	bne.w bad
.if DEFER_MACRO_LINES
	.DEFERRED_ACQUISITION_COUNTER a4, State.AcquiredBound, Entry.Reserved(a3)
.else
	.DEFERRED_COUNTER a4, State.AcquiredBound, #1
.endif
	bra.w publish
.endif
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; realize
; A0=State. D1-D4=created/bound/cache hits/stored recipe bytes.
; Other registers preserved; CCR unspecified. Ordinary builds return zeros.
getStats	.block
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
	moveq #0, d4
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.l State.Created(a0), d1
	move.l State.Bound(a0), d2
	move.l State.Hits(a0), d3
	move.l State.StoredBytes(a0), d4
.endif
.endif
.endif
	rts
	.bend  ; getStats
.if ACQUIRE_MACRO_CANDIDATES
; A0=State. D1/D2=acquisition recipes created/successfully realized.
; Other registers preserved; CCR unspecified. Ordinary builds return zeros.
getAcquisitionStats	.block
	moveq #0, d1
	moveq #0, d2
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.l State.AcquiredCreated(a0), d1
	move.l State.AcquiredBound(a0), d2
.endif
.endif
.endif
	rts
	.bend  ; getAcquisitionStats
.endif
.if DEFER_MACRO_LINES
; A0=State. D1/D2=whole-line recipes created/successfully realized.
; Other registers preserved; CCR unspecified. Ordinary builds return zeros.
getLineStats	.block
	moveq #0, d1
	moveq #0, d2
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.l State.LinesCreated(a0), d1
	move.l State.LinesBound(a0), d2
.endif
.endif
.endif
	rts
	.bend  ; getLineStats
.endif
	.endsection
	.endmodule
