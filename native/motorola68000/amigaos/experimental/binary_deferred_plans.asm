; Owned declaration-time packed maps; normal descriptors are copied on first use.
; Recipes contain numeric capture offsets and handles, never borrowed pointers.
; @opforge-owner: experimental.amigaos.binary_deferred_plans
	.module experimental.amigaos.binary_deferred_plans
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_macro_plans as plans
	.use experimental.amigaos.binary_capture as capture
	.include "deferred_plan_telemetry.i"
	.include "lazy_work.i"
	.pub
TAG = $80000000
HANDLE_MASK = $7fffffff
ARENA_LIMIT = $7ffffffe
State	.struct
Block	.res memory.Block.Used+4
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
Created	.long ?
Bound	.long ?
Hits	.long ?
StoredBytes	.long ?
.endif
.endif
.endif
.endstruct
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
STATE_BYTES = State.StoredBytes+4
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
.endstruct
REALIZE_FRAME_BYTES = RealizeFrame.Resolve+4
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
	clr.w Entry.Reserved(a3)
	clr.l Entry.Cache(a3)
	lea Entry.Map(a3), a1
	movea.l CreateFrame.PackedMap(a6), a0
copyMap
	move.w (a0)+, (a1)+
	dbra d7, copyMap
	move.l d6, memory.Block.Used(a4)
	.DEFERRED_COUNTER a4, State.Created, #1
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
	move.l Entry.Cache(a3), d1
	beq.w uncached
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
	tst.l d1
	bmi.w bad
	movea.l memory.Block.Pointer(a4), a3
	adda.l d5, a3
	move.l d1, Entry.Cache(a3)
	.DEFERRED_COUNTER a4, State.Bound, #1
	moveq #0, d0
	bra.w done
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
	.endsection
	.endmodule
