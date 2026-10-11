; Transactional scalar symbol views. Typed descriptors remain in binary_values.
; @opforge-owner: experimental.amigaos.binary_symbol_storage
	.module experimental.amigaos.binary_symbol_storage
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_values as values
	.pub
MAX_SYMBOLS = 65536
VALUE_BYTES = 8
MEMBER_BYTES = 2
State	.struct
Values	.res memory.Block.Used+4
Defined	.res memory.Block.Used+4
Sections	.res memory.Block.Used+4
MemberKeys	.res memory.Block.Used+4
Count	.long ?
Reserve	.long ?  ; optional A0=empty Block,D0=bytes; status, other registers kept
.endstruct
STATE_BYTES = State.Reserve+4
	.priv
STAGES = 5
Stage	.struct
Destination	.long ?
Bytes	.long ?
Replacement	.res memory.Block.Used+4
.endstruct
STAGE_BYTES = Stage.Replacement+memory.Block.Used+4
WORK_BYTES = STAGES*STAGE_BYTES
	.section code, kind=code
	.pub
; A0=State,A1=Context,D0=new Count. Other registers kept; D0/CCR=status.
; Context.Owner is a live typed owner. Reserve all replacement storage before
; publishing any view/count. Failure preserves cells, kinds, member keys and
; pointers. Existing preparation-exported member keys/kinds may precede the
; first scalar allocation; their Used extents are preserved.
grow	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #WORK_BYTES, sp
	movea.l a0, a5
	movea.l a1, a6
	move.l d0, d7
	movea.l sp, a0
	moveq #WORK_BYTES/4-1, d1
clearStages
	clr.l (a0)+
	dbra d1, clearStages
	cmpi.l #MAX_SYMBOLS, d7
	bhi.w bad
	cmp.l State.Count(a5), d7
	blo.w bad
	move.l package.Context.Count(a6), d0
	cmp.l State.Count(a5), d0
	bne.w bad
	move.l State.Count(a5), d0
	beq.w freshViews
	move.l package.Context.Values(a6), d1
	cmp.l State.Values+memory.Block.Pointer(a5), d1
	bne.w bad
	lsl.l #3, d0
	cmp.l State.Values+memory.Block.Used(a5), d0
	bne.w bad
	move.l package.Context.Defined(a6), d1
	cmp.l State.Defined+memory.Block.Pointer(a5), d1
	bne.w bad
	move.l State.Count(a5), d0
	cmp.l State.Defined+memory.Block.Used(a5), d0
	bne.w bad
	move.l package.Context.SectionIds(a6), d1
	cmp.l State.Sections+memory.Block.Pointer(a5), d1
	bne.w bad
	cmp.l State.Sections+memory.Block.Used(a5), d0
	bne.w bad
freshViews
	movea.l package.Context.Owner(a6), a4
	move.l a4, d0
	beq.w bad
	move.l values.Owner.Count(a4), d0
	cmp.l State.Count(a5), d0
	beq.w typedReady
	tst.l State.Count(a5)
	bne.w bad
	cmp.l d7, d0
	bne.w bad
typedReady
	movea.l sp, a3
	lea State.Values(a5), a0
	move.l d7, d0
	lsl.l #3, d0
	bsr.w describe
	lea State.Defined(a5), a0
	move.l d7, d0
	bsr.w describe
	lea State.Sections(a5), a0
	move.l d7, d0
	bsr.w describe
	lea State.MemberKeys(a5), a0
	move.l d7, d0
	add.l d0, d0
	bsr.w describe
	lea values.Owner.Kinds(a4), a0
	move.l d7, d0
	tst.l memory.Block.Pointer(a0)
	bne.w kindsAllocated
	moveq #0, d0  ; absent kinds denote scalar; retain lazy typed allocation
kindsAllocated
	bsr.w describe
	movea.l sp, a3
	moveq #STAGES-1, d6
reserveStage
	movea.l Stage.Destination(a3), a2
	move.l memory.Block.Used(a2), d0
	cmp.l memory.Block.Capacity(a2), d0
	bhi.w bad
	cmp.l Stage.Bytes(a3), d0
	bhi.w bad
	move.l memory.Block.Pointer(a2), d1
	tst.l memory.Block.Capacity(a2)
	beq.w emptyBlock
	tst.l d1
	beq.w bad
	bra.w validBlock
emptyBlock
	tst.l d1
	bne.w bad
validBlock
	move.l Stage.Bytes(a3), d0
	cmp.l memory.Block.Capacity(a2), d0
	bls.w reserved
	lea Stage.Replacement(a3), a0
	move.l State.Reserve(a5), d1
	beq.w defaultReserve
	movea.l d1, a1
	jsr (a1)
	bra.w allocationChecked
defaultReserve
	jsr memory.reserve
allocationChecked
	tst.l d0
	bne.w bad
	move.l memory.Block.Pointer(a0), d1
	beq.w bad
	move.l memory.Block.Capacity(a0), d1
	cmp.l Stage.Bytes(a3), d1
	blo.w bad
	movea.l memory.Block.Pointer(a2), a0
	movea.l Stage.Replacement+memory.Block.Pointer(a3), a1
	move.l memory.Block.Used(a2), d0
	beq.w reserved
copy
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copy
reserved
	adda.w #STAGE_BYTES, a3
	dbra d6, reserveStage
	; No operation below can fail. Publish replacements and initialize only
	; newly appended bytes; old typed handles and member keys remain untouched.
	movea.l sp, a3
	moveq #STAGES-1, d6
commitStage
	movea.l Stage.Destination(a3), a2
	tst.l Stage.Replacement+memory.Block.Pointer(a3)
	beq.w retained
	lea Stage.Replacement(a3), a1
	moveq #memory.Block.Used/4, d5
	movea.l a2, a0
swap
	move.l (a0), d0
	move.l (a1), (a0)+
	move.l d0, (a1)+
	dbra d5, swap
	lea Stage.Replacement(a3), a0
	jsr memory.release
	; Replacement Used was zero; the old extent survives in the staging slot.
	move.l Stage.Replacement+memory.Block.Used(a3), memory.Block.Used(a2)
retained
	move.l Stage.Bytes(a3), d0
	sub.l memory.Block.Used(a2), d0
	movea.l memory.Block.Pointer(a2), a0
	adda.l memory.Block.Used(a2), a0
	beq.w initialized
clearTail
	clr.b (a0)+
	subq.l #1, d0
	bne.w clearTail
initialized
	move.l Stage.Bytes(a3), memory.Block.Used(a2)
	adda.w #STAGE_BYTES, a3
	dbra d6, commitStage
	move.l d7, values.Owner.Count(a4)
	move.l State.Values+memory.Block.Pointer(a5), package.Context.Values(a6)
	move.l State.Defined+memory.Block.Pointer(a5), package.Context.Defined(a6)
	move.l State.Sections+memory.Block.Pointer(a5), package.Context.SectionIds(a6)
	move.l d7, State.Count(a5)
	move.l d7, package.Context.Count(a6)
	moveq #0, d0
	bra.w done
bad
	movea.l sp, a3
	moveq #STAGES-1, d6
rollback
	lea Stage.Replacement(a3), a0
	jsr memory.release
	adda.w #STAGE_BYTES, a3
	dbra d6, rollback
	moveq #1, d0
done
	adda.w #WORK_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; grow
; A0=State. Caller releases its typed owner separately. Context views cease to
; be valid on release. Other registers preserved; D0/CCR=status.
release	.block
	movem.l a0, -(sp)
	jsr memory.release
	lea State.Defined(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea State.Sections(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea State.MemberKeys(a0), a0
	jsr memory.release
	movea.l (sp)+, a0
	clr.l State.Values+memory.Block.Used(a0)
	clr.l State.Defined+memory.Block.Used(a0)
	clr.l State.Sections+memory.Block.Used(a0)
	clr.l State.MemberKeys+memory.Block.Used(a0)
	clr.l State.Count(a0)
	moveq #0, d0
	rts
	.bend  ; release
	.priv
; A3=next staging slot,A0=destination Block,D0=required bytes.
describe	.block
	move.l a0, Stage.Destination(a3)
	move.l d0, Stage.Bytes(a3)
	adda.w #STAGE_BYTES, a3
	rts
	.bend  ; describe
	.endsection
	.endmodule
